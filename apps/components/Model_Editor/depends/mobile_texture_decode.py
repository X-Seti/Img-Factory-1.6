#this belongs in apps/methods/mobile_texture_decode.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6 - Mobile Texture Pixel Decoders
"""
Mobile texture pixel decoders for PVRTC and ETC1 formats.

ETC1  — full pure-Python decoder (4x4 block, 64-bit per block)
PVRTC — not decoded (complex proprietary algorithm); returns grey placeholder
RGB565/RGBA4444/RGBA5551/RGBA8888 — trivial unpack
"""

import struct
from typing import Tuple, Optional

## Methods list -
# decode_etc1_block
# decode_etc1
# decode_rgb565
# decode_rgba4444
# decode_rgba5551
# decode_rgba8888
# decode_mobile_texture
# to_pil_image


#    ETC1 decoder                                                               

# ETC1 modifier tables (per spec)
_ETC1_MODIFIER = [
    [2,   8],  [5,  17], [9,  29], [13,  42],
    [18, 60], [24,  80], [33, 106], [47, 183],
]

def _clamp(v: int) -> int:
    return max(0, min(255, v))

def decode_etc1_block(block: bytes) -> bytes:
    """Decode one 8-byte ETC1 block → 4×4 RGBA bytes (64 bytes)."""
    if len(block) < 8:
        return b'\xff\x00\xff\xff' * 16   # magenta placeholder

    # ETC1 block layout (MSB first in the 64-bit word):
    # Bytes 0-3: pixel data (indices + flip/diff bits)
    # Bytes 4-7: color data
    p0, p1, p2, p3 = block[0], block[1], block[2], block[3]
    c0, c1, c2, c3 = block[4], block[5], block[6], block[7]

    diff_bit  = (c3 >> 1) & 1
    flip_bit  = c3 & 1
    table_idx = [(c3 >> 5) & 0x7, (c3 >> 2) & 0x7]

    if diff_bit:
        # Differential mode: 5-bit base + 3-bit signed delta
        r1 = (c0 >> 3) & 0x1F
        g1 = (c1 >> 3) & 0x1F
        b1 = (c2 >> 3) & 0x1F
        dr = c0 & 0x7; dr = dr if dr < 4 else dr - 8
        dg = c1 & 0x7; dg = dg if dg < 4 else dg - 8
        db = c2 & 0x7; db = db if db < 4 else db - 8
        r2, g2, b2 = r1 + dr, g1 + dg, b1 + db
        # Expand 5→8 bits
        base = [
            (_clamp(r1 * 255 // 31), _clamp(g1 * 255 // 31), _clamp(b1 * 255 // 31)),
            (_clamp(r2 * 255 // 31), _clamp(g2 * 255 // 31), _clamp(b2 * 255 // 31)),
        ]
    else:
        # Individual mode: two 4-bit colours per sub-block
        r1 = (c0 >> 4) & 0xF;  g1 = (c1 >> 4) & 0xF;  b1 = (c2 >> 4) & 0xF
        r2 = c0 & 0xF;          g2 = c1 & 0xF;          b2 = c2 & 0xF
        # Expand 4→8 bits
        base = [
            (r1 * 17, g1 * 17, b1 * 17),
            (r2 * 17, g2 * 17, b2 * 17),
        ]

    # Build pixel LUT for each sub-block
    # index = 2-bit value from pixel bits; sign from msb pixel bit
    # Combine pixel index bits from p0..p3 (16 pixels × 2 bits)
    # Bit layout: msb plane in p0,p1; lsb plane in p2,p3 (column-major)
    out = bytearray(64)

    for px in range(16):
        col = px // 4    # 0-3
        row = px % 4     # 0-3

        if flip_bit:
            sub = 1 if row >= 2 else 0
        else:
            sub = 1 if col >= 2 else 0

        r, g, b = base[sub]
        tbl = _ETC1_MODIFIER[table_idx[sub]]

        # Extract 2-bit index for this pixel
        bit = (3 - col) * 4 + (3 - row)   # bit position in the 16-bit plane
        msb = (p0 >> (bit - 8) & 1) if bit >= 8 else (p1 >> bit & 1)
        lsb = (p2 >> (bit - 8) & 1) if bit >= 8 else (p3 >> bit & 1)

        # Safer bit extraction
        byte_msb = p0 if bit >= 8 else p1
        byte_lsb = p2 if bit >= 8 else p3
        b_pos = bit % 8 if bit < 8 else bit - 8
        msb = (byte_msb >> b_pos) & 1
        lsb = (byte_lsb >> b_pos) & 1

        idx = (msb << 1) | lsb
        mod = tbl[1] if idx >= 2 else tbl[0]
        sign = -1 if idx in (3, 0) else 1   # ETC1: 0→+small, 1→+large, 2→-large, 3→-small
        # Correct ETC1 modifier sign: 0=+mod[0], 1=+mod[1], 2=-mod[1], 3=-mod[0]
        if   idx == 0: mod =  tbl[0]
        elif idx == 1: mod =  tbl[1]
        elif idx == 2: mod = -tbl[1]
        else:          mod = -tbl[0]

        # Output pixel at (col, row) — RGBA
        out_idx = (row * 4 + col) * 4
        out[out_idx]   = _clamp(r + mod)
        out[out_idx+1] = _clamp(g + mod)
        out[out_idx+2] = _clamp(b + mod)
        out[out_idx+3] = 255

    return bytes(out)


def decode_etc1(data: bytes, width: int, height: int) -> bytes:
    """Decode full ETC1 image → raw RGBA bytes (width*height*4)."""
    blocks_x = max(1, (width  + 3) // 4)
    blocks_y = max(1, (height + 3) // 4)
    rgba = bytearray(width * height * 4)

    block_idx = 0
    for by in range(blocks_y):
        for bx in range(blocks_x):
            block_off = block_idx * 8
            if block_off + 8 > len(data):
                break
            block_rgba = decode_etc1_block(data[block_off:block_off+8])
            block_idx += 1

            # Write 4×4 block into output
            for py in range(4):
                for px in range(4):
                    ox = bx * 4 + px
                    oy = by * 4 + py
                    if ox >= width or oy >= height:
                        continue
                    src = (py * 4 + px) * 4
                    dst = (oy * width + ox) * 4
                    rgba[dst:dst+4] = block_rgba[src:src+4]

    return bytes(rgba)


#    Simple format decoders                                                      

def decode_rgb565(data: bytes, width: int, height: int) -> bytes:
    """RGB565 → RGBA8888."""
    out = bytearray(width * height * 4)
    for i in range(width * height):
        if i * 2 + 2 > len(data): break
        v = struct.unpack_from('<H', data, i * 2)[0]
        r = ((v >> 11) & 0x1F) * 255 // 31
        g = ((v >> 5)  & 0x3F) * 255 // 63
        b =  (v        & 0x1F) * 255 // 31
        out[i*4:i*4+4] = [r, g, b, 255]
    return bytes(out)


def decode_rgba4444(data: bytes, width: int, height: int) -> bytes:
    """RGBA4444 → RGBA8888."""
    out = bytearray(width * height * 4)
    for i in range(width * height):
        if i * 2 + 2 > len(data): break
        v = struct.unpack_from('<H', data, i * 2)[0]
        r = ((v >> 12) & 0xF) * 17
        g = ((v >> 8)  & 0xF) * 17
        b = ((v >> 4)  & 0xF) * 17
        a =  (v        & 0xF) * 17
        out[i*4:i*4+4] = [r, g, b, a]
    return bytes(out)


def decode_rgba5551(data: bytes, width: int, height: int) -> bytes:
    """RGBA5551 → RGBA8888."""
    out = bytearray(width * height * 4)
    for i in range(width * height):
        if i * 2 + 2 > len(data): break
        v = struct.unpack_from('<H', data, i * 2)[0]
        r = ((v >> 11) & 0x1F) * 255 // 31
        g = ((v >> 6)  & 0x1F) * 255 // 31
        b = ((v >> 1)  & 0x1F) * 255 // 31
        a = 255 if (v & 1) else 0
        out[i*4:i*4+4] = [r, g, b, a]
    return bytes(out)


def decode_rgba8888(data: bytes, width: int, height: int) -> bytes:
    """RGBA8888 — data is already RGBA, just clip to size."""
    size = width * height * 4
    return (data + b'\x00' * size)[:size]


#    Dispatch                                                                    

def decode_mobile_texture(tex) -> Optional[bytes]:
    """
    Decode a MobileTexture object to raw RGBA bytes.

    Returns None for PVRTC (not yet implemented) or on error.
    Returns bytes of length width*height*4 for supported formats.
    """
    from apps.methods.mobile_texture_db import (
        ENCODING_RGBA8888, ENCODING_ETC1,
        ENCODING_RGB565, ENCODING_RGBA4444, ENCODING_RGBA5551,
        ENCODING_IS_PVRTC,
    )

    data = tex.pixel_data or tex.raw_data
    if not data or tex.width <= 0 or tex.height <= 0:
        return None

    enc = tex.encoding_type
    w, h = tex.width, tex.height

    if enc == ENCODING_RGBA8888:
        return decode_rgba8888(data, w, h)
    elif enc == ENCODING_ETC1:
        return decode_etc1(data, w, h)
    elif enc == ENCODING_RGB565:
        return decode_rgb565(data, w, h)
    elif enc == ENCODING_RGBA4444:
        return decode_rgba4444(data, w, h)
    elif enc == ENCODING_RGBA5551:
        return decode_rgba5551(data, w, h)
    elif enc in ENCODING_IS_PVRTC:
        # iOS PVRTC — placeholder (standard PVRTC4/2 different from VC variant)
        placeholder = bytearray(w * h * 4)
        for y in range(h):
            for x in range(w):
                border = (x == 0 or x == w-1 or y == 0 or y == h-1)
                i = (y * w + x) * 4
                if border:
                    placeholder[i:i+4] = [255, 64, 128, 255]
                else:
                    v = 80 + (x + y) % 40
                    placeholder[i:i+4] = [v, v, v+10, 255]
        return bytes(placeholder)

    # VC Android PVRTC2 (enc 0x8C01 / 0x8C02)
    from apps.methods.mobile_texture_db import ENCODING_VC_PVRTC2, ENCODING_VC_PVRTC2B
    if enc in (ENCODING_VC_PVRTC2, ENCODING_VC_PVRTC2B):
        try:
            from apps.methods.pvrtc_decode import decode_pvrtc2
            return decode_pvrtc2(data, w, h)
        except Exception:
            pass

    return None


def to_pil_image(tex):
    """Convert a MobileTexture to a PIL Image, or return None."""
    try:
        from PIL import Image
        rgba = decode_mobile_texture(tex)
        if rgba is None:
            return None
        return Image.frombytes('RGBA', (tex.width, tex.height), rgba)
    except Exception:
        return None


