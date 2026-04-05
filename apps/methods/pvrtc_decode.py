#!/usr/bin/env python3
#this belongs in apps/methods/pvrtc_decode.py - Version: 2
# X-Seti - Apr 2026 - IMG Factory 1.6
# PVRTC2 (2bpp) pure-Python decoder
"""
PVRTC2 (PowerVR Texture Compression, 2bpp) decoder for GTA VC Android
mobile texture databases (gta3hi.pvr.dat, encoding 0x8C01 / 0x8C02).

Block layout: 8×4 pixels per block, 8 bytes per block
  bytes [0:4] = modulation data (32 bits = 8×4 pixels × 2 bits each, row-major)
  bytes [4:8] = colour word:
    bits [15: 0] = Colour A (15 bits + opaque flag at bit 15)
    bits [31:16] = Colour B (15 bits + opaque flag at bit 31)

Colour A format (15 bits):
  opaque (bit15=1): 5R | 5G | 4B  → scale: R×8, G×8, B<<4
  transparent (bit15=0): 4R | 4G | 4B | 3A → scale: R×17, G×17, B×17, A×36

Colour B format (15 bits):
  opaque (bit15=1): 5R | 5G | 5B  → scale: R×8, G×8, B×8
  transparent (bit15=0): same as A

Modulation (2 bits per pixel, row-major within 8×4 block):
  00 = Colour A
  01 = 5/8 A + 3/8 B
  10 = 3/8 A + 5/8 B
  11 = Colour B
"""

import struct
from typing import Tuple

## Methods list -
# _unpack_col_a
# _unpack_col_b
# decode_pvrtc2

_CLAMP = lambda v: max(0, min(255, v))

def _unpack_col_a(c: int) -> Tuple[int,int,int,int]:
    if c & 0x8000:
        return (((c>>10)&0x1F)*8, ((c>>5)&0x1F)*8,
                ((c & 0xF) << 4), 255)
    return (((c>>11)&0xF)*17, ((c>>7)&0xF)*17,
            ((c>>3)&0xF)*17, (c&0x7)*36)

def _unpack_col_b(c: int) -> Tuple[int,int,int,int]:
    if c & 0x8000:
        return (((c>>10)&0x1F)*8, ((c>>5)&0x1F)*8,
                (c & 0x1F)*8, 255)
    return (((c>>11)&0xF)*17, ((c>>7)&0xF)*17,
            ((c>>3)&0xF)*17, (c&0x7)*36)


def decode_pvrtc2(data: bytes, width: int, height: int) -> bytes:
    """
    Decode PVRTC2 (2bpp, 8×4 blocks) data to RGBA32 bytes.

    Uses only the base mip level (first bw*bh*8 bytes).

    Returns bytes of length width * height * 4 (RGBA32).
    """
    BWIDTH, BHEIGHT = 8, 4
    bw = max(1, width  // BWIDTH)
    bh = max(1, height // BHEIGHT)
    base_bytes = bw * bh * 8

    # Use only base mip level
    if len(data) > base_bytes:
        data = data[:base_bytes]

    out = bytearray(width * height * 4)

    for by in range(bh):
        for bx in range(bw):
            idx = (by * bw + bx) * 8
            if idx + 8 > len(data):
                break

            mod_bits = struct.unpack_from('<I', data, idx)[0]
            col_word  = struct.unpack_from('<I', data, idx + 4)[0]

            # Low 16 = Colour A, high 16 = Colour B (per PVRTC spec)
            A = _unpack_col_a(col_word & 0xFFFF)
            B = _unpack_col_b((col_word >> 16) & 0xFFFF)

            for py in range(BHEIGHT):
                for px in range(BWIDTH):
                    # Row-major modulation index
                    mod = (mod_bits >> ((py * BWIDTH + px) * 2)) & 0x3

                    if mod == 0:
                        r, g, b, a = A
                    elif mod == 3:
                        r, g, b, a = B
                    elif mod == 1:
                        r = (A[0]*5 + B[0]*3) // 8
                        g = (A[1]*5 + B[1]*3) // 8
                        b = (A[2]*5 + B[2]*3) // 8
                        a = (A[3]*5 + B[3]*3) // 8
                    else:  # mod == 2
                        r = (A[0]*3 + B[0]*5) // 8
                        g = (A[1]*3 + B[1]*5) // 8
                        b = (A[2]*3 + B[2]*5) // 8
                        a = (A[3]*3 + B[3]*5) // 8

                    ox = bx * BWIDTH  + px
                    oy = by * BHEIGHT + py
                    if ox < width and oy < height:
                        i = (oy * width + ox) * 4
                        out[i:i+4] = [
                            _CLAMP(r), _CLAMP(g),
                            _CLAMP(b), _CLAMP(a)
                        ]

    return bytes(out)


__all__ = ['decode_pvrtc2']
