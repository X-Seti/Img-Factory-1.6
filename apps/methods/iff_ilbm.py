#!/usr/bin/env python3
#this belongs in apps/methods/iff_ilbm.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6
# IFF ILBM (Amiga Interchange File Format - Interleaved Bitmap) reader/writer
"""
Reads and writes IFF ILBM files as used by Deluxe Paint on the Amiga.

Supported:
  Read:  BMHD + CMAP + BODY (planar → chunky, ByteRun1 decompression)
  Write: BMHD + CMAP + BODY (chunky 8bpp → planar, ByteRun1 compressed)

Returns/accepts images as (width, height, palette, pixels) tuples:
  palette: list of 256 (R,G,B) tuples
  pixels:  bytes of width*height palette indices (8bpp chunky)
"""

import struct
from typing import Optional, List, Tuple

## Methods list -
# read_iff_ilbm
# write_iff_ilbm
# _decode_byterun1
# _encode_byterun1
# _planar_to_chunky
# _chunky_to_planar

IFF_FORM = b'FORM'
IFF_ILBM = b'ILBM'
IFF_BMHD = b'BMHD'
IFF_CMAP = b'CMAP'
IFF_BODY = b'BODY'
IFF_CAMG = b'CAMG'
IFF_ANNO = b'ANNO'

BMHD_FMT = '>HHhhBBBBHBBhhHH'   # big-endian
# width, height, x, y, nPlanes, masking, compression, pad, transparentColor,
# xAspect, yAspect, pageWidth, pageHeight, (14 bytes, rest optional/pad)


def _decode_byterun1(data: bytes, row_bytes: int, height: int) -> bytes:
    """Decompress ByteRun1 (PackBits) BODY data."""
    out = bytearray()
    i = 0
    n = len(data)
    while i < n and len(out) < row_bytes * height:
        b = data[i]; i += 1
        if b <= 127:
            count = b + 1
            out.extend(data[i:i+count]); i += count
        elif b != 128:
            count = 257 - b
            out.extend([data[i]] * count); i += 1
    return bytes(out)


def _encode_byterun1(data: bytes) -> bytes:
    """Compress data using ByteRun1 (PackBits)."""
    out = bytearray()
    i = 0
    n = len(data)
    while i < n:
        # Look for run of same byte
        j = i + 1
        while j < n and j - i < 128 and data[j] == data[i]:
            j += 1
        run = j - i
        if run > 2:
            out.append(257 - run & 0xFF)
            out.append(data[i])
            i = j
            continue
        # Literal run
        j = i + 1
        while j < n and j - i < 128:
            if j + 1 < n and data[j] == data[j+1]:
                break
            j += 1
        count = j - i
        out.append(count - 1)
        out.extend(data[i:j])
        i = j
    return bytes(out)


def _planar_to_chunky(planes: List[bytes], width: int, height: int,
                      n_planes: int) -> bytes:
    """Convert planar bitmap data to chunky 8bpp palette indices."""
    row_bytes = (width + 15) // 16 * 2  # words, padded
    out = bytearray(width * height)
    for y in range(height):
        for x in range(width):
            byte_pos = x // 8
            bit_pos  = 7 - (x % 8)
            pixel = 0
            for p in range(n_planes):
                plane_row = planes[p][y * row_bytes : (y+1) * row_bytes]
                if byte_pos < len(plane_row):
                    pixel |= ((plane_row[byte_pos] >> bit_pos) & 1) << p
            out[y * width + x] = pixel
    return bytes(out)


def _chunky_to_planar(pixels: bytes, width: int, height: int,
                      n_planes: int) -> List[bytes]:
    """Convert chunky 8bpp indices to planar bitmap data."""
    row_bytes = (width + 15) // 16 * 2
    planes = [bytearray(row_bytes * height) for _ in range(n_planes)]
    for y in range(height):
        for x in range(width):
            pixel = pixels[y * width + x]
            byte_pos = x // 8
            bit_pos  = 7 - (x % 8)
            for p in range(n_planes):
                if (pixel >> p) & 1:
                    planes[p][y * row_bytes + byte_pos] |= (1 << bit_pos)
    return [bytes(p) for p in planes]


def read_iff_ilbm(data: bytes) -> Optional[Tuple]:
    """
    Parse an IFF ILBM file.

    Returns:
        (width, height, palette, pixels) or None on failure
        palette: list of 256 (R,G,B) tuples  
        pixels:  bytes, width*height chunky 8bpp indices
    """
    if data[:4] != IFF_FORM or data[8:12] != IFF_ILBM:
        return None

    # Parse chunks
    chunks = {}
    pos = 12
    while pos < len(data) - 8:
        tag = data[pos:pos+4]
        size = struct.unpack_from('>I', data, pos+4)[0]
        chunks[tag] = data[pos+8 : pos+8+size]
        pos += 8 + size + (size & 1)   # IFF chunks are word-aligned

    if IFF_BMHD not in chunks:
        return None

    # BMHD
    bh = chunks[IFF_BMHD]
    width, height = struct.unpack_from('>HH', bh, 0)
    n_planes  = bh[8]
    masking   = bh[9]
    compress  = bh[10]

    # CMAP → palette
    palette = [(0,0,0)] * 256
    if IFF_CMAP in chunks:
        cm = chunks[IFF_CMAP]
        for i in range(min(256, len(cm) // 3)):
            palette[i] = (cm[i*3], cm[i*3+1], cm[i*3+2])

    # BODY → planar data
    if IFF_BODY not in chunks:
        return None

    row_bytes = (width + 15) // 16 * 2
    body = chunks[IFF_BODY]
    if compress == 1:
        body = _decode_byterun1(body, row_bytes * n_planes, height)

    # Split into planes (interleaved: row0_plane0, row0_plane1, ..., row1_plane0, ...)
    planes = [bytearray() for _ in range(n_planes)]
    pos = 0
    for _y in range(height):
        for p in range(n_planes):
            planes[p].extend(body[pos:pos+row_bytes])
            pos += row_bytes
        if masking == 1:
            pos += row_bytes  # skip mask plane

    pixels = _planar_to_chunky([bytes(p) for p in planes], width, height, n_planes)
    return width, height, palette, pixels


def write_iff_ilbm(width: int, height: int,
                   palette: List[Tuple[int,int,int]],
                   pixels: bytes,
                   n_planes: int = 8,
                   compress: bool = True) -> bytes:
    """
    Write an IFF ILBM file.

    Args:
        width, height: image dimensions
        palette:  list of up to 256 (R,G,B) tuples
        pixels:   bytes of width*height 8bpp chunky palette indices
        n_planes: colour depth in bitplanes (8 for 256-colour)
        compress: use ByteRun1 compression
    """
    # BMHD chunk
    row_bytes = (width + 15) // 16 * 2
    bmhd_data = struct.pack('>HHhhBBBBHBBhhHH',
        width, height, 0, 0,   # w, h, x, y
        n_planes, 0,            # planes, no masking
        1 if compress else 0,  # compression
        0,                     # pad
        0,                     # transparent index
        10, 11,                # aspect ratio
        width, height,         # page size
        0, 0)                  # (extra)

    def chunk(tag: bytes, data: bytes) -> bytes:
        size = len(data)
        pad = b'\x00' if size & 1 else b''
        return tag + struct.pack('>I', size) + data + pad

    # CMAP chunk
    pal_bytes = bytearray()
    for i in range(min(256, len(palette))):
        r, g, b = palette[i]
        pal_bytes.extend([r&0xFF, g&0xFF, b&0xFF])
    while len(pal_bytes) < 256 * 3:
        pal_bytes.extend([0, 0, 0])

    # BODY chunk — convert chunky to planar, interleave rows
    planes = _chunky_to_planar(pixels, width, height, n_planes)
    body_raw = bytearray()
    for y in range(height):
        for p in range(n_planes):
            row = planes[p][y * row_bytes : (y+1) * row_bytes]
            body_raw.extend(row)

    body_data = _encode_byterun1(bytes(body_raw)) if compress else bytes(body_raw)

    # Assemble FORM
    ilbm_body = (
        chunk(IFF_BMHD, bmhd_data) +
        chunk(IFF_CMAP, bytes(pal_bytes)) +
        chunk(IFF_BODY, body_data)
    )
    form = IFF_FORM + struct.pack('>I', len(IFF_ILBM) + len(ilbm_body)) + IFF_ILBM + ilbm_body
    return form


__all__ = ['read_iff_ilbm', 'write_iff_ilbm']
