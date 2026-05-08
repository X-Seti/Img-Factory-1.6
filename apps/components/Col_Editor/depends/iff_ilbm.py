#!/usr/bin/env python3
#this belongs in apps/methods/iff_ilbm.py - Version: 2
# X-Seti - April26 2026 - IMG Factory 1.6
# IFF ILBM full reader/writer — OCS/ECS/AGA/HAM6/HAM8/EHB/24-bit

"""
Full IFF ILBM (Interleaved Bitmap) codec for IMG Factory.

Read support:
  Indexed 1-8 bitplanes (OCS/ECS/AGA)
  EHB (Extra Half-Brite, 64 colours)
  HAM6 / HAM8 (Hold-And-Modify)
  24-bit truecolour
  ByteRun1 / uncompressed BODY
  Masking plane (ignored on read)

Write support:
  Indexed 1-8 bitplanes with ByteRun1 compression
  HAM6 / HAM8
  24-bit truecolour
  Optional CAMG, ANNO, GRAB chunks
  Transparency colour index

All read functions return RGBA bytes.
All write functions accept RGBA bytes + palette.
"""

import struct
from typing import Optional, List, Tuple

## Methods list -
# find_chunk
# iter_chunks
# read_iff_ilbm
# read_iff_ilbm_rgba
# write_iff_ilbm
# write_iff_ilbm_rgba
# write_iff_ham
# rgba_to_indexed
# _decode_byterun1
# _encode_byterun1
# _planar_to_chunky
# _chunky_to_planar
# _decode_ham
# _encode_ham6
# _encode_ham8
# _decode_24bit
# _encode_24bit
# _build_chunk
# _build_cmap
# _build_bmhd

# ── IFF chunk tags ────────────────────────────────────────────────────────────
FORM = b'FORM'; ILBM = b'ILBM'; PBM  = b'PBM '
BMHD = b'BMHD'; CMAP = b'CMAP'; BODY = b'BODY'
CAMG = b'CAMG'; ANNO = b'ANNO'; GRAB = b'GRAB'
CRNG = b'CRNG'; CCRT = b'CCRT'; DPAN = b'DPAN'

# CAMG flags
CAMG_HAM  = 0x0800
CAMG_EHB  = 0x0080
CAMG_LACE = 0x0004
CAMG_HIRES= 0x8000
CAMG_AGA  = 0x0001


# =============================================================================
# Low-level helpers
# =============================================================================

def find_chunk(data: bytes, tag: bytes, start: int = 12) -> bytes: #vers 1
    """Return data of first matching IFF chunk, or b'' if not found."""
    pos = start
    while pos < len(data) - 8:
        ctag  = data[pos:pos+4]
        csize = struct.unpack_from('>I', data, pos+4)[0]
        if ctag == tag:
            return data[pos+8 : pos+8+csize]
        pos += 8 + csize + (csize & 1)
    return b''


def iter_chunks(data: bytes, start: int = 12): #vers 1
    """Yield (tag, chunk_data) pairs from an IFF container."""
    pos = start
    while pos < len(data) - 8:
        tag   = data[pos:pos+4]
        csize = struct.unpack_from('>I', data, pos+4)[0]
        yield tag, data[pos+8 : pos+8+csize]
        pos += 8 + csize + (csize & 1)


def _build_chunk(tag: bytes, data: bytes) -> bytes: #vers 1
    """Build a single IFF chunk with padding."""
    pad = b'\x00' if len(data) & 1 else b''
    return tag + struct.pack('>I', len(data)) + data + pad


def _build_bmhd(w: int, h: int, planes: int, compress: int = 1,
                masking: int = 0, transparent: int = 0,
                x: int = 0, y: int = 0) -> bytes: #vers 1
    """Build a BMHD chunk."""
    return struct.pack('>HHhhBBBBHBBhh',
        w, h, x, y, planes, masking, compress, 0,
        transparent, 10, 11, w, h)


def _build_cmap(palette: List[Tuple]) -> bytes: #vers 1
    """Build a CMAP chunk from list of (R,G,B) tuples."""
    out = bytearray()
    for r, g, b in palette:
        out += bytes([r & 0xFF, g & 0xFF, b & 0xFF])
    return bytes(out)


# =============================================================================
# Compression: ByteRun1 / PackBits
# =============================================================================

def _decode_byterun1(data: bytes, row_bytes: int, height: int,
                     n_planes: int = 1) -> bytes: #vers 2
    """Decompress ByteRun1 (PackBits) BODY data."""
    total = row_bytes * height * n_planes
    out = bytearray()
    i = 0
    n = len(data)
    while i < n and len(out) < total:
        b = data[i]; i += 1
        if b <= 127:
            count = b + 1
            out.extend(data[i:i+count]); i += count
        elif b != 128:
            count = 257 - b
            out.extend([data[i]] * count); i += 1
    return bytes(out)


def _encode_byterun1(data: bytes) -> bytes: #vers 2
    """Compress data with ByteRun1 (PackBits)."""
    out = bytearray()
    i = 0
    n = len(data)
    while i < n:
        # Check for run
        j = i + 1
        while j < n and j - i < 128 and data[j] == data[i]:
            j += 1
        run = j - i
        if run > 2:
            out.append((257 - run) & 0xFF)
            out.append(data[i])
            i = j
            continue
        # Literal run
        j = i + 1
        while j < n and j - i < 128:
            if j + 2 < n and data[j] == data[j+1] == data[j+2]:
                break
            j += 1
        count = j - i
        out.append(count - 1)
        out.extend(data[i:j])
        i = j
    return bytes(out)


# =============================================================================
# Planar ↔ Chunky conversion
# =============================================================================

def _planar_to_chunky(body: bytes, width: int, height: int,
                       n_planes: int, masking: int = 0) -> bytes: #vers 2
    """Convert interleaved planar BODY to chunky 8bpp pixel indices."""
    row_bytes = (width + 15) // 16 * 2
    planes_per_row = n_planes + (1 if masking == 1 else 0)
    out = bytearray(width * height)
    for y in range(height):
        row_off = y * row_bytes * planes_per_row
        plane_rows = []
        for p in range(n_planes):
            start = row_off + p * row_bytes
            plane_rows.append(body[start:start + row_bytes])
        for x in range(width):
            byte_i = x // 8
            bit_m  = 0x80 >> (x % 8)
            px = 0
            for p in range(n_planes):
                if byte_i < len(plane_rows[p]) and plane_rows[p][byte_i] & bit_m:
                    px |= (1 << p)
            out[y * width + x] = px
    return bytes(out)


def _chunky_to_planar(pixels: bytes, width: int, height: int,
                       n_planes: int) -> bytes: #vers 2
    """Convert chunky 8bpp to interleaved planar — returns full BODY bytes."""
    row_bytes = (width + 15) // 16 * 2
    plane_rows = [bytearray(row_bytes) for _ in range(n_planes)]
    body = bytearray()
    for y in range(height):
        for p in range(n_planes):
            plane_rows[p] = bytearray(row_bytes)
        for x in range(width):
            px = pixels[y * width + x]
            byte_i = x // 8
            bit_m  = 0x80 >> (x % 8)
            for p in range(n_planes):
                if (px >> p) & 1:
                    plane_rows[p][byte_i] |= bit_m
        for p in range(n_planes):
            body.extend(plane_rows[p])
    return bytes(body)


# =============================================================================
# HAM encode/decode
# =============================================================================

def _decode_ham(body: bytes, width: int, height: int, n_planes: int,
                palette: List[Tuple], masking: int = 0) -> bytearray: #vers 1
    """Decode HAM6 or HAM8 BODY to RGBA bytearray."""
    is_ham8 = (n_planes == 8)
    base_planes = n_planes - 2
    base_n = 1 << base_planes  # 16 for HAM6, 64 for HAM8
    row_bytes = (width + 15) // 16 * 2
    planes_per_row = n_planes + (1 if masking == 1 else 0)
    rgba = bytearray(width * height * 4)
    for y in range(height):
        row_off = y * row_bytes * planes_per_row
        plane_rows = [body[row_off + p*row_bytes : row_off + p*row_bytes + row_bytes]
                      for p in range(n_planes)]
        pr = pg = pb = 0
        for x in range(width):
            byte_i = x // 8
            bit_m  = 0x80 >> (x % 8)
            px = sum(((1 if (plane_rows[p][byte_i] & bit_m) else 0) << p)
                     for p in range(n_planes) if byte_i < len(plane_rows[p]))
            ctrl = px >> base_planes
            val  = px & (base_n - 1)
            if ctrl == 0:
                if val < len(palette):
                    pr, pg, pb = palette[val]
            elif ctrl == 1:  # modify blue
                pb = (val << (8 - base_planes)) if is_ham8 else val * 17
            elif ctrl == 2:  # modify red
                pr = (val << (8 - base_planes)) if is_ham8 else val * 17
            elif ctrl == 3:  # modify green
                pg = (val << (8 - base_planes)) if is_ham8 else val * 17
            i = (y * width + x) * 4
            rgba[i:i+4] = [pr, pg, pb, 255]
    return rgba


def _encode_ham6(rgba: bytes, width: int, height: int,
                 palette: List[Tuple]) -> bytes: #vers 1
    """Encode RGBA to HAM6 chunky pixel stream (6 bits/pixel)."""
    def nearest(r, g, b):
        return min(range(len(palette)),
                   key=lambda i: (palette[i][0]-r)**2 +
                                 (palette[i][1]-g)**2 +
                                 (palette[i][2]-b)**2)
    rows = []
    for y in range(height):
        pr = pg = pb = 0
        row = []
        for x in range(width):
            i = (y * width + x) * 4
            r, g, b = rgba[i], rgba[i+1], rgba[i+2]
            bi  = nearest(r, g, b)
            bc  = palette[bi]
            r4, g4, b4 = r >> 4, g >> 4, b >> 4
            e_pal  = (bc[0]-r)**2   + (bc[1]-g)**2   + (bc[2]-b)**2
            e_r    = (r4*17-r)**2   + (pg-g)**2       + (pb-b)**2
            e_g    = (pr-r)**2      + (g4*17-g)**2    + (pb-b)**2
            e_b    = (pr-r)**2      + (pg-g)**2       + (b4*17-b)**2
            best   = min(e_pal, e_r, e_g, e_b)
            if best == e_pal:
                row.append(bi); pr, pg, pb = bc
            elif best == e_r:
                row.append(0x20 | r4); pr = r4 * 17
            elif best == e_g:
                row.append(0x30 | g4); pg = g4 * 17
            else:
                row.append(0x10 | b4); pb = b4 * 17
        rows.append(row)
    # Convert to planar
    out = bytearray()
    row_bytes = (width + 15) // 16 * 2
    for row in rows:
        planes = [bytearray(row_bytes) for _ in range(6)]
        for x, px in enumerate(row):
            for p in range(6):
                if (px >> p) & 1:
                    planes[p][x // 8] |= 0x80 >> (x % 8)
        for p in planes:
            out.extend(p)
    return bytes(out)


def _encode_ham8(rgba: bytes, width: int, height: int,
                 palette: List[Tuple]) -> bytes: #vers 1
    """Encode RGBA to HAM8 chunky pixel stream (8 bits/pixel)."""
    def nearest(r, g, b):
        return min(range(len(palette)),
                   key=lambda i: (palette[i][0]-r)**2 +
                                 (palette[i][1]-g)**2 +
                                 (palette[i][2]-b)**2)
    rows = []
    for y in range(height):
        pr = pg = pb = 0
        row = []
        for x in range(width):
            i = (y * width + x) * 4
            r, g, b = rgba[i], rgba[i+1], rgba[i+2]
            bi  = nearest(r, g, b)
            bc  = palette[bi]
            r6, g6, b6 = r >> 2, g >> 2, b >> 2
            e_pal = (bc[0]-r)**2   + (bc[1]-g)**2   + (bc[2]-b)**2
            e_r   = ((r6<<2)-r)**2 + (pg-g)**2       + (pb-b)**2
            e_g   = (pr-r)**2      + ((g6<<2)-g)**2  + (pb-b)**2
            e_b   = (pr-r)**2      + (pg-g)**2       + ((b6<<2)-b)**2
            best  = min(e_pal, e_r, e_g, e_b)
            if best == e_pal:
                row.append(bi); pr, pg, pb = bc
            elif best == e_r:
                row.append(0x80 | r6); pr = r6 << 2
            elif best == e_g:
                row.append(0xC0 | g6); pg = g6 << 2
            else:
                row.append(0x40 | b6); pb = b6 << 2
        rows.append(row)
    out = bytearray()
    row_bytes = (width + 15) // 16 * 2
    for row in rows:
        planes = [bytearray(row_bytes) for _ in range(8)]
        for x, px in enumerate(row):
            for p in range(8):
                if (px >> p) & 1:
                    planes[p][x // 8] |= 0x80 >> (x % 8)
        for p in planes:
            out.extend(p)
    return bytes(out)


def _decode_24bit(body: bytes, width: int, height: int,
                  masking: int = 0) -> bytearray: #vers 1
    """Decode 24-bit IFF (8 planes each for R/G/B) to RGBA."""
    row_bytes = (width + 15) // 16 * 2
    n_planes = 24 + (1 if masking == 1 else 0)
    rgba = bytearray(width * height * 4)
    for y in range(height):
        row_off = y * row_bytes * n_planes
        channels = [body[row_off + p*row_bytes : row_off + p*row_bytes + row_bytes]
                    for p in range(24)]
        for x in range(width):
            byte_i = x // 8
            bit_m  = 0x80 >> (x % 8)
            r = sum(((1 if (channels[p][byte_i] & bit_m) else 0) << p)
                    for p in range(8) if byte_i < len(channels[p]))
            g = sum(((1 if (channels[8+p][byte_i] & bit_m) else 0) << p)
                    for p in range(8) if byte_i < len(channels[8+p]))
            b = sum(((1 if (channels[16+p][byte_i] & bit_m) else 0) << p)
                    for p in range(8) if byte_i < len(channels[16+p]))
            i = (y * width + x) * 4
            rgba[i:i+4] = [r, g, b, 255]
    return rgba


def _encode_24bit(rgba: bytes, width: int, height: int) -> bytes: #vers 1
    """Encode RGBA to 24-bit IFF BODY (8 planes each R/G/B, interleaved)."""
    row_bytes = (width + 15) // 16 * 2
    body = bytearray()
    for y in range(height):
        # Build 24 plane rows
        plane_rows = [bytearray(row_bytes) for _ in range(24)]
        for x in range(width):
            i = (y * width + x) * 4
            r, g, b = rgba[i], rgba[i+1], rgba[i+2]
            byte_i = x // 8
            bit_m  = 0x80 >> (x % 8)
            for p in range(8):
                if (r >> p) & 1:    plane_rows[p][byte_i]    |= bit_m
                if (g >> p) & 1:    plane_rows[8+p][byte_i]  |= bit_m
                if (b >> p) & 1:    plane_rows[16+p][byte_i] |= bit_m
        for p in range(24):
            body.extend(plane_rows[p])
    return bytes(body)


# =============================================================================
# Palette helpers
# =============================================================================

def rgba_to_indexed(rgba: bytes, width: int, height: int,
                    n_colors: int = 256,
                    alpha_color: Optional[Tuple] = None) -> Tuple: #vers 1
    """
    Quantise RGBA image to indexed palette.
    alpha_color: if set, pixels with A<128 map to this palette entry (index 0).
    Returns (palette, pixels) where:
      palette = list of (R,G,B) tuples, len <= n_colors
      pixels  = bytes of width*height 8bpp indices
    """
    try:
        from PIL import Image
        img = Image.frombytes('RGBA', (width, height), rgba)
        if alpha_color is not None:
            # Force colour 0 = alpha_color, map transparent pixels to index 0
            bg = Image.new('RGBA', (width, height), alpha_color + (255,))
            img_rgb = Image.alpha_composite(bg, img).convert('RGB')
        else:
            img_rgb = img.convert('RGB')
        q = img_rgb.quantize(colors=n_colors, method=Image.Quantize.MEDIANCUT)
        pal_flat = q.getpalette()
        palette  = [(pal_flat[i*3], pal_flat[i*3+1], pal_flat[i*3+2])
                    for i in range(n_colors)]
        if alpha_color is not None:
            # Put alpha_color at index 0
            palette[0] = alpha_color[:3]
        pixels = bytes(q.tobytes())
        return palette, pixels
    except ImportError:
        # No PIL — build greyscale palette
        palette = [(i, i, i) for i in range(n_colors)]
        pixels  = bytes(width * height)
        return palette, pixels


# =============================================================================
# High-level read
# =============================================================================

def read_iff_ilbm(data: bytes) -> Optional[Tuple]: #vers 2
    """
    Parse IFF ILBM. Returns (width, height, palette, pixels) or None.
    palette: list of (R,G,B). pixels: bytes of 8bpp chunky indices.
    For truecolour/HAM, pixels are raw RGBA (width*height*4).
    """
    if len(data) < 12 or data[:4] != FORM or data[8:12] != ILBM:
        return None
    chunks = {}
    for tag, body in iter_chunks(data):
        chunks.setdefault(tag, body)

    bh = chunks.get(BMHD, b'')
    if len(bh) < 14:
        return None
    width, height = struct.unpack_from('>HH', bh, 0)
    n_planes  = bh[8]
    masking   = bh[9]
    compress  = bh[10]

    camg_raw = chunks.get(CAMG, b'')
    camg = struct.unpack_from('>I', camg_raw)[0] if len(camg_raw) >= 4 else 0
    is_ham  = bool(camg & CAMG_HAM)
    is_ehb  = bool(camg & CAMG_EHB) and not is_ham
    is_ham8 = is_ham and n_planes == 8

    # Palette from CMAP
    cm = chunks.get(CMAP, b'')
    palette = [(cm[i*3], cm[i*3+1], cm[i*3+2])
               for i in range(len(cm) // 3)]
    if is_ehb and len(palette) == 32:
        palette += [(r>>1, g>>1, b>>1) for r,g,b in palette]

    # BODY
    body_raw = chunks.get(BODY, b'')
    if not body_raw:
        return None
    row_bytes = (width + 15) // 16 * 2
    planes_per_row = n_planes + (1 if masking == 1 else 0)
    body = _decode_byterun1(body_raw, row_bytes, height * planes_per_row) \
           if compress == 1 else body_raw

    if n_planes == 24:
        pixels = _decode_24bit(body, width, height, masking)
        return width, height, [], pixels  # pixels = raw RGBA

    if is_ham:
        if not palette:
            palette = [(i*17, i*17, i*17) for i in range(16)]
        pixels = _decode_ham(body, width, height, n_planes, palette, masking)
        return width, height, palette, pixels  # pixels = raw RGBA

    # Indexed
    if not palette:
        n = 1 << n_planes
        palette = [(i*255//(n-1),)*3 for i in range(n)]
    pixels = _planar_to_chunky(body, width, height, n_planes, masking)
    return width, height, palette, pixels


def read_iff_ilbm_rgba(data: bytes,
                        alpha_index: Optional[int] = None) -> Optional[Tuple]: #vers 1
    """
    Parse IFF ILBM and return (width, height, rgba_bytes).
    alpha_index: palette index to treat as transparent (0 for Amiga colour 0).
    """
    result = read_iff_ilbm(data)
    if result is None:
        return None
    width, height, palette, pixels = result

    # pixels is already RGBA for HAM/24bit
    if len(pixels) == width * height * 4:
        return width, height, bytes(pixels)

    # Convert indexed to RGBA
    rgba = bytearray(width * height * 4)
    for i in range(width * height):
        idx = pixels[i]
        if idx < len(palette):
            r, g, b = palette[idx]
        else:
            r = g = b = 0
        a = 0 if (alpha_index is not None and idx == alpha_index) else 255
        rgba[i*4:i*4+4] = [r, g, b, a]
    return width, height, bytes(rgba)


# =============================================================================
# High-level write
# =============================================================================

def write_iff_ilbm(width: int, height: int,
                   palette: List[Tuple],
                   pixels: bytes,
                   n_planes: int = 8,
                   compress: bool = True,
                   camg_flags: int = 0,
                   transparent_index: int = 0,
                   annotation: str = '') -> bytes: #vers 2
    """
    Write indexed IFF ILBM file.
    pixels: bytes of width*height 8bpp chunky palette indices.
    """
    body_raw = _chunky_to_planar(pixels, width, height, n_planes)
    body_data = _encode_byterun1(body_raw) if compress else body_raw

    chunks = b''
    chunks += _build_chunk(BMHD, _build_bmhd(
        width, height, n_planes,
        compress=1 if compress else 0,
        transparent=transparent_index))
    chunks += _build_chunk(CMAP, _build_cmap(palette))
    if camg_flags:
        chunks += _build_chunk(CAMG, struct.pack('>I', camg_flags))
    if annotation:
        ann = annotation.encode('latin-1', errors='replace')
        chunks += _build_chunk(ANNO, ann)
    chunks += _build_chunk(BODY, body_data)

    form_body = ILBM + chunks
    return FORM + struct.pack('>I', len(form_body)) + form_body


def write_iff_ilbm_rgba(rgba: bytes, width: int, height: int,
                         n_planes: int = 8,
                         alpha_color: Optional[Tuple] = (0, 0, 0),
                         alpha_is_index0: bool = True,
                         compress: bool = True,
                         camg_flags: int = 0,
                         annotation: str = 'DP5 Workshop') -> bytes: #vers 1
    """
    Write IFF ILBM from RGBA bytes.
    Quantises to n_planes bitplanes automatically.
    alpha_color: colour to use for transparent pixels (index 0).
    """
    n_colors = 1 << n_planes
    palette, pixels = rgba_to_indexed(
        rgba, width, height, n_colors,
        alpha_color=alpha_color if alpha_is_index0 else None)
    return write_iff_ilbm(
        width, height, palette, pixels,
        n_planes=n_planes, compress=compress,
        camg_flags=camg_flags,
        transparent_index=0 if alpha_is_index0 else -1,
        annotation=annotation)


def write_iff_ham(rgba: bytes, width: int, height: int,
                  palette: Optional[List[Tuple]] = None,
                  ham8: bool = False,
                  compress: bool = True,
                  annotation: str = 'DP5 Workshop') -> bytes: #vers 1
    """
    Write HAM6 or HAM8 IFF ILBM from RGBA bytes.
    palette: 16 (HAM6) or 64 (HAM8) base colours. Auto-generated if None.
    """
    n_planes = 8 if ham8 else 6
    base_n   = 64 if ham8 else 16

    if palette is None:
        try:
            from PIL import Image
            img = Image.frombytes('RGBA', (width, height), rgba).convert('RGB')
            q   = img.quantize(colors=base_n)
            pf  = q.getpalette()
            palette = [(pf[i*3], pf[i*3+1], pf[i*3+2]) for i in range(base_n)]
        except ImportError:
            palette = [(i*255//(base_n-1),)*3 for i in range(base_n)]

    palette = list(palette)[:base_n]
    while len(palette) < base_n:
        palette.append((0, 0, 0))

    body_raw = (_encode_ham8(rgba, width, height, palette) if ham8
                else _encode_ham6(rgba, width, height, palette))
    body_data = _encode_byterun1(body_raw) if compress else body_raw

    camg = CAMG_HAM | (CAMG_AGA if ham8 else 0)
    chunks  = _build_chunk(BMHD, _build_bmhd(width, height, n_planes,
                                              compress=1 if compress else 0))
    chunks += _build_chunk(CMAP, _build_cmap(palette))
    chunks += _build_chunk(CAMG, struct.pack('>I', camg))
    if annotation:
        chunks += _build_chunk(ANNO, annotation.encode('latin-1', errors='replace'))
    chunks += _build_chunk(BODY, body_data)

    form_body = ILBM + chunks
    return FORM + struct.pack('>I', len(form_body)) + form_body


def write_iff_24bit(rgba: bytes, width: int, height: int,
                    compress: bool = True,
                    annotation: str = 'DP5 Workshop') -> bytes: #vers 1
    """Write 24-bit truecolour IFF ILBM from RGBA bytes."""
    body_raw  = _encode_24bit(rgba, width, height)
    body_data = _encode_byterun1(body_raw) if compress else body_raw

    chunks  = _build_chunk(BMHD, _build_bmhd(width, height, 24,
                                              compress=1 if compress else 0))
    chunks += _build_chunk(CAMG, struct.pack('>I', 0))
    if annotation:
        chunks += _build_chunk(ANNO, annotation.encode('latin-1', errors='replace'))
    chunks += _build_chunk(BODY, body_data)

    form_body = ILBM + chunks
    return FORM + struct.pack('>I', len(form_body)) + form_body


__all__ = [
    'read_iff_ilbm', 'read_iff_ilbm_rgba',
    'write_iff_ilbm', 'write_iff_ilbm_rgba',
    'write_iff_ham', 'write_iff_24bit',
    'rgba_to_indexed',
    'find_chunk', 'iter_chunks',
    'CAMG_HAM', 'CAMG_EHB', 'CAMG_LACE', 'CAMG_HIRES', 'CAMG_AGA',
]
