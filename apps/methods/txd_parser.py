# X-Seti - May 2026 - apps/methods/txd_parser.py - Version: 2
# Self-contained GTA PC TXD parser (VC/III/SA).
# Decodes DXT1, DXT3, DXT5 and uncompressed RGBA32/RGB24 textures to RGBA8888.
# No external dependencies -- works standalone inside Model-Workshop.
#
# Returns a list of dicts compatible with COL3DViewport.load_textures():
#   { 'name': str, 'width': int, 'height': int,
#     'rgba_data': bytes,  # raw RGBA8888 bytes (w*h*4)
#     'format': str }      # 'DXT1'/'DXT3'/'DXT5'/'RGBA32' etc.

import struct
from typing import List, Optional


# ─── RW chunk types ──────────────────────────────────────────────────────────
RW_STRUCT           = 0x01
RW_STRING           = 0x02
RW_TEXTURE_DICT     = 0x16
RW_TEXTURE_NATIVE   = 0x15

# ─── Raster format flags (used in VC/III/SA PC TXDs) ─────────────────────────
RASTER_FORMAT_DEFAULT = 0x0000
RASTER_1555     = 0x0100   # ARGB1555
RASTER_565      = 0x0200   # RGB565
RASTER_4444     = 0x0300   # ARGB4444
RASTER_LUM8     = 0x0400   # Luminance 8
RASTER_8888     = 0x0500   # ARGB8888
RASTER_888      = 0x0600   # RGB888 (no alpha)
RASTER_555      = 0x0A00   # BGR555
RASTER_PAL8     = 0x2000   # 8-bit palette
RASTER_PAL4     = 0x4000   # 4-bit palette
RASTER_MIPMAP   = 0x0800   # has mipmaps
RASTER_COMPRESS_D3D8 = 0x00800000
RASTER_COMPRESS_PVRTC2 = 0x01000000
RASTER_COMPRESS_PVRTC4 = 0x02000000

D3D_DXT1 = 0x31545844  # 'DXT1'
D3D_DXT2 = 0x32545844
D3D_DXT3 = 0x33545844
D3D_DXT4 = 0x34545844
D3D_DXT5 = 0x35545844


def _read_chunk(data: bytes, pos: int):
    """Read an RW chunk header. Returns (type, size, version, next_pos)."""
    if pos + 12 > len(data):
        return None, 0, 0, pos
    t, s, v = struct.unpack_from('<III', data, pos)
    return t, s, v, pos + 12


def _decode_dxt1(data: bytes, w: int, h: int) -> bytes:
    """Decode DXT1 block-compressed data to RGBA8888."""
    out = bytearray(w * h * 4)
    bw, bh = max(1, (w+3)//4), max(1, (h+3)//4)
    pos = 0
    for by in range(bh):
        for bx in range(bw):
            if pos + 8 > len(data): break
            c0, c1 = struct.unpack_from('<HH', data, pos)
            bits   = struct.unpack_from('<I', data, pos+4)[0]
            pos += 8
            # Decode two reference colours from RGB565
            def rgb565(c):
                r = ((c >> 11) & 0x1F) * 255 // 31
                g = ((c >>  5) & 0x3F) * 255 // 63
                b = ( c        & 0x1F) * 255 // 31
                return r, g, b
            r0,g0,b0 = rgb565(c0)
            r1,g1,b1 = rgb565(c1)
            if c0 > c1:
                palette = [
                    (r0,g0,b0,255),
                    (r1,g1,b1,255),
                    ((2*r0+r1)//3,(2*g0+g1)//3,(2*b0+b1)//3,255),
                    ((r0+2*r1)//3,(g0+2*g1)//3,(b0+2*b1)//3,255),
                ]
            else:
                palette = [
                    (r0,g0,b0,255),
                    (r1,g1,b1,255),
                    ((r0+r1)//2,(g0+g1)//2,(b0+b1)//2,255),
                    (0,0,0,0),
                ]
            for row in range(4):
                for col in range(4):
                    px = bx*4+col; py = by*4+row
                    if px >= w or py >= h: continue
                    idx = (bits >> (2*(row*4+col))) & 3
                    off = (py*w+px)*4
                    out[off:off+4] = palette[idx]
    return bytes(out)


def _decode_dxt3(data: bytes, w: int, h: int) -> bytes:
    """Decode DXT3 block-compressed data to RGBA8888."""
    out = bytearray(w * h * 4)
    bw, bh = max(1, (w+3)//4), max(1, (h+3)//4)
    pos = 0
    for by in range(bh):
        for bx in range(bw):
            if pos + 16 > len(data): break
            alpha_bits = struct.unpack_from('<Q', data, pos)[0]
            c0, c1    = struct.unpack_from('<HH', data, pos+8)
            color_bits = struct.unpack_from('<I', data, pos+12)[0]
            pos += 16
            def rgb565(c):
                r = ((c >> 11) & 0x1F) * 255 // 31
                g = ((c >>  5) & 0x3F) * 255 // 63
                b = ( c        & 0x1F) * 255 // 31
                return r, g, b
            r0,g0,b0 = rgb565(c0); r1,g1,b1 = rgb565(c1)
            palette = [
                (r0,g0,b0), (r1,g1,b1),
                ((2*r0+r1)//3,(2*g0+g1)//3,(2*b0+b1)//3),
                ((r0+2*r1)//3,(g0+2*g1)//3,(b0+2*b1)//3),
            ]
            for row in range(4):
                for col in range(4):
                    px = bx*4+col; py = by*4+row
                    if px >= w or py >= h: continue
                    ci   = (color_bits >> (2*(row*4+col))) & 3
                    a4   = (alpha_bits >> (4*(row*4+col))) & 0xF
                    a    = a4 * 255 // 15
                    r,g,b = palette[ci]
                    off  = (py*w+px)*4
                    out[off:off+4] = (r,g,b,a)
    return bytes(out)


def _decode_dxt5(data: bytes, w: int, h: int) -> bytes:
    """Decode DXT5 block-compressed data to RGBA8888."""
    out = bytearray(w * h * 4)
    bw, bh = max(1, (w+3)//4), max(1, (h+3)//4)
    pos = 0
    for by in range(bh):
        for bx in range(bw):
            if pos + 16 > len(data): break
            a0, a1    = data[pos], data[pos+1]
            abits     = int.from_bytes(data[pos+2:pos+8], 'little')
            c0, c1    = struct.unpack_from('<HH', data, pos+8)
            cbits     = struct.unpack_from('<I', data, pos+12)[0]
            pos += 16
            # Alpha palette
            if a0 > a1:
                ap = [a0, a1,
                      (6*a0+1*a1)//7, (5*a0+2*a1)//7,
                      (4*a0+3*a1)//7, (3*a0+4*a1)//7,
                      (2*a0+5*a1)//7, (1*a0+6*a1)//7]
            else:
                ap = [a0, a1,
                      (4*a0+1*a1)//5, (3*a0+2*a1)//5,
                      (2*a0+3*a1)//5, (1*a0+4*a1)//5,
                      0, 255]
            def rgb565(c):
                r = ((c >> 11) & 0x1F) * 255 // 31
                g = ((c >>  5) & 0x3F) * 255 // 63
                b = ( c        & 0x1F) * 255 // 31
                return r, g, b
            r0,g0,b0 = rgb565(c0); r1,g1,b1 = rgb565(c1)
            cp = [(r0,g0,b0),(r1,g1,b1),
                  ((2*r0+r1)//3,(2*g0+g1)//3,(2*b0+b1)//3),
                  ((r0+2*r1)//3,(g0+2*g1)//3,(b0+2*b1)//3)]
            for row in range(4):
                for col in range(4):
                    px = bx*4+col; py = by*4+row
                    if px >= w or py >= h: continue
                    bi = row*4+col
                    ai = (abits >> (3*bi)) & 7
                    ci = (cbits >> (2*bi)) & 3
                    a    = ap[ai]
                    r,g,b = cp[ci]
                    off = (py*w+px)*4
                    out[off:off+4] = (r,g,b,a)
    return bytes(out)


def _decode_rgba8888(data: bytes, w: int, h: int) -> bytes:
    """Convert ARGB8888 (VC storage order) to RGBA8888 (QImage order)."""
    out = bytearray(w * h * 4)
    for i in range(w * h):
        if i*4+4 > len(data): break
        b, g, r, a = data[i*4], data[i*4+1], data[i*4+2], data[i*4+3]
        out[i*4:i*4+4] = (r, g, b, a)
    return bytes(out)


def _decode_rgb888(data: bytes, w: int, h: int) -> bytes:
    """Convert BGR888 to RGBA8888."""
    out = bytearray(w * h * 4)
    for i in range(w * h):
        if i*3+3 > len(data): break
        b, g, r = data[i*3], data[i*3+1], data[i*3+2]
        out[i*4:i*4+4] = (r, g, b, 255)
    return bytes(out)


def _decode_rgb565(data: bytes, w: int, h: int) -> bytes:
    out = bytearray(w * h * 4)
    for i in range(w * h):
        if i*2+2 > len(data): break
        c = struct.unpack_from('<H', data, i*2)[0]
        r = ((c >> 11) & 0x1F) * 255 // 31
        g = ((c >>  5) & 0x3F) * 255 // 63
        b = ( c        & 0x1F) * 255 // 31
        out[i*4:i*4+4] = (r, g, b, 255)
    return bytes(out)


def _parse_native_texture(data: bytes, base: int) -> Optional[dict]:
    """Parse one NativeTexture (chunk type 0x15).
    Handles both SA (d3d_format FourCC) and VC/III (has_alpha + mip data header) layouts."""
    try:
        pos = base
        ct, cs, cv, pos = _read_chunk(data, pos)
        if ct != RW_TEXTURE_NATIVE:
            return None

        st, ss, sv, pos = _read_chunk(data, pos)
        if st != RW_STRUCT:
            return None

        platform_id = struct.unpack_from('<I', data, pos)[0]
        pos += 4
        pos += 4  # filter/wrap flags

        tex_name  = data[pos:pos+32].split(b'\x00')[0].decode('ascii','replace').strip()
        pos += 32
        mask_name = data[pos:pos+32].split(b'\x00')[0].decode('ascii','replace').strip()
        pos += 32

        raster_format = struct.unpack_from('<I', data, pos)[0]
        pos += 4

        # Next 4 bytes: SA = D3D FourCC, VC/III = has_alpha bool (0 or 1)
        d3d_or_alpha = struct.unpack_from('<I', data, pos)[0]
        pos += 4

        w         = struct.unpack_from('<H', data, pos)[0]
        h         = struct.unpack_from('<H', data, pos+2)[0]
        depth     = data[pos+4]
        mip_count = data[pos+5]
        raster_type = data[pos+6]
        flags     = data[pos+7]
        pos += 8

        if w == 0 or h == 0 or w > 4096 or h > 4096:
            return None

        # Determine if this is SA (FourCC) or VC/III (has_alpha) format
        known_fourccs = (D3D_DXT1, D3D_DXT2, D3D_DXT3, D3D_DXT4, D3D_DXT5)
        is_sa_format  = d3d_or_alpha in known_fourccs

        fmt  = 'UNKNOWN'
        rgba = None

        if is_sa_format:
            # SA: d3d_or_alpha IS the FourCC
            if d3d_or_alpha == D3D_DXT1:
                fmt  = 'DXT1'
                size = max(1,(w+3)//4)*max(1,(h+3)//4)*8
                if pos+size <= len(data): rgba = _decode_dxt1(data[pos:pos+size], w, h)
            elif d3d_or_alpha in (D3D_DXT2, D3D_DXT3):
                fmt  = 'DXT3'
                size = max(1,(w+3)//4)*max(1,(h+3)//4)*16
                if pos+size <= len(data): rgba = _decode_dxt3(data[pos:pos+size], w, h)
            elif d3d_or_alpha in (D3D_DXT4, D3D_DXT5):
                fmt  = 'DXT5'
                size = max(1,(w+3)//4)*max(1,(h+3)//4)*16
                if pos+size <= len(data): rgba = _decode_dxt5(data[pos:pos+size], w, h)
        else:
            # VC/III: compression signalled by raster_format flags or mip data header
            # Each mip level is preceded by a 4-byte size field
            if mip_count > 0 and pos+4 <= len(data):
                mip_size = struct.unpack_from('<I', data, pos)[0]
                pos += 4
                mip_data = data[pos:pos+mip_size] if pos+mip_size <= len(data) else b''
            else:
                mip_size = 0
                mip_data = b''

            rf_base = raster_format & 0x0F00
            if rf_base == RASTER_8888:
                fmt = 'RGBA32'
                if mip_size == w*h*4: rgba = _decode_rgba8888(mip_data, w, h)
            elif rf_base == RASTER_888:
                fmt = 'RGB24'
                if mip_size == w*h*3: rgba = _decode_rgb888(mip_data, w, h)
            elif rf_base == RASTER_565:
                fmt = 'RGB565'
                if mip_size == w*h*2: rgba = _decode_rgb565(mip_data, w, h)
            elif rf_base == RASTER_1555:
                fmt = 'ARGB1555'
                # treat as RGB565 approximation
                if mip_size == w*h*2: rgba = _decode_rgb565(mip_data, w, h)
            else:
                # Try DXT from mip data size signature
                dxt1_size = max(1,(w+3)//4)*max(1,(h+3)//4)*8
                dxt5_size = max(1,(w+3)//4)*max(1,(h+3)//4)*16
                if mip_size == dxt1_size:
                    fmt = 'DXT1'; rgba = _decode_dxt1(mip_data[:mip_size], w, h)
                elif mip_size == dxt5_size:
                    fmt = 'DXT5'; rgba = _decode_dxt5(mip_data[:mip_size], w, h)

        if rgba is None:
            return None

        return {
            'name':      tex_name,
            'mask':      mask_name,
            'width':     w,
            'height':    h,
            'format':    fmt,
            'rgba_data': rgba,
            'mip_count': mip_count,
            'platform':  platform_id,
        }
    except Exception as e:
        print(f"txd_parser: _parse_native_texture error: {e}")
        return None


def parse_txd(data: bytes) -> List[dict]:
    """
    Parse a GTA PC TXD file (VC / III / SA).
    Returns a list of texture dicts with rgba_data decoded to RGBA8888.
    """
    textures = []
    try:
        if len(data) < 12:
            return textures
        ct, cs, cv, pos = _read_chunk(data, 0)
        if ct != RW_TEXTURE_DICT:
            return textures

        # Dict struct: texture count
        st, ss, sv, pos = _read_chunk(data, pos)
        if st != RW_STRUCT or ss < 2:
            return textures
        tex_count = struct.unpack_from('<H', data, pos)[0]
        pos += ss

        if tex_count == 0 or tex_count > 4096:
            return textures

        # Parse each NativeTexture
        for _ in range(tex_count):
            if pos + 12 > len(data):
                break
            ct, cs, cv, _ = _read_chunk(data, pos)
            if ct == RW_TEXTURE_NATIVE:
                tex = _parse_native_texture(data, pos)
                if tex:
                    textures.append(tex)
            pos += 12 + cs

    except Exception as e:
        print(f"txd_parser: parse_txd error: {e}")
    return textures


def load_txd(path: str) -> List[dict]:
    """Load and parse a TXD file from disk."""
    try:
        with open(path, 'rb') as f:
            return parse_txd(f.read())
    except Exception as e:
        print(f"txd_parser: load_txd({path}) error: {e}")
        return []
