#this belongs in apps/methods/txd_ps2_parser.py - Version: 2
# X-Seti - Apr 2026 - IMG Factory 1.6 - GTA PS2 TXD Parser
"""
GTA PS2 TXD parser — rewritten using DragonFF's NativePS2Texture approach.

DragonFF reference: gta-blender-scripts/dff/native_ps2.py
                    gta-blender-scripts/dff/txd.py

Structure per NativeTexture (0x15):
  Struct(8):       platform_id("PS2\\0") + filter_mode(2) + uv_addressing(2)
  String chunk:    texture name
  String chunk:    mask name
  Native chunk:    outer container (no payload — step into)
  Raster chunk:    width(4)+height(4)+depth(4)+raster_format_flags(4)+
                   tex0_gs_reg(8)+tex1_gs_reg(8)+miptbp1(8)+miptbp2(8)+
                   pixels_size(4)+palette_size(4)+gpu_data_aligned(4)+sky_mip(4)
  Texture chunk:   inner container (no payload — step into)
  Then:            80-byte GIF header + pixel data
                   80-byte GIF header + palette data (if palettised)

Unswizzle algorithms are taken verbatim from DragonFF's NativePS2Texture:
  unswizzle8()          — GS page/column/byte algorithm
  unswizzle4()          — unpack nibbles → unswizzle8 → repack
  unswizzle_palette()   — CLUT reorder for 256-entry palette

PS2 alpha: stored 0-128, expanded to 0-255 (multiply × 2, cap at 255).

Supported:
  depth=8,  palette_type=PAL8 (PSMT8,  256-colour)
  depth=4,  palette_type=PAL4 (PSMT4,  16-colour)
  depth=32, no palette         (PSMCT32, RGBA32)

device_id=6  (DEVICE_PS2)  — SA PS2: EFFECTS.TXD, FONTS.TXD
device_id=0  (DEVICE_NONE) — LC/VC PS2: GENERIC.TXD, PARTICLE.TXD
Both route to this parser when platform_id == "PS2\\0".
"""

import struct
from typing import List, Optional, Dict


# ── RW chunk reader ────────────────────────────────────────────────────────────

def _read_chunk(data: bytes, pos: int):
    """Read a 12-byte RW chunk header → (type, size, lib, payload_start)."""
    ct, sz, lib = struct.unpack('<III', data[pos:pos+12])
    return ct, sz, lib, pos + 12


# ── DragonFF unswizzle algorithms (verbatim) ──────────────────────────────────

def _unswizzle8(data: bytes, width: int, height: int) -> bytes:
    """GS VRAM unswizzle for PSMT8 (8bpp palette-indexed)."""
    res = bytearray(width * height)
    for y in range(height):
        block_y            = (y & ~0xf) * width
        posY               = (((y & ~3) >> 1) + (y & 1)) & 0x7
        swap_selector      = (((y + 2) >> 2) & 0x1) * 4
        base_col_loc       = posY * width * 2
        for x in range(width):
            block_x        = (x & ~0xf) * 2
            col_loc        = base_col_loc + ((x + swap_selector) & 0x7) * 4
            byte_num       = ((y >> 1) & 1) + ((x >> 2) & 2)
            swizzle_id     = block_y + block_x + col_loc + byte_num
            res[y * width + x] = data[swizzle_id]
    return bytes(res)


def _unswizzle4(data: bytes, width: int, height: int) -> bytes:
    """GS VRAM unswizzle for PSMT4 (4bpp): unpack nibbles → unswizzle8 → repack."""
    pixels = bytearray(width * height)
    for i in range(width * height // 2):
        b = data[i]
        pixels[i * 2]     = b & 0xF
        pixels[i * 2 + 1] = (b >> 4) & 0xF
    pixels = _unswizzle8(pixels, width, height)
    res = bytearray(width * height // 2)
    for i in range(width * height // 2):
        res[i] = (pixels[i * 2 + 1] << 4) | pixels[i * 2]
    return bytes(res)


def _unswizzle_palette(data: bytes) -> bytes:
    """Reorder a 256-entry (1024-byte) GS CLUT from upload order to index order."""
    palette = bytearray(1024)
    for p in range(256):
        pos_l = ((p & 231) | ((p & 8) << 1) | ((p & 16) >> 1)) * 4
        palette[pos_l:pos_l + 4] = data[p * 4:p * 4 + 4]
    return bytes(palette)


def _read_palette(data: bytes, pos: int, size: int) -> bytes:
    """Read palette bytes and expand PS2 alpha 0-128 → 0-255."""
    raw = data[pos:pos + size]
    out = bytearray(size)
    for i in range(0, size, 4):
        r, g, b, a = raw[i:i+4]
        out[i:i+4] = r, g, b, min(a * 2, 255)
    return bytes(out)


# ── Main parsers ───────────────────────────────────────────────────────────────

def detect_ps2_txd(data: bytes) -> bool:
    """Return True if data starts with a TextureDict containing PS2\\0 textures."""
    if len(data) < 32:
        return False
    ct = struct.unpack('<I', data[:4])[0]
    if ct != 0x16:   # not TextureDict
        return False
    return b'PS2\x00' in data[12:min(200, len(data))]


def parse_ps2_txd(data: bytes) -> List[Dict]:
    """
    Parse a GTA PS2 TXD file.

    Returns a list of texture dicts with keys:
      name, mask, width, height, depth, raster_format_flags,
      pixels (raw bytes), palette (RGBA bytes, alpha already expanded),
      pixels_size, palette_size, device_id, platform_id
    """
    results = []
    if len(data) < 28:
        return results

    ct, sz, lib, pos = _read_chunk(data, 0)
    if ct != 0x16:
        return results
    td_end = pos + sz

    # TexDict struct: tex_count(u16) + device_id(u16)
    ct2, sz2, lib2, p2 = _read_chunk(data, pos)
    if ct2 != 0x01 or sz2 < 4:
        return results
    tex_count, device_id = struct.unpack('<HH', data[p2:p2+4])
    pos = p2 + sz2

    for _ in range(tex_count):
        if pos >= td_end - 12:
            break

        ct3, sz3, lib3, p3 = _read_chunk(data, pos)
        if ct3 != 0x15:    # NativeTexture
            pos = p3 + sz3; continue
        nt_end = p3 + sz3

        tex: Dict = {
            'name': '', 'mask': '', 'width': 0, 'height': 0,
            'depth': 0, 'raster_format_flags': 0,
            'pixels': None, 'palette': None,
            'pixels_size': 0, 'palette_size': 0,
            'device_id': device_id, 'platform_id': 0,
        }
        pos = p3    # walk inside NativeTex

        # ── Struct(8): platform_id + filter + uv ──────────────────────────
        if pos < nt_end - 12:
            ct4, sz4, _, p4 = _read_chunk(data, pos)
            if ct4 == 0x01 and sz4 == 8:
                tex['platform_id'] = struct.unpack('<I', data[p4:p4+4])[0]
            pos = p4 + sz4

        # ── String chunks: name, mask ─────────────────────────────────────
        for key in ('name', 'mask'):
            if pos >= nt_end - 12: break
            ct4, sz4, _, p4 = _read_chunk(data, pos)
            if ct4 == 0x02:
                tex[key] = data[p4:p4+sz4].split(b'\x00')[0].decode('ascii', 'replace')
            pos = p4 + sz4

        # ── Native chunk (outer wrapper) — step INTO ──────────────────────
        if pos < nt_end - 12:
            ct4, sz4, _, p4 = _read_chunk(data, pos)
            pos = p4   # do NOT skip payload — it contains Raster + Texture chunks

        # ── Raster chunk: w/h/depth/flags + GS registers + sizes ─────────
        if pos < nt_end - 12:
            ct4, sz4, _, p4 = _read_chunk(data, pos)
            FMT = '<4I4Q4I'
            FS  = struct.calcsize(FMT)   # 16+32+16 = 64 bytes
            if sz4 >= FS:
                (w, h, depth, raster_fmt,
                 tex0, tex1, mip1, mip2,
                 pix_sz, pal_sz, gpu_sz, sky_mip
                 ) = struct.unpack_from(FMT, data, p4)
                tex['width']               = w
                tex['height']              = h
                tex['depth']               = depth
                tex['raster_format_flags'] = raster_fmt
                tex['pixels_size']         = pix_sz
                tex['palette_size']        = pal_sz
            pos = p4 + sz4

        # ── Texture chunk (inner) — step INTO pixel/palette data ──────────
        if pos < nt_end - 12:
            ct4, sz4, _, p4 = _read_chunk(data, pos)
            pos = p4

        # ── Pixel + palette data ──────────────────────────────────────────
        raster_type  = (tex['raster_format_flags'] >> 8) & 0xF   # 5 = RASTER_8888
        palette_type = (tex['raster_format_flags'] >> 13) & 0x3  # 1=PAL8 2=PAL4
        w, h, depth  = tex['width'], tex['height'], tex['depth']
        pix_sz       = tex['pixels_size']
        pal_sz       = tex['palette_size']

        if raster_type == 5 and pal_sz > 0:
            # Palettised PSMT8 or PSMT4
            pix_sz -= 80;  pal_sz -= 80

            pos += 80                                     # skip pixel GIF header
            raw_pixels = data[pos:pos + pix_sz];  pos += pix_sz

            pos += 80                                     # skip palette GIF header
            if palette_type == 1:                         # PAL8 — 256 entries
                palette = _read_palette(data, pos, 1024);  pos += 1024
            elif palette_type == 2:                       # PAL4 — 16 entries
                palette = _read_palette(data, pos, 64);    pos += pal_sz
            else:
                palette = None

            # Unswizzle — only apply when texture is large enough for GS VRAM paging:
            #   PSMT8: GS page = 64px wide → need width >= 64
            #   PSMT4: GS page = 128px wide → need width >= 128
            # Smaller textures are stored linearly (no swizzle applied by game).
            if depth == 8 and palette and w >= 64:
                palette     = _unswizzle_palette(palette)
                raw_pixels  = _unswizzle8(raw_pixels, w, h)
            elif depth == 4 and w >= 128:
                raw_pixels  = _unswizzle4(raw_pixels, w, h)

            tex['pixels']  = raw_pixels
            tex['palette'] = palette

        elif raster_type == 5 and depth == 32:
            # Unpalettised PSMCT32 — raw RGBA32
            tex['pixels'] = data[pos:pos + pix_sz]

        results.append(tex)
        pos = nt_end

    return results


def ps2_tex_to_rgba(tex: Dict) -> Optional[bytes]:
    """
    Convert a parsed PS2 texture dict to raw RGBA bytes (width*height*4).
    Returns None if texture cannot be decoded.
    """
    w, h, d = tex['width'], tex['height'], tex['depth']
    pixels  = tex.get('pixels')
    palette = tex.get('palette')
    if not pixels or w <= 0 or h <= 0:
        return None

    out = bytearray(w * h * 4)

    if d == 8 and palette and len(palette) >= 1024:
        for i in range(min(len(pixels), w * h)):
            idx = pixels[i]
            out[i*4:i*4+4] = palette[idx*4:idx*4+4]

    elif d == 4 and palette and len(palette) >= 64:
        for i in range(min(len(pixels) * 2, w * h)):
            byte = pixels[i // 2]
            idx  = (byte & 0xF) if (i % 2 == 0) else ((byte >> 4) & 0xF)
            out[i*4:i*4+4] = palette[idx*4:idx*4+4]

    elif d == 32:
        for i in range(min(len(pixels) // 4, w * h)):
            r, g, b, a = pixels[i*4], pixels[i*4+1], pixels[i*4+2], pixels[i*4+3]
            out[i*4:i*4+4] = r, g, b, min(a * 2, 255)

    else:
        return None

    return bytes(out)


__all__ = ['detect_ps2_txd', 'parse_ps2_txd', 'ps2_tex_to_rgba']
