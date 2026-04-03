#this belongs in apps/methods/txd_ps2_parser.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6 - GTA PS2 TXD Parser
"""
GTA III/VC PS2 native TXD parser.

PS2 TXD uses RenderWare chunk 0x15 (NativeTexture) with:
  platform = "PS2\0"
  header struct: width, height, depth (bpp), ...
  GS data: GIF IMAGE packets containing pixel and palette data

Supported formats:
  PSMT8  (8bpp, 256-colour palette)
  PSMT4  (4bpp, 16-colour palette)
  PSMCT32 (32-bit RGBA)
  PSMCT16 (16-bit RGBA1555)

References:
  https://gtamods.com/wiki/TXD_(GTA_III_-_GTA_VC)
  Community research / DragonFF
"""

import struct
from typing import List, Optional, Dict


def parse_ps2_txd(data: bytes) -> List[Dict]:
    """Parse a GTA PS2 TXD file. Returns list of texture dicts."""
    textures = []
    if len(data) < 12:
        return textures
    ct, sz, lib = struct.unpack('<III', data[:12])
    if ct != 0x16:  # not a TextureDict
        return textures
    pos = 12
    end = 12 + sz
    while pos < end - 12:
        ct2, sz2, _ = struct.unpack('<III', data[pos:pos+12])
        if ct2 == 0x15:  # NativeTexture
            tex = _parse_native_tex(data, pos, pos + 12 + sz2)
            if tex:
                textures.append(tex)
        pos += 12 + sz2
    return textures


def _parse_native_tex(data: bytes, start: int, end: int) -> Optional[Dict]:
    pos = start + 12
    tex = {'platform': None, 'name': '', 'mask': '',
            'width': 0, 'height': 0, 'depth': 0,
            'format': '', 'pixels': None, 'palette': None}
    str_count = 0
    while pos < end - 12:
        ct, sz, _ = struct.unpack('<III', data[pos:pos+12])
        payload = data[pos+12:pos+12+sz]
        if ct == 0x01 and sz == 8:        # platform ident
            if payload[:4] == b'PS2\x00':
                tex['platform'] = 'PS2'
        elif ct == 0x02:                   # string (name / mask)
            s = payload.split(b'\x00')[0].decode('ascii', errors='replace')
            if str_count == 0:
                tex['name'] = s
            elif str_count == 1:
                tex['mask'] = s
            str_count += 1
        elif ct == 0x01 and sz > 100:      # GS data container
            result = _parse_gs_container(payload)
            if result:
                tex.update(result)
        pos += 12 + sz
    if not tex['name'] or tex['platform'] != 'PS2':
        return None
    return tex


def _parse_gs_container(payload: bytes) -> Optional[Dict]:
    """Extract width/height/depth from 64-byte header + pixel/palette from GIF packets."""
    pos = 0
    header = None
    gs_data = None
    while pos < len(payload) - 12:
        ct, sz, _ = struct.unpack('<III', payload[pos:pos+12])
        if ct == 0x01:
            chunk = payload[pos+12:pos+12+sz]
            if sz == 64 and header is None:
                header = chunk
            elif sz > 100:
                gs_data = chunk
        pos += 12 + sz
    if header is None or gs_data is None:
        return None
    w = struct.unpack('<I', header[0:4])[0]
    h = struct.unpack('<I', header[4:8])[0]
    d = struct.unpack('<I', header[8:12])[0]
    if w == 0 or h == 0 or d not in (4, 8, 16, 32):
        return None
    # Collect all IMAGE GIF blocks (FLG=2) — 16-byte aligned scan only
    image_blocks = []
    off = 0
    while off <= len(gs_data) - 16:
        qw0 = struct.unpack('<Q', gs_data[off:off+8])[0]
        if ((qw0 >> 58) & 3) == 2:  # FLG=IMAGE
            nloop = qw0 & 0x7FFF
            bd = nloop * 16
            if bd > 0 and off + 16 + bd <= len(gs_data):
                image_blocks.append(gs_data[off+16:off+16+bd])
            off += 16 + max(bd, 0)
        else:
            off += 16  # GIF tags are always 16-byte aligned
    pixel_sz = w * h * d // 8
    pal_sz = (16 if d == 4 else 256) * 4
    pixels = None
    palette = None
    for block in image_blocks:
        sz_b = len(block)
        if pixels is None and sz_b >= pixel_sz:
            if sz_b == pal_sz == pixel_sz:
                # Same size: first block = pixels (order-based)
                pixels = block[:pixel_sz]
            elif sz_b != pal_sz:
                pixels = block[:pixel_sz]
        elif palette is None and sz_b >= pal_sz:
            palette = block[:pal_sz]
    fmt = f'PSMT{d}' if d in (4, 8) else f'PSMCT{d}'
    return {'width': w, 'height': h, 'depth': d,
             'format': fmt, 'pixels': pixels, 'palette': palette}


def ps2_tex_to_rgba(tex: Dict) -> Optional[bytes]:
    """Convert a parsed PS2 texture dict to raw RGBA bytes (width*height*4)."""
    w, h, d = tex['width'], tex['height'], tex['depth']
    pixels = tex.get('pixels')
    palette = tex.get('palette')
    if not pixels or w <= 0 or h <= 0:
        return None
    out = bytearray(w * h * 4)
    if d == 8 and palette and len(palette) >= 1024:
        for i in range(min(len(pixels), w * h)):
            idx = pixels[i]
            r,g,b,a = palette[idx*4], palette[idx*4+1], palette[idx*4+2], palette[idx*4+3]
            out[i*4:i*4+4] = [r, g, b, min(a*2, 255)]
    elif d == 4 and palette and len(palette) >= 64:
        # PS2 PSMT4: low nibble = even pixel, high nibble = odd pixel
        for i in range(min(len(pixels)*2, w*h)):
            byte = pixels[i//2]
            idx = (byte & 0xF) if (i % 2 == 0) else ((byte >> 4) & 0xF)
            r,g,b,a = palette[idx*4], palette[idx*4+1], palette[idx*4+2], palette[idx*4+3]
            out[i*4:i*4+4] = [r, g, b, min(a*2, 255)]
    elif d == 32:
        for i in range(min(len(pixels)//4, w*h)):
            r,g,b,a = pixels[i*4], pixels[i*4+1], pixels[i*4+2], pixels[i*4+3]
            out[i*4:i*4+4] = [r, g, b, min(a*2, 255)]
    elif d == 16:
        for i in range(min(len(pixels)//2, w*h)):
            v = struct.unpack_from('<H', pixels, i*2)[0]
            r=(v&0x1F)*255//31; g=((v>>5)&0x1F)*255//31; b=((v>>10)&0x1F)*255//31
            out[i*4:i*4+4] = [r, g, b, 255 if (v>>15) else 0]
    else:
        return None
    return bytes(out)


def detect_ps2_txd(data: bytes) -> bool:
    """Return True if data looks like a PS2 TXD file."""
    if len(data) < 16:
        return False
    ct, sz, lib = struct.unpack('<III', data[:12])
    if ct != 0x16:
        return False
    # Check for PS2 platform marker in first NativeTex block
    return b'PS2\x00' in data[12:min(100, len(data))]


__all__ = ['parse_ps2_txd', 'ps2_tex_to_rgba', 'detect_ps2_txd']
