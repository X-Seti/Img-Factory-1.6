#this belongs in apps/methods/txd_ps2_parser.py - Version: 5
# X-Seti - October05 2026 - IMG Factory 1.6 - GTA PS2 TXD Parser
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
  Then:            per mip level: 80-byte GIF header + level data
                   80-byte GIF header + palette data (if palettised)

PS2 alpha: stored 0-128, expanded to 0-255 (multiply × 2, cap at 255).

Supported:
  depth=8,  palette_type=PAL8 (PSMT8,  256-colour)
  depth=4,  palette_type=PAL4 (PSMT4,  16-colour)
  depth=32, no palette         (PSMCT32, RGBA32)

device_id=6  (DEVICE_PS2)  — SA PS2: EFFECTS.TXD, FONTS.TXD
device_id=0  (DEVICE_NONE) — LC/VC PS2: GENERIC.TXD, PARTICLE.TXD
Both route to this parser when platform_id == "PS2\\0".
"""

# Level header: GIFtag, TRXPOS, TRXREG, TRXDIR, IMAGE GIFtag
# TRXREG size vs data size gives the upload format:
# 4 bytes/texel PSMCT32: PAL8 swizzled, texels (2*tw, 2*th)
# 2 bytes/texel PSMCT16: PAL4 swizzled, texels (2*tw, 2*th)
# 1 or 0.5 bytes/texel: PSMT8/PSMT4 linear, texels (tw, th)
# Level image sits top-left; padding bytes kept on save
# PAL8 CLUT is always stored in CSM1 order

import struct
from typing import List, Optional, Dict

import numpy as np

from apps.methods.txd_platform_psp import (
    decode_palette, encode_indexed, encode_palette,
    pack_indices, ps2_swizzle8_map, unpack_indices)

##Methods list -
# _box_filter
# _level_store
# _level_texels
# _parse_native
# _read_chunk
# _read_levels
# detect_ps2_txd
# parse_ps2_txd
# ps2_level_rgba
# ps2_tex_to_rgba
# rebuild_ps2_chunk


#    RW chunk reader

def _read_chunk(data: bytes, pos: int): #vers 1
    """Read a 12-byte RW chunk header → (type, size, lib, payload_start)."""
    ct, sz, lib = struct.unpack('<III', data[pos:pos+12])
    return ct, sz, lib, pos + 12


#    Mip level layout

def _box_filter(rgba: np.ndarray, w: int, h: int) -> np.ndarray: #vers 1
    """Half-size RGBA (h x w x 4) by 2x2 box average."""
    a = rgba.astype(np.uint32)
    if a.shape[0] % 2:
        a = np.concatenate([a, a[-1:]], 0)
    if a.shape[1] % 2:
        a = np.concatenate([a, a[:, -1:]], 1)
    a = (a[0::2, 0::2] + a[1::2, 0::2] + a[0::2, 1::2] + a[1::2, 1::2] + 2) // 4
    return a[:h, :w].astype(np.uint8)


def _level_store(new: np.ndarray, lv: Dict, depth: int, orig: bytes) -> bytes: #vers 1
    """Write level texels into its stored block, keeping padding."""
    W, H = lv['store_w'], lv['store_h']
    if depth == 32:
        full = np.frombuffer(orig, np.uint8).reshape(H, W, 4).copy()
        full[:lv['height'], :lv['width']] = new.reshape(lv['height'], lv['width'], 4)
        return full.tobytes()
    full = unpack_indices(np.frombuffer(orig, np.uint8), depth)
    smap = ps2_swizzle8_map(W, H) if lv['swizzled'] else None
    if lv['swizzled']:
        full = full[smap]
    full = full.reshape(H, W)
    full[:lv['height'], :lv['width']] = new.reshape(lv['height'], lv['width'])
    full = full.ravel()
    if lv['swizzled']:
        stored = np.empty_like(full)
        stored[smap] = full
        full = stored
    return pack_indices(full, depth).tobytes()


def _level_texels(raw: bytes, lv: Dict, depth: int) -> np.ndarray: #vers 1
    """Stored level block to linear indices (or RGBA for depth 32)."""
    W, H = lv['store_w'], lv['store_h']
    b = np.frombuffer(raw, np.uint8)
    if depth == 32:
        return b.reshape(H, W, 4)[:lv['height'], :lv['width']].reshape(-1, 4).copy()
    idx = unpack_indices(b, depth)
    if lv['swizzled']:
        idx = idx[ps2_swizzle8_map(W, H)]
    return idx.reshape(H, W)[:lv['height'], :lv['width']].ravel().copy()


def _read_levels(data: bytes, pos: int, w: int, h: int, depth: int,
                 pix_sz: int, chunk_pos: int) -> List[Dict]: #vers 3
    """Walk per-level GIF headers; offsets relative to chunk_pos."""
    levels, off, lw, lh = [], 0, w, h
    lo, hi = struct.unpack_from('<QQ', data, pos)
    if (hi != 0x0E or (lo & 0x7FFF) != 3) and pix_sz == w * h * depth // 8:
        # header-less raster (Manhunt): one linear level
        return [{'level': 0, 'width': w, 'height': h, 'offset': pos - chunk_pos,
                 'length': pix_sz, 'swizzled': False, 'store_w': w, 'store_h': h}]
    while off < pix_sz:
        lo, hi = struct.unpack_from('<QQ', data, pos + off)
        if hi != 0x0E or (lo & 0x7FFF) != 3:
            if levels:          # filler after the last level, kept as-is
                return levels
            raise ValueError(f"PS2 level {len(levels)}: bad GIF header at +{off}")
        trx = struct.unpack_from('<Q', data, pos + off + 32)[0]
        tw, th = trx & 0xFFF, (trx >> 32) & 0xFFF
        n = (struct.unpack_from('<Q', data, pos + off + 64)[0] & 0x7FFF) * 16
        texels = tw * th
        if depth == 32 and n == texels * 4:
            swz, sw, sh = False, tw, th
        elif depth == 8 and n == texels * 4:
            swz, sw, sh = True, 2 * tw, 2 * th
        elif depth == 4 and n == texels * 2:
            swz, sw, sh = True, 2 * tw, 2 * th
        elif depth == 8 and n == texels:
            swz, sw, sh = False, tw, th
        elif depth == 4 and n * 2 == texels:
            swz, sw, sh = False, tw, th
        else:
            raise ValueError(f"PS2 level {len(levels)}: {n} bytes for {tw}x{th} "
                             f"transfer at depth {depth}")
        if sw < lw or sh < lh:
            raise ValueError(f"PS2 level {len(levels)}: block {sw}x{sh} < {lw}x{lh}")
        levels.append({'level': len(levels), 'width': lw, 'height': lh,
                       'offset': pos + off + 80 - chunk_pos, 'length': n,
                       'swizzled': swz, 'store_w': sw, 'store_h': sh})
        off += 80 + n
        lw, lh = max(1, lw // 2), max(1, lh // 2)
    if off != pix_sz:
        raise ValueError(f"PS2 levels end at {off}, pixel block is {pix_sz}")
    return levels


#    Main parsers

def detect_ps2_txd(data: bytes) -> bool: #vers 1
    """Return True if data starts with a TextureDict containing PS2\\0 textures."""
    if len(data) < 32:
        return False
    ct = struct.unpack('<I', data[:4])[0]
    if ct != 0x16:   # not TextureDict
        return False
    return b'PS2\x00' in data[12:min(200, len(data))]


def _parse_native(data: bytes, chunk_pos: int, device_id: int = 0) -> Dict: #vers 2
    """One PS2 NativeTexture; records level/palette offsets for saves."""
    ct3, sz3, lib3, p3 = _read_chunk(data, chunk_pos)
    nt_end = p3 + sz3
    tex: Dict = {
        'name': '', 'mask': '', 'width': 0, 'height': 0,
        'depth': 0, 'raster_format_flags': 0,
        'pixels': None, 'palette': None,
        'pixels_size': 0, 'palette_size': 0,
        'device_id': device_id, 'platform_id': 0,
        'mip_levels': [],
    }
    pos = p3    # walk inside NativeTex

    #    Struct(8): platform_id + filter + uv
    if pos < nt_end - 12:
        ct4, sz4, _, p4 = _read_chunk(data, pos)
        if ct4 == 0x01 and sz4 == 8:
            tex['platform_id'] = struct.unpack('<I', data[p4:p4+4])[0]
        pos = p4 + sz4

    #    String chunks: name, mask
    for key in ('name', 'mask'):
        if pos >= nt_end - 12: break
        ct4, sz4, _, p4 = _read_chunk(data, pos)
        if ct4 == 0x02:
            tex[key] = data[p4:p4+sz4].split(b'\x00')[0].decode('ascii', 'replace')
        pos = p4 + sz4

    #    Native chunk (outer wrapper) — step INTO
    if pos < nt_end - 12:
        ct4, sz4, _, p4 = _read_chunk(data, pos)
        pos = p4   # step in: holds Raster + Texture chunks

    #    Raster chunk: w/h/depth/flags + GS registers + sizes
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
            tex['tbp0']                = int(tex0) & 0x3FFF
        pos = p4 + sz4

    #    Texture chunk (inner) — step INTO pixel/palette data
    if pos < nt_end - 12:
        ct4, sz4, _, p4 = _read_chunk(data, pos)
        pos = p4

    #    Pixel + palette data
    raster_type  = (tex['raster_format_flags'] >> 8) & 0xF   # 5 = RASTER_8888
    palette_type = (tex['raster_format_flags'] >> 13) & 0x3  # 1=PAL8 2=PAL4
    w, h, depth  = tex['width'], tex['height'], tex['depth']
    pix_sz       = tex['pixels_size']
    pal_sz       = tex['palette_size']

    if raster_type == 5 and ((pal_sz > 0 and depth in (4, 8)) or depth == 32):
        levels = _read_levels(data, pos, w, h, depth, pix_sz, chunk_pos)
        for lv in levels:
            o = chunk_pos + lv['offset']
            lin = _level_texels(data[o:o + lv['length']], lv, depth)
            if depth == 32:
                lv['pixels'] = lin.tobytes()
            else:
                lv['pixels'] = pack_indices(lin, depth).tobytes()
        tex['mip_levels'] = levels
        tex['pixels'] = levels[0]['pixels']
        tex['_pix_off'], tex['_pix_len'] = levels[0]['offset'], levels[0]['length']
        tex['_swizzled'] = levels[0]['swizzled']
        if depth in (4, 8):
            pal_len = 1024 if palette_type == 1 else 64
            if palette_type not in (1, 2):
                raise ValueError(f"'{tex['name']}': palette type {palette_type} unknown")
            tex['_pal_off'] = pos + pix_sz + 80 - chunk_pos
            tex['_pal_len'] = pal_len
            po = chunk_pos + tex['_pal_off']
            tex['palette'] = decode_palette(data[po:po + pal_len], ps2_alpha=True,
                                            csm1=(palette_type == 1)).tobytes()

    return tex


def parse_ps2_txd(data: bytes) -> List[Dict]: #vers 2
    """Parse a GTA PS2 TXD into texture dicts (with mip_levels)."""
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
        if ct3 == 0x15:
            results.append(_parse_native(data, pos, device_id))
        pos = p3 + sz3

    return results


def ps2_level_rgba(tex: Dict, level: int) -> bytes: #vers 1
    """RGBA bytes of one mip level from a parsed texture."""
    lv = tex['mip_levels'][level]
    sub = dict(tex, width=lv['width'], height=lv['height'], pixels=lv['pixels'])
    rgba = ps2_tex_to_rgba(sub)
    if rgba is None:
        raise ValueError(f"'{tex.get('name')}': level {level} can't be decoded")
    return rgba


def ps2_tex_to_rgba(tex: Dict) -> Optional[bytes]: #vers 2
    """Parsed PS2 texture (level 0) to RGBA bytes; None if unsupported."""
    w, h, d = tex['width'], tex['height'], tex['depth']
    pixels  = tex.get('pixels')
    palette = tex.get('palette')
    if not pixels or w <= 0 or h <= 0:
        return None
    raw = np.frombuffer(pixels, np.uint8)
    if d in (4, 8) and palette and len(palette) >= (4 << d):
        pal = np.frombuffer(palette, np.uint8).reshape(-1, 4)
        idx = unpack_indices(raw, d)[:w * h]
        return np.ascontiguousarray(pal[idx]).tobytes()
    if d == 32:
        a = raw[:w * h * 4].reshape(-1, 4).copy()
        a[:, 3] = np.minimum(255, a[:, 3].astype(np.int32) * 2)
        return a.tobytes()
    return None


def rebuild_ps2_chunk(chunk: bytes, tex: Dict) -> bytes: #vers 2
    """PS2 NativeTexture chunk with every mip level rewritten in place."""
    lay = _parse_native(chunk, 0)
    w, h, depth = lay['width'], lay['height'], lay['depth']
    name = tex.get('name')
    if (int(tex.get('width') or 0), int(tex.get('height') or 0)) != (w, h):
        raise ValueError(f"'{name}': PS2 textures keep their size ({w}x{h})")
    if '_pix_off' not in lay:
        raise ValueError(f"'{name}': PS2 raster type not supported for saving")
    rgba = bytes(tex.get('rgba_data') or b'')
    if len(rgba) != w * h * 4:
        raise ValueError(f"'{name}' needs {w}x{h} RGBA pixel data")
    levels = lay['mip_levels']
    orig = [np.frombuffer(ps2_level_rgba(lay, i), np.uint8) for i in range(len(levels))]
    given = {}
    for m in tex.get('mipmap_levels') or []:
        i, data = int(m.get('level', -1)), m.get('rgba_data') or b''
        if 0 < i < len(levels) and len(data) == levels[i]['width'] * levels[i]['height'] * 4:
            given[i] = np.frombuffer(bytes(data), np.uint8)
    keep_low = not given and np.array_equal(np.frombuffer(rgba, np.uint8), orig[0])
    targets = [np.frombuffer(rgba, np.uint8)]
    for i in range(1, len(levels)):
        lv, pv = levels[i], levels[i - 1]
        if i in given:
            targets.append(given[i])
        elif keep_low:
            targets.append(orig[i])
        else:
            prev = targets[-1].reshape(pv['height'], pv['width'], 4)
            targets.append(_box_filter(prev, lv['width'], lv['height']).ravel())
    out = bytearray(chunk)
    if depth == 32:
        for lv, t in zip(levels, targets):
            a = t.reshape(-1, 4).copy()
            a[:, 3] = (a[:, 3].astype(np.uint16) + 1) // 2
            o = lv['offset']
            out[o:o + lv['length']] = _level_store(a, lv, 32, chunk[o:o + lv['length']])
        return bytes(out)
    old_pal = np.frombuffer(lay['palette'], np.uint8).reshape(-1, 4)
    old_idx = np.concatenate([unpack_indices(np.frombuffer(lv['pixels'], np.uint8), depth)
                              [:lv['width'] * lv['height']] for lv in levels])
    allpx = np.concatenate(targets).tobytes()
    idx, new_pal = encode_indexed(allpx, 1, len(allpx) // 4, old_idx, old_pal, 1 << depth)
    start = 0
    for lv in levels:
        n = lv['width'] * lv['height']
        o = lv['offset']
        out[o:o + lv['length']] = _level_store(idx[start:start + n], lv, depth,
                                               chunk[o:o + lv['length']])
        start += n
    if new_pal is not None:
        pb = encode_palette(new_pal, ps2_alpha=True, csm1=(depth == 8))
        out[lay['_pal_off']:lay['_pal_off'] + len(pb)] = pb
    return bytes(out)


__all__ = ['detect_ps2_txd', 'parse_ps2_txd', 'ps2_level_rgba', 'ps2_tex_to_rgba',
           'rebuild_ps2_chunk']
