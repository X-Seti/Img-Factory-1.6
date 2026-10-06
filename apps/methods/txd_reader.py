#this belongs in apps/methods/txd_reader.py - Version: 3
# X-Seti - October05 2026 - IMG Factory 1.6 - Shared texture file reader

"""
One reader for every texture file type: TXD Workshop, Model, Map and viewers.
"""

##Methods list -
# decompress_dxt
# decompress_raw
# parse_native_texture
# ps2_entry
# read_psp_txd
# read_rw_txd
# read_texture_file
# _rgba_out
# _rw_native_names
# texture_names

import struct
from typing import Callable, Dict, List, Optional, Tuple

import numpy as np

# Files TXD Workshop opens (IMG Factory routes these to it)
TEXTURE_EXTS = ('.txd', '.wtd', '.nft', '.xtx', '.chk')


def decompress_dxt(compressed_data, width, height, format_str): #vers 1
    """DXT1/3/5 to RGBA bytes (PIL DDS path, numpy fallback)."""
    import io
    fmt = 'DXT1' if 'DXT1' in format_str else 'DXT3' if 'DXT3' in format_str else \
        'DXT5' if 'DXT5' in format_str else None
    if fmt is None:
        return compressed_data
    try:
        from PIL import Image
        pitch = max(1, (width + 3) // 4) * (8 if fmt == 'DXT1' else 16)
        hdr = bytearray(128)
        struct.pack_into('<I', hdr, 0, 0x20534444)          # 'DDS '
        struct.pack_into('<I', hdr, 4, 124)
        struct.pack_into('<I', hdr, 8, 0x1 | 0x2 | 0x4 | 0x1000)
        struct.pack_into('<I', hdr, 12, height)
        struct.pack_into('<I', hdr, 16, width)
        struct.pack_into('<I', hdr, 20, pitch * max(1, (height + 3) // 4))
        struct.pack_into('<I', hdr, 28, 1)
        struct.pack_into('<I', hdr, 76, 32)
        struct.pack_into('<I', hdr, 80, 0x4)                # DDPF_FOURCC
        hdr[84:88] = fmt.encode('ascii')
        img = Image.open(io.BytesIO(bytes(hdr) + bytes(compressed_data))).convert('RGBA')
        return bytes(img.tobytes())
    except Exception:
        pass
    try:
        from apps.methods.mobile_texture_decode import decode_dxt, GL_DXT1A, GL_DXT3, GL_DXT5
        enc = {'DXT1': GL_DXT1A, 'DXT3': GL_DXT3, 'DXT5': GL_DXT5}[fmt]
        need = ((width + 3) // 4) * ((height + 3) // 4) * (8 if fmt == 'DXT1' else 16)
        data = bytes(compressed_data).ljust(need, b'\0')
        return np.ascontiguousarray(decode_dxt(data, width, height, enc)).tobytes()
    except Exception:
        return None


def _rgba_out(n_pix: int, rgba: np.ndarray) -> bytes: #vers 1
    """Pad decoded pixels with zeros to the full image size."""
    out = np.zeros((n_pix, 4), np.uint8)
    out[:len(rgba)] = rgba[:n_pix]
    return out.tobytes()


def decompress_raw(data, width, height, format_type, palette=None,
                   palette_entry_fmt='ARGB8888', depth=0, force_opaque=False,
                   palette_is_bgra=True): #vers 1
    """RW uncompressed and palettised formats to RGBA bytes."""
    try:
        n = width * height
        buf = np.frombuffer(bytes(data), np.uint8)

        def u16(k):
            m = min(n, len(buf) // 2)
            return buf[:m * 2].view('<u2').astype(np.int32)

        def rgba(r, g, b, a):
            return np.stack([r, g, b, a], 1).astype(np.uint8)

        if format_type in ('PAL8', 'PAL4'):
            need = 1024 if format_type == 'PAL8' else 64
            if not palette or len(palette) < need:
                return None
            pal = np.frombuffer(bytes(palette), np.uint8)
            pal = pal[:len(pal) // 4 * 4].reshape(-1, 4)
            if palette_is_bgra:
                pal = pal[:, [2, 1, 0, 3]]
            pal = pal.copy()
            if palette_entry_fmt == 'RGB888':
                pal[:, 3] = 255
            if format_type == 'PAL8':
                idx = buf[:n]
            else:
                idx = np.stack([buf >> 4, buf & 15], 1).reshape(-1)[:n]
            ok = idx < len(pal)
            px = np.zeros((len(idx), 4), np.uint8)
            px[ok] = pal[idx[ok]]
            return _rgba_out(n, px)
        if 'ARGB8888' in format_type or 'ARGB32' in format_type:
            px = buf[:min(n, len(buf) // 4) * 4].reshape(-1, 4)[:, [2, 1, 0, 3]].copy()
            if force_opaque:
                px[:, 3] = 255
            return _rgba_out(n, px)
        if 'RGB888' in format_type:
            st = 4 if depth == 32 else 3
            px = buf[:min(n, len(buf) // st) * st].reshape(-1, st)
            return _rgba_out(n, rgba(px[:, 2], px[:, 1], px[:, 0], np.full(len(px), 255)))
        if 'RGB565' in format_type:
            p = u16(0)
            return _rgba_out(n, rgba(((p >> 11) & 31) << 3, ((p >> 5) & 63) << 2,
                                     (p & 31) << 3, np.full(len(p), 255)))
        if 'ARGB1555' in format_type:
            p = u16(0)
            return _rgba_out(n, rgba(((p >> 10) & 31) << 3, ((p >> 5) & 31) << 3,
                                     (p & 31) << 3, np.where(p & 0x8000, 255, 0)))
        if 'ARGB4444' in format_type:
            p = u16(0)
            return _rgba_out(n, rgba(((p >> 8) & 15) * 17, ((p >> 4) & 15) * 17,
                                     (p & 15) * 17, ((p >> 12) & 15) * 17))
        if 'RGB555' in format_type:
            p = u16(0)
            return _rgba_out(n, rgba(((p >> 10) & 31) << 3, ((p >> 5) & 31) << 3,
                                     (p & 31) << 3, np.full(len(p), 255)))
        if 'A8L8' in format_type:
            px = buf[:min(n, len(buf) // 2) * 2].reshape(-1, 2)
            return _rgba_out(n, rgba(px[:, 0], px[:, 0], px[:, 0], px[:, 1]))
        if 'LUM8' in format_type or 'L8' in format_type:
            l = buf[:n]
            return _rgba_out(n, rgba(l, l, l, np.full(len(l), 255)))
        return bytes([128, 128, 128, 255]) * n
    except Exception:
        return None


def parse_native_texture(txd_data, offset, index, rw_version=0x1803FFFF, levels=True, log=None): #vers 1
    """One RW texture native to a workshop dict; levels=False decodes mip 0 only."""

    tex = {
        'name': f'texture_{index}',
        'width': 0,
        'height': 0,
        'depth': 32,
        'format': 'DXT1',
        'has_alpha': False,
        'mipmaps': 1,
        'rgba_data': b'',
        'alpha_mask': b'',              # NEW: Separate grayscale alpha channel
        'compressed_data': b'',
        'original_bgra_data': b'',
        'mipmap_levels': [],
        'bumpmap_data': b'',
        'bumpmap_type': 0,
        'has_bumpmap': False,
        'reflection_map': b'',
        'fresnel_map': b'',
        'has_reflection': False,
        'raster_format_flags': 0
    }

    try:
        # TextureNative structure
        parent_type, parent_size, parent_version = struct.unpack('<III', txd_data[offset:offset+12])

        if parent_type != 0x15:
            return tex

        # Struct section
        struct_offset = offset + 12
        struct_type, struct_size, struct_version = struct.unpack('<III', txd_data[struct_offset:struct_offset+12])

        if struct_type != 0x01:
            return tex

        pos = struct_offset + 12
        struct_end = pos + struct_size

        # Read 88-byte header
        platform_id, filter_mode, uv_addressing = struct.unpack('<I2B', txd_data[pos:pos+6])[:3]
        tex['platform_id'] = platform_id
        tex['filter_flags'] = struct.unpack_from('<I', txd_data, pos + 4)[0]

        #    Xbox (platform_id == 5): delegate to Xbox parser               
        if platform_id == 5:
            try:
                from apps.methods.txd_platform_xbox import parse_xbox_nativetex
                xbox_tex = parse_xbox_nativetex(txd_data, offset, index)
                if xbox_tex:
                    # Decode compressed data to RGBA for display
                    if xbox_tex.get('compressed_data') and not xbox_tex.get('rgba_data'):
                        fmt = xbox_tex.get('format', 'DXT1')
                        w   = xbox_tex.get('width', 0)
                        h   = xbox_tex.get('height', 0)
                        if w > 0 and h > 0 and 'DXT' in fmt:
                            xbox_tex['rgba_data'] = decompress_dxt(
                                xbox_tex['compressed_data'], w, h, fmt)
                    return xbox_tex
            except Exception as _xe:
                print(f"[Xbox TXD] Texture {index}: {_xe}")
            return tex  # return empty rather than crash
        #    End Xbox                                                      

        pos += 8  # Skip padding

        name_bytes = txd_data[pos:pos+32]
        # Use first-null termination (like C strlen) not rstrip — DragonFF does this too
        _null = name_bytes.find(b'\x00')
        tex['name'] = (name_bytes[:_null] if _null >= 0 else name_bytes).decode('ascii', errors='ignore') or f'texture_{index}'
        pos += 32

        mask_bytes = txd_data[pos:pos+32]
        _null2 = mask_bytes.find(b'\x00')
        alpha_name = (mask_bytes[:_null2] if _null2 >= 0 else mask_bytes).decode('ascii', errors='ignore')
        if alpha_name:
            tex['alpha_name'] = alpha_name
            tex['has_alpha'] = True
        pos += 32

        raster_format_flags, d3d_format, width, height, depth, num_levels, raster_type = struct.unpack('<IIHHBBB', txd_data[pos:pos+15])
        tex['width'] = width
        tex['height'] = height
        tex['depth'] = depth
        tex['mipmaps'] = num_levels
        tex['raster_format_flags'] = raster_format_flags

        # Check for bumpmap flag (bit 0x10)
        if raster_format_flags & 0x10:
            tex['has_bumpmap'] = True

        pos += 15

        platform_prop = struct.unpack('<B', txd_data[pos:pos+1])[0]
        pos += 1

        # Format detection - version-aware
        is_pal8     = bool(raster_format_flags & 0x2000)  # FORMAT_EXT_PAL8
        is_pal4     = bool(raster_format_flags & 0x4000)  # FORMAT_EXT_PAL4
        pixel_fmt   = raster_format_flags & 0x0F00  # bits 8-11 only, excludes PAL flags
        is_sa_plus  = (rw_version >= 0x1803FFFF)
        # GTA3/VC: RGBA palette (no swap); SA: BGRA palette (swap B<->R)
        tex['palette_is_bgra'] = is_sa_plus

        raster_pixel_map = {
            0x0100: 'ARGB1555', 0x0200: 'RGB565',
            0x0300: 'ARGB4444', 0x0400: 'LUM8',
            0x0500: 'ARGB8888', 0x0600: 'RGB888',
            0x0A00: 'RGB555',
        }

        # Xbox (platform_id=5): compression byte 0x0C/0x0E/0x10 = DXT1/3/5
        # D3D8 GTA3/VC: platform_prop 1/3/5 = DXT1/3/5
        # SA D3D9: use d3d_format FourCC
        is_xbox = (platform_id == 5)
        if is_pal8:
            tex['format'] = 'PAL8'
        elif is_pal4:
            tex['format'] = 'PAL4'
        elif is_xbox and platform_prop == 0x00:
            # Raw ARGB8888 - use raster_format pixel bits
            tex['format'] = {0x0500:'ARGB8888',0x0600:'RGB888'}.get(pixel_fmt, 'ARGB8888')
        elif is_xbox and platform_prop in (0x0B, 0x0C):
            # 0x0B = LIN_DXT1 (linear/standard), 0x0C = DXT1 (swizzled)
            # Both decode identically via PIL DDS
            tex['format'] = 'DXT1'
        elif is_xbox and platform_prop in (0x0E, 0x0F):
            # 0x0E = DXT3 (swizzled), 0x0F = LIN_DXT3 (linear/standard)
            tex['format'] = 'DXT3'
            tex['has_alpha'] = True
        elif is_xbox and platform_prop in (0x10, 0x11):
            # 0x10 = DXT5 (swizzled), 0x11 = LIN_DXT5 (linear/standard)
            tex['format'] = 'DXT5'
            tex['has_alpha'] = True
        elif d3d_format == 0x31545844:
            tex['format'] = 'DXT1'
        elif d3d_format == 0x33545844:
            tex['format'] = 'DXT3'
            tex['has_alpha'] = True
        elif d3d_format == 0x35545844:
            tex['format'] = 'DXT5'
            tex['has_alpha'] = True
        elif not is_xbox and platform_id == 8 and platform_prop == 1:
            # D3D8 only: platform_prop 1/3/5 = DXT type
            # D3D9 uses d3d_format field — platform_prop is alpha/cube/mip/compressed flags
            tex['format'] = 'DXT1'
        elif not is_xbox and platform_id == 8 and platform_prop == 3:
            tex['format'] = 'DXT3'
            tex['has_alpha'] = True
        elif not is_xbox and platform_id == 8 and platform_prop == 5:
            tex['format'] = 'DXT5'
            tex['has_alpha'] = True
        elif is_sa_plus:
            d3d_fmt_map = {
                # D3D9 format enum -> internal format name
                21: 'ARGB8888',  # D3DFMT_A8R8G8B8 - stored BGRA 4bpp
                22: 'ARGB8888',  # D3DFMT_X8R8G8B8 - stored BGRX 4bpp, treat as ARGB8888 (alpha=255)
                32: 'ARGB8888',  # D3DFMT_A8B8G8R8
                20: 'RGB888',    # D3DFMT_R8G8B8   - true 24-bit, rare
                23: 'RGB565',    # D3DFMT_R5G6B5
                25: 'ARGB1555',  # D3DFMT_A1R5G5B5
                26: 'ARGB4444',  # D3DFMT_A4R4G4B4
                24: 'RGB555',    # D3DFMT_X1R5G5B5
                50: 'LUM8',      # D3DFMT_L8
                51: 'A8L8',      # D3DFMT_A8L8
                41: 'PAL8',      # D3DFMT_P8
            }
            tex['format'] = d3d_fmt_map.get(d3d_format,
                raster_pixel_map.get(pixel_fmt, f'UNKNOWN_{raster_format_flags:08X}'))
        else:
            tex['format'] = raster_pixel_map.get(pixel_fmt,
                f'UNKNOWN_{raster_format_flags:08X}')

        # D3DFMT_X8R8G8B8 (22): stored BGRX, X channel is padding not alpha
        if d3d_format == 22:
            tex['force_opaque'] = True  # force alpha=255 when decoding

        if tex['format'] in ('ARGB8888', 'ARGB1555', 'ARGB4444', 'DXT3', 'DXT5', 'PAL8', 'A8L8'):
            if not tex.get('has_alpha') and not tex.get('force_opaque'):
                tex['has_alpha'] = True

        # Read mipmap data
        # SA (D3D9, RW >= 0x1803FFFF): ALL formats have a 4-byte data_size field PER mipmap level
        # GTA3/VC (D3D8): ONLY DXT formats have data_size per level; raw/PAL data follows directly
        # Xbox (platform_id=5): ONE total data_size field covers ALL mipmap levels combined
        fmt = tex['format']
        is_dxt = 'DXT' in fmt

        # PC (D3D8/D3D9): palette once, then per level u32 size + data
        pal_data = b''
        if fmt in ('PAL8', 'PAL4'):
            pal_size = 1024 if fmt == 'PAL8' else (64 if depth == 4 else 128)
            pal_data = txd_data[pos:pos + pal_size]
            pos += pal_size
        w, h = width, height
        for level in range(num_levels):
            if 'DXT1' in fmt:
                expected = max(1, (w+3)//4) * max(1, (h+3)//4) * 8
            elif 'DXT' in fmt:
                expected = max(1, (w+3)//4) * max(1, (h+3)//4) * 16
            elif fmt in ('ARGB8888', 'A8L8'):
                expected = w * h * (4 if fmt == 'ARGB8888' else 2)
            elif fmt == 'RGB888':
                expected = w * h * (4 if tex.get('depth', 0) == 32 else 3)
            elif fmt == 'LUM8' or fmt == 'PAL8':
                expected = w * h
            elif fmt == 'PAL4':
                expected = (w * h + 1) // 2
            else:
                expected = w * h * 2
            if pos + 4 > struct_end:
                break
            declared = struct.unpack('<I', txd_data[pos:pos+4])[0]
            pos += 4
            size = declared if 0 < declared and pos + declared <= struct_end else expected
            if pos + size > len(txd_data):
                break
            level_data = txd_data[pos:pos+size]
            pos += size
            if level and not levels:
                w, h = max(1, w // 2), max(1, h // 2)
                continue

            lw = max(1, width >> level)
            lh = max(1, height >> level)
            if 'DXT' in fmt:
                rgba_data = decompress_dxt(level_data, lw, lh, fmt)
            elif fmt in ('PAL8', 'PAL4'):
                # GTA3/VC palettes are RGBA; SA (>=0x1803FFFF) palettes are BGRA
                _NO_ALPHA_TYPES = {0x0600, 0x0200, 0x0A00, 0x0400}  # 888,565,555,LUM
                force_opaque_pal = (raster_format_flags & 0x0F00) in _NO_ALPHA_TYPES
                rgba_data = decompress_raw(
                    level_data, lw, lh, fmt,
                    palette=pal_data,
                    palette_entry_fmt=tex.get('palette_entry_format', 'ARGB8888'),
                    palette_is_bgra=tex.get('palette_is_bgra', True),
                    force_opaque=force_opaque_pal)
            else:
                rgba_data = decompress_raw(
                    level_data, lw, lh, fmt,
                    depth=tex.get('depth', 0),
                    force_opaque=tex.get('force_opaque', False))

            mipmap_level = {
                'level': level,
                'width': max(1, width >> level),
                'height': max(1, height >> level),
                'rgba_data': rgba_data,
                'compressed_data': level_data if 'DXT' in tex['format'] else None,
                'compressed_size': len(level_data)
            }
            tex['mipmap_levels'].append(mipmap_level)

            # Store main texture data
            if level == 0:
                if rgba_data is None:
                    rgba_data = b'\x00' * (lw * lh * 4)
                tex['rgba_data'] = rgba_data

                # NEW: Extract alpha channel as separate grayscale mask
                if tex['has_alpha'] and rgba_data and len(rgba_data) == width * height * 4:
                    tex['alpha_mask'] = bytes(rgba_data[3::4])

            # Advance mipmap dimensions
            w = max(1, w // 2)
            h = max(1, h // 2)

        # Legacy IMG Factory bumpmap inside the struct (old saves)
        if tex['has_bumpmap'] and pos + 5 <= struct_end:
            try:
                bumpmap_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
                pos += 4

                bumpmap_type = struct.unpack('<B', txd_data[pos:pos+1])[0]
                pos += 1

                if pos + bumpmap_size <= struct_end:
                    tex['bumpmap_data'] = txd_data[pos:pos+bumpmap_size]
                    tex['bumpmap_type'] = bumpmap_type
                    pos += bumpmap_size

                    if log:
                        type_names = ['Height Map', 'Normal Map', 'Both']
                        type_name = type_names[bumpmap_type] if bumpmap_type < 3 else 'Unknown'
                        log(
                            f"  Bumpmap: {type_name} ({bumpmap_size} bytes)"
                        )
            except Exception as e:
                if log:
                    log(f"  Bumpmap read error: {str(e)}")

        # Legacy reflection map inside the struct (old saves)
        if pos + 8 <= struct_end:
            try:
                reflection_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
                pos += 4

                expected_reflection_size = width * height * 3
                if reflection_size == expected_reflection_size and pos + reflection_size <= struct_end:
                    tex['reflection_map'] = txd_data[pos:pos+reflection_size]
                    tex['has_reflection'] = True
                    pos += reflection_size

                    if pos + 4 <= len(txd_data):
                        fresnel_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
                        pos += 4

                        expected_fresnel_size = width * height
                        if fresnel_size == expected_fresnel_size and pos + fresnel_size <= struct_end:
                            tex['fresnel_map'] = txd_data[pos:pos+fresnel_size]
                            pos += fresnel_size

                            if log:
                                log(
                                    f"  Reflection maps: "
                                    f"Vector ({reflection_size}B) + Fresnel ({fresnel_size}B)"
                                )
            except Exception as e:
                pass

        # IMG Factory bumpmap/reflection plugin in the extension chunk
        if struct_end + 12 <= len(txd_data) and \
                struct.unpack_from('<I', txd_data, struct_end)[0] == 0x03:
            from apps.methods.txd_splice import read_bump_ext
            ext_size = struct.unpack_from('<I', txd_data, struct_end + 4)[0]
            tex.update(read_bump_ext(txd_data[struct_end + 12:struct_end + 12 + ext_size]))
        tex['has_bumpmap'] = bool(tex.get('bumpmap_data'))

    except Exception as e:
        if log:
            log(f"Texture parse error: {str(e)}")

    return tex


def ps2_entry(tex: dict) -> dict: #vers 1
    """PS2 parser dict to a workshop texture entry."""
    from apps.methods.txd_ps2_parser import ps2_tex_to_rgba
    rgba = ps2_tex_to_rgba(tex) or bytes(tex['width'] * tex['height'] * 4)
    d = tex['depth']
    fmt = {4: "PSMT4", 8: "PSMT8", 16: "PSMCT16", 32: "PSMCT32"}.get(d, f"{d}bpp")
    pal_type = (tex['raster_format_flags'] >> 13) & 0x3
    if pal_type in (1, 2):
        fmt += f"-PAL{'8' if pal_type == 1 else '4'}"
    return {'name': tex['name'], 'width': tex['width'], 'height': tex['height'],
            'depth': d, 'format': fmt, 'has_alpha': True,
            'alpha_name': tex.get('mask', ''), 'mipmaps': 1, 'rgba_data': rgba,
            'raster_format_flags': tex['raster_format_flags'], 'is_swizzled': False,
            'platform': 'PS2', 'compressed_size': tex.get('pixels_size', 0)}


def read_psp_txd(data: bytes) -> List[Dict]: #vers 1
    """TXD with PSP natives (LCS iOS); PS2 natives allowed too."""
    from apps.methods.txd_platform_psp import parse_psp_nativetex, psp_native_end
    from apps.methods.txd_ps2_parser import _parse_native
    count = struct.unpack_from('<H', data, 24)[0]
    texs, off = [], 28
    for i in range(count):
        if data[off + 24:off + 28] == b'PSP\0':
            t = parse_psp_nativetex(data, off, i)
            end = psp_native_end(data, off)
        else:
            t = ps2_entry(_parse_native(data, off))
            end = off + 12 + struct.unpack_from('<I', data, off + 4)[0]
        t['_chunk_off'], t['_chunk_end'] = off, end
        texs.append(t)
        off = end
    return texs


def read_rw_txd(data: bytes, levels: bool = True,
                log: Optional[Callable] = None) -> List[Dict]: #vers 1
    """PC / Xbox RW texture dictionary to texture dicts."""
    from apps.methods.txd_versions import detect_txd_version
    if len(data) < 28 or struct.unpack_from('<I', data, 0)[0] != 0x16:
        raise ValueError("Not a RenderWare texture dictionary")
    rw_ver = detect_txd_version(data)[0]
    stype, ssize = struct.unpack_from('<II', data, 12)
    if stype != 0x01 or ssize < 4:
        raise ValueError("Invalid TXD struct section")
    if rw_ver >= 0x1803FFFF:
        count = struct.unpack_from('<H', data, 24)[0]
    else:
        count = struct.unpack_from('<I', data, 24)[0]
    if count > 4096:
        raise ValueError(f"Invalid texture count {count}")
    texs, off = [], 24 + ssize
    for i in range(count):
        if off + 12 > len(data):
            break
        ctype, csize = struct.unpack_from('<II', data, off)
        if ctype == 0x15:
            texs.append(parse_native_texture(data, off, i, rw_ver, levels, log))
        off += 12 + csize
    return texs


def read_texture_file(data: bytes, name: str = '', levels: bool = True) -> Tuple[str, List[Dict]]: #vers 1
    """(kind, textures) for any supported texture file."""
    from apps.methods.nif_textures import is_nif_textures, parse_nif_textures
    from apps.methods.txd_lc_android import detect_lc_android_txd, parse_lc_android_txd
    from apps.methods.txd_ps2_parser import detect_ps2_txd, parse_ps2_txd
    if is_nif_textures(data):
        return 'nif', parse_nif_textures(data)
    if data[:4] == b'RSC\x05':
        from apps.methods.xtd_textures import parse_iv_wtd
        return 'wtd', parse_iv_wtd(data)
    if name.lower().endswith(('.xtx', '.chk')):
        from apps.methods.xtx_reader import parse_stories_textures
        return 'stories', parse_stories_textures(data)
    if detect_lc_android_txd(data):
        return 'lc_mobile', parse_lc_android_txd(data)
    if len(data) > 56 and data[52:56] == b'PSP\0':
        return 'inplace', read_psp_txd(data)
    if detect_ps2_txd(data[:64]):
        return 'rw', [ps2_entry(t) for t in parse_ps2_txd(data)]
    return 'rw', read_rw_txd(data, levels)


def _rw_native_names(data: bytes) -> List[str]: #vers 1
    """Names from RW natives (PC, Xbox, PS2, PSP) without pixel decode."""
    count = struct.unpack_from('<H', data, 24)[0]
    off, out = 24 + struct.unpack_from('<I', data, 16)[0], []
    for _ in range(count):
        if off + 24 > len(data):
            break
        ctype, csize = struct.unpack_from('<II', data, off)
        if ctype == 0x15:
            stype, ssize = struct.unpack_from('<II', data, off + 12)
            plat = struct.unpack_from('<I', data, off + 24)[0]
            if stype == 0x01 and ssize >= 88 and plat in (5, 8, 9):
                raw = data[off + 32:off + 64]
            else:
                q = off + 24 + ssize
                raw = data[q + 12:q + 12 + struct.unpack_from('<I', data, q + 4)[0]]
            out.append(raw.split(b'\0', 1)[0].decode('latin-1'))
        if data[52:56] == b'PSP\0' and ctype == 0x15:
            from apps.methods.txd_platform_psp import psp_native_end
            off = psp_native_end(data, off)
        else:
            off += 12 + csize
    return out


def texture_names(data: bytes, name: str = '') -> List[str]: #vers 1
    """Texture names in any supported texture file, fast for RW."""
    from apps.methods.txd_lc_android import detect_lc_android_txd
    if data[:4] == b'\x16\x00\x00\x00' and not detect_lc_android_txd(data):
        return _rw_native_names(data)
    if data[:4] == b'RSC\x05':
        import zlib
        from apps.methods.xtd_textures import _iv_entries, _rsc5_sizes
        vs = _rsc5_sizes(struct.unpack_from('<I', data, 8)[0])[0]
        return [e['name'] for e in _iv_entries(zlib.decompress(data[12:]), vs)]
    from apps.methods.nif_textures import is_nif_textures, _source_textures
    if is_nif_textures(data):
        return [n for n, _ref in _source_textures(data)[2]]
    return [t['name'] for t in read_texture_file(data, name, levels=False)[1]]
