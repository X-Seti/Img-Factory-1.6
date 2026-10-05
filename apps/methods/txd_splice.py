#this belongs in apps/methods/txd_splice.py - Version: 3
# X-Seti - October05 2026 - IMG Factory 1.6 - TXD splice rebuild

"""txd_splice.py - TXD writer. Rebuilds from the ORIGINAL file bytes so a
save never re-encodes what the user didn't touch: unchanged textures are
copied byte-for-byte, renames and filter flags are patched in place,
bumpmap/reflection data lives in an IMG Factory extension plugin, and
only textures whose pixels/format changed are encoded (D3D8 or D3D9,
every PC raster format)."""

##Methods list -
# _bump_payload
# _chunk_names
# _encode_level
# _ext_chunks
# _join_native
# _level_rgba
# _native_parts
# _palette
# _palette_index
# _patch_names
# _platform_of
# _set_versions
# _with_ext
# build_native_chunk
# build_txd
# ext_signature
# meta_signature
# read_bump_ext
# rebuild_txd
# split_txd
# tag_loaded_texture
# texture_signature

import struct
from typing import Dict, List, Optional

import numpy as np

_TXD_DICT = 0x16
_TEX_NATIVE = 0x15
_STRUCT, _STRING, _EXT = 0x01, 0x02, 0x03
_PS2 = 0x00325350                       # 'PS2\0'
_SA_VER = 0x1803FFFF
IMGF_EXT = 0x00F1A001                  # IMG Factory bumpmap/reflection plugin

# format -> (raster pixel type, depth, d3d9 format)
_RAW = {
    'ARGB8888': (0x0500, 32, 21), 'RGB888': (0x0600, 32, 22),
    'ARGB1555': (0x0100, 16, 25), 'RGB565': (0x0200, 16, 23),
    'ARGB4444': (0x0300, 16, 26), 'RGB555': (0x0A00, 16, 24),
    'LUM8': (0x0400, 8, 50), 'A8L8': (0x0400, 16, 51),
}
_DXT = {'DXT1': (1, 0x31545844), 'DXT3': (3, 0x33545844), 'DXT5': (5, 0x35545844)}


def texture_signature(tex: Dict) -> tuple: #vers 2
    """Everything that changes the encoded pixel data (name/flags excluded)."""
    lv = tex.get('mipmap_levels') or []
    return (tex.get('width'), tex.get('height'), tex.get('depth'), tex.get('format'),
            bool(tex.get('has_alpha')), len(lv),
            (tex.get('raster_format_flags') or 0) & 0xFF00,
            hash(bytes(tex.get('rgba_data') or b'')),
            hash(bytes(tex.get('compressed_data') or b'')),
            tuple(hash(bytes(l.get('rgba_data') or b'')) for l in lv[1:]))


def meta_signature(tex: Dict) -> int: #vers 1
    """Filter / addressing flags (patched in place, no re-encode)."""
    return int(tex.get('filter_flags') or 0)


def ext_signature(tex: Dict) -> tuple: #vers 1
    """Bumpmap / reflection data stored in the IMG Factory plugin."""
    return (int(tex.get('bumpmap_type') or 0), hash(bytes(tex.get('bumpmap_data') or b'')),
            hash(bytes(tex.get('reflection_map') or b'')), hash(bytes(tex.get('fresnel_map') or b'')))


def tag_loaded_texture(tex: Dict): #vers 2
    """Call right after the loader builds a texture from the file."""
    tex['_src_name'] = str(tex.get('name', ''))
    tex['_src_alpha'] = str(tex.get('alpha_name', '') or '')
    tex['_src_sig'] = texture_signature(tex)
    tex['_src_meta'] = meta_signature(tex)
    tex['_src_ext'] = ext_signature(tex)


def split_txd(data: bytes): #vers 1
    """Returns (dict_header12, dict_struct_bytes, [native_chunks], tail) or
    None when data isn't a plain texture dictionary."""
    if len(data) < 28:
        return None
    ctype, csize, cver = struct.unpack_from('<III', data, 0)
    if ctype != _TXD_DICT:
        return None
    stype, ssize, _ = struct.unpack_from('<III', data, 12)
    if stype != 1 or ssize != 4:
        return None
    pos, end = 28, min(len(data), 12 + csize)
    chunks = []
    while pos + 12 <= end:
        t, sz, _v = struct.unpack_from('<III', data, pos)
        if t != _TEX_NATIVE:
            break
        chunks.append(data[pos:pos + 12 + sz])
        pos += 12 + sz
    return data[:12], data[12:28], chunks, data[pos:]


def _platform_of(chunk: bytes) -> int: #vers 1
    return struct.unpack_from('<I', chunk, 24)[0] if len(chunk) >= 28 else 0


def _native_parts(chunk: bytes): #vers 1
    """(version, [child (type, payload)]) of a texture-native chunk."""
    ver = struct.unpack_from('<I', chunk, 8)[0]
    kids, pos = [], 12
    while pos + 12 <= len(chunk):
        t, sz, _v = struct.unpack_from('<III', chunk, pos)
        kids.append((t, chunk[pos + 12:pos + 12 + sz]))
        pos += 12 + sz
    return ver, kids


def _ext_chunks(payload: bytes) -> List[tuple]: #vers 1
    """Plugin chunks inside an extension payload: [(type, ver, data)]."""
    out, pos = [], 0
    while pos + 12 <= len(payload):
        t, sz, v = struct.unpack_from('<III', payload, pos)
        out.append((t, v, payload[pos + 12:pos + 12 + sz]))
        pos += 12 + sz
    return out


def _bump_payload(tex: Dict) -> bytes: #vers 1
    """IMG Factory plugin data, empty when the texture has none."""
    bump = bytes(tex.get('bumpmap_data') or b'')
    refl = bytes(tex.get('reflection_map') or b'')
    fres = bytes(tex.get('fresnel_map') or b'')
    if not (bump or refl or fres):
        return b''
    return (b'IMGF' + struct.pack('<BB', 1, int(tex.get('bumpmap_type') or 0))
            + struct.pack('<I', len(bump)) + bump + struct.pack('<I', len(refl)) + refl
            + struct.pack('<I', len(fres)) + fres)


def read_bump_ext(ext_payload: bytes) -> Dict: #vers 1
    """Bumpmap / reflection fields from a texture's extension payload."""
    for t, _v, d in _ext_chunks(ext_payload):
        if t != IMGF_EXT or d[:4] != b'IMGF' or len(d) < 18:
            continue
        btype = d[5]
        pos = 6
        out = {}
        for key in ('bumpmap_data', 'reflection_map', 'fresnel_map'):
            n = struct.unpack_from('<I', d, pos)[0]
            out[key] = d[pos + 4:pos + 4 + n]
            pos += 4 + n
        out['bumpmap_type'] = btype
        out['has_bumpmap'] = bool(out['bumpmap_data'])
        out['has_reflection'] = bool(out['reflection_map'])
        return out
    return {}


def _with_ext(ext_payload: bytes, tex: Dict, ver: int) -> bytes: #vers 1
    """Extension payload with the IMG Factory plugin replaced."""
    keep = b''.join(struct.pack('<III', t, len(d), v) + d
                    for t, v, d in _ext_chunks(ext_payload) if t != IMGF_EXT)
    bp = _bump_payload(tex)
    return keep + (struct.pack('<III', IMGF_EXT, len(bp), ver) + bp if bp else b'')


def _join_native(ver: int, kids: List[tuple]) -> bytes: #vers 1
    body = b''.join(struct.pack('<III', t, len(p), ver) + p for t, p in kids)
    return struct.pack('<III', _TEX_NATIVE, len(body), ver) + body


def _chunk_names(chunk: bytes): #vers 2
    """(name, alpha) of a native chunk, any platform; None if unreadable."""
    plat = _platform_of(chunk)
    if plat == _PS2:
        _v, kids = _native_parts(chunk)
        s = [p for t, p in kids if t == _STRING]
        if len(s) < 2:
            return None
        return tuple(x.split(b'\0', 1)[0].decode('ascii', 'ignore') for x in s[:2])
    if len(chunk) < 96:
        return None
    nm = chunk[32:64].split(b'\0', 1)[0].decode('ascii', 'ignore')
    al = chunk[64:96].split(b'\0', 1)[0].decode('ascii', 'ignore')
    return nm, al


def _patch_names(chunk: bytes, name: str, alpha: str) -> bytes: #vers 2
    """Rename in place: fixed fields (PC/Xbox) or string chunks (PS2)."""
    if _platform_of(chunk) == _PS2:
        ver, kids = _native_parts(chunk)
        new, n = [], 0
        for t, p in kids:
            if t == _STRING and n < 2:
                s = (name if n == 0 else alpha).encode('ascii', 'ignore')
                p = s + b'\0' * (4 - len(s) % 4)
                n += 1
            new.append((t, p))
        return _join_native(ver, new)
    b = bytearray(chunk)
    b[32:64] = name.encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    b[64:96] = alpha.encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    return bytes(b)


def _level_rgba(rgba: bytes, w: int, h: int, lw: int, lh: int) -> bytes: #vers 1
    """RGBA of one mip level, box-filtered down from the full image."""
    if (lw, lh) == (w, h):
        return rgba
    from PIL import Image
    return Image.frombytes('RGBA', (w, h), rgba).resize((lw, lh), Image.Resampling.BOX).tobytes()


def _palette(rgba: bytes, w: int, h: int, colours: int) -> bytes: #vers 1
    """Palette (colours x RGBA) for the image."""
    from PIL import Image
    q = Image.frombytes('RGBA', (w, h), rgba).quantize(colors=colours, method=Image.Quantize.FASTOCTREE)
    pal = q.getpalette(rawmode='RGBA') or []
    return bytes(pal[:colours * 4]).ljust(colours * 4, b'\0')


def _palette_index(rgba: bytes, pal: bytes) -> np.ndarray: #vers 1
    """Nearest palette entry (RGBA distance) per pixel."""
    px = np.frombuffer(rgba, dtype=np.uint8).reshape(-1, 4).astype(np.int32)
    pe = np.frombuffer(pal, dtype=np.uint8).reshape(-1, 4).astype(np.int32)
    out = np.empty(len(px), dtype=np.uint8)
    for i in range(0, len(px), 16384):
        d = ((px[i:i + 16384, None, :] - pe[None, :, :]) ** 2).sum(-1)
        out[i:i + 16384] = d.argmin(1)
    return out


def _encode_level(fmt: str, rgba: bytes, w: int, h: int, pal: bytes = b'') -> bytes: #vers 1
    """Pixel data of one level in the TXD's stored layout."""
    from apps.methods.txd_dxt_encode import _encode_dxt1, _encode_dxt3, _encode_dxt5
    if fmt == 'DXT1':
        return _encode_dxt1(rgba, w, h, alpha=True)
    if fmt in _DXT:
        return {'DXT3': _encode_dxt3, 'DXT5': _encode_dxt5}[fmt](rgba, w, h)
    if fmt in ('PAL8', 'PAL4'):
        ix = _palette_index(rgba, pal)
        if fmt == 'PAL8':
            return ix.tobytes()
        if len(ix) % 2:
            ix = np.append(ix, 0)
        return ((ix[0::2] << 4) | (ix[1::2] & 15)).astype(np.uint8).tobytes()
    a = np.frombuffer(rgba, dtype=np.uint8).reshape(-1, 4).astype(np.uint32)
    r, g, b, al = a[:, 0], a[:, 1], a[:, 2], a[:, 3]
    if fmt == 'ARGB8888':
        return np.stack([b, g, r, al], 1).astype(np.uint8).tobytes()
    if fmt == 'RGB888':
        return np.stack([b, g, r, np.full_like(r, 255)], 1).astype(np.uint8).tobytes()
    if fmt == 'LUM8':
        return ((r * 299 + g * 587 + b * 114) // 1000).astype(np.uint8).tobytes()
    if fmt == 'A8L8':
        return np.stack([(r * 299 + g * 587 + b * 114) // 1000, al], 1).astype(np.uint8).tobytes()
    if fmt == 'ARGB1555':
        v = ((al >= 128) << 15) | ((r >> 3) << 10) | ((g >> 3) << 5) | (b >> 3)
    elif fmt == 'RGB565':
        v = ((r >> 3) << 11) | ((g >> 2) << 5) | (b >> 3)
    elif fmt == 'ARGB4444':
        v = ((al >> 4) << 12) | ((r >> 4) << 8) | ((g >> 4) << 4) | (b >> 4)
    elif fmt == 'RGB555':
        v = (1 << 15) | ((r >> 3) << 10) | ((g >> 3) << 5) | (b >> 3)
    else:
        raise ValueError(f"Cannot write texture format '{fmt}'")
    return v.astype('<u2').tobytes()


def build_native_chunk(tex: Dict, rw_ver: int, platform: int = None,
                       ext_payload: bytes = b'') -> bytes: #vers 1
    """One texture-native chunk (D3D8 = III/VC layout, D3D9 = SA) encoded
    from tex['rgba_data'] in tex['format']. Mip levels: imported level
    RGBA when its size matches, else box-filtered from the image."""
    if platform is None:
        platform = 9 if rw_ver >= _SA_VER else 8
    if platform not in (8, 9):
        raise ValueError("Only PC (D3D8/D3D9) textures can be encoded")
    fmt = str(tex.get('format') or 'DXT1')
    w, h = int(tex.get('width') or 0), int(tex.get('height') or 0)
    rgba = bytes(tex.get('rgba_data') or b'')
    if w <= 0 or h <= 0 or len(rgba) < w * h * 4:
        raise ValueError(f"Texture '{tex.get('name')}' has no pixel data")
    rgba = rgba[:w * h * 4]
    if fmt not in _DXT and fmt not in _RAW and fmt not in ('PAL8', 'PAL4'):
        raise ValueError(f"Texture '{tex.get('name')}': format '{fmt}' can't be saved")
    has_alpha = bool(np.frombuffer(rgba, dtype=np.uint8)[3::4].min() < 255)

    src_levels = tex.get('mipmap_levels') or []
    n_levels = max(1, len(src_levels))
    pal = stored = b''
    if fmt in ('PAL8', 'PAL4'):
        pal = stored = _palette(rgba, w, h, 256 if fmt == 'PAL8' else 16)
        if rw_ver >= _SA_VER:                           # SA palettes stored BGRA
            stored = np.frombuffer(pal, dtype=np.uint8).reshape(-1, 4)[:, [2, 1, 0, 3]].tobytes()
    levels, lw, lh = [], w, h
    for i in range(n_levels):
        lv = src_levels[i] if i < len(src_levels) else {}
        src = bytes(lv.get('rgba_data') or b'') if i else rgba
        if len(src) != lw * lh * 4:
            src = _level_rgba(rgba, w, h, lw, lh)
        levels.append(_encode_level(fmt, src, lw, lh, pal))
        if lw == 1 and lh == 1:
            break
        lw, lh = max(1, lw // 2), max(1, lh // 2)

    if fmt in _DXT:
        cmp_code, d3d = _DXT[fmt]
        ptype = 0x0300 if fmt != 'DXT1' else (0x0100 if has_alpha else 0x0200)
        depth = 16
    elif fmt in ('PAL8', 'PAL4'):
        cmp_code, d3d = 0, 41
        ptype = (0x2000 if fmt == 'PAL8' else 0x4000) | (0x0500 if has_alpha else 0x0600)
        depth = 8 if fmt == 'PAL8' else 4
    else:
        ptype, depth, d3d = _RAW[fmt]
        cmp_code = 0
    rf = ptype
    if len(levels) > 1:
        rf |= 0x8000 | (int(tex.get('raster_format_flags') or 0) & 0x1000)

    name = str(tex.get('name', 'texture')).encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    mask = str(tex.get('alpha_name', '') or '').encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    head = struct.pack('<II', platform, int(tex.get('filter_flags') or 0x1106)) + name + mask
    if platform == 8:
        head += struct.pack('<IIHHBBBB', rf, 1 if has_alpha else 0, w, h, depth, len(levels), 4, cmp_code)
    else:
        flags = (1 if has_alpha else 0) | (8 if fmt in _DXT else 0)
        head += struct.pack('<IIHHBBBB', rf, d3d, w, h, depth, len(levels), 4, flags)
    body = head + stored + b''.join(struct.pack('<I', len(b)) + b for b in levels)
    ext = _with_ext(ext_payload, tex, rw_ver)
    return _join_native(rw_ver, [(_STRUCT, body), (_EXT, ext)])


def _set_versions(chunk: bytes, ver: int) -> bytes: #vers 1
    """Same native chunk with every header version set to ver."""
    _v, kids = _native_parts(chunk)
    out = []
    for t, p in kids:
        if t == _EXT:
            p = b''.join(struct.pack('<III', et, len(d), ver) + d for et, _ev, d in _ext_chunks(p))
        out.append((t, p))
    return _join_native(ver, out)


def build_txd(textures: List[Dict], rw_ver: int, device: int = None) -> bytes: #vers 2
    """New TXD from scratch, every texture encoded."""
    out = [build_native_chunk(t, rw_ver) for t in textures]
    dev = device if device is not None else (2 if rw_ver >= _SA_VER else 0)
    inner = (struct.pack('<III', 1, 4, rw_ver) + struct.pack('<HH', len(out), dev)
             + b''.join(out) + struct.pack('<III', 3, 0, rw_ver))
    return struct.pack('<III', _TXD_DICT, len(inner), rw_ver) + inner


def rebuild_txd(original: bytes, textures: List[Dict], target_ver: int = None,
                target_dev: int = None) -> Optional[bytes]: #vers 2
    """New TXD bytes from the original file plus the edited texture list.
    Unchanged chunks are copied, renames/flags/plugin data patched, edited
    pixels re-encoded on the texture's own platform. target_ver converts
    the version (D3D9 textures become D3D8 for pre-SA versions). Raises
    ValueError for edits that can't be written (e.g. PS2 pixels)."""
    parts = split_txd(original) if original else None
    if not parts:
        return None
    hdr, dstruct, chunks, tail = parts
    src_ver = struct.unpack_from('<I', hdr, 8)[0]
    ver = target_ver or src_ver
    by_name: Dict[str, List[int]] = {}
    for i, c in enumerate(chunks):
        n = _chunk_names(c)
        by_name.setdefault((n[0] if n else f"#{i}").lower(), []).append(i)

    out = []
    for t in textures:
        src = t.get('_src_name')
        idx = None
        if src is not None:
            q = by_name.get(src.lower())
            idx = q[0] if q else None
            if q and len(q) > 1:
                q.pop(0)          # consume in file order; last one stays reusable for copies
        chunk = chunks[idx] if idx is not None else None
        plat = _platform_of(chunk) if chunk else None
        if plat == 9 and ver < _SA_VER:
            plat = 8
        pixels_same = chunk is not None and t.get('_src_sig') == texture_signature(t) \
            and (plat == _platform_of(chunk))
        if not pixels_same:
            if plat not in (None, 8, 9):
                raise ValueError(f"'{t.get('name')}': pixel edits can't be saved on this "
                                 f"platform (only names and flags)")
            ext = _native_parts(chunk)[1] if chunk else []
            ext_payload = next((p for k, p in ext if k == _EXT), b'')
            out.append(build_native_chunk(t, ver, plat, ext_payload))
            continue
        names = _chunk_names(chunk)
        new_name = str(t.get('name', names[0] if names else ''))
        new_alpha = str(t.get('alpha_name', '') or '')
        if names is not None and (new_name != names[0] or new_alpha != names[1]):
            chunk = _patch_names(chunk, new_name, new_alpha)
        elif names is None and (new_name != t.get('_src_name') or new_alpha != t.get('_src_alpha')):
            raise ValueError(f"'{t.get('name')}': rename not supported for this platform")
        if plat in (8, 9, 5) and t.get('_src_meta') != meta_signature(t):
            b = bytearray(chunk)
            struct.pack_into('<I', b, 28, meta_signature(t))
            chunk = bytes(b)
        if t.get('_src_ext') != ext_signature(t):
            cv, kids = _native_parts(chunk)
            kids = [(k, _with_ext(p, t, cv) if k == _EXT else p) for k, p in kids]
            if not any(k == _EXT for k, _p in kids):
                kids.append((_EXT, _with_ext(b'', t, cv)))
            chunk = _join_native(cv, kids)
        if ver != src_ver:
            chunk = _set_versions(chunk, ver)
        out.append(chunk)

    count_dev = struct.unpack_from('<HH', dstruct, 12)
    new_struct = struct.pack('<HH', len(out), count_dev[1] if target_dev is None else target_dev)
    if ver != src_ver:
        tail = b''.join(struct.pack('<III', t2, len(d), ver) + d for t2, _v2, d in _ext_chunks(tail))
    inner = struct.pack('<III', 1, 4, ver) + new_struct + b''.join(out) + tail
    return struct.pack('<III', _TXD_DICT, len(inner), ver) + inner
