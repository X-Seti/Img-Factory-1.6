#this belongs in apps/methods/txd_splice.py - Version: 1
# X-Seti - September 20 2026 - IMG Factory 1.6 - TXD splice rebuild

"""txd_splice.py - Rebuild a TXD from the ORIGINAL file bytes so a save
never re-encodes what the user didn't touch. Unchanged textures are
copied byte-for-byte, renames are patched in place (D3D8/D3D9 name
fields), deleted textures are dropped, and only new/replaced textures go
through the serializer. Fixes the old rebuild that either dropped every
edit (original data kept as-is) or re-encoded everything in SA layout
(losing palettes and half the VC/III textures on reload)."""

##Methods list -
# tag_loaded_texture
# texture_signature
# split_txd
# build_d3d8_chunk
# rebuild_txd

import struct
from typing import Callable, Dict, List, Optional

_TXD_DICT = 0x16
_TEX_NATIVE = 0x15
_D3D_PLATFORMS = (8, 9)   # names live at fixed struct offsets only for these


def texture_signature(tex: Dict) -> tuple: #vers 1
    """Everything that changes the encoded pixel data (name excluded)."""
    return (tex.get('width'), tex.get('height'), tex.get('depth'), tex.get('format'),
            bool(tex.get('has_alpha')), len(tex.get('mipmap_levels') or []),
            hash(bytes(tex.get('rgba_data') or b'')),
            hash(bytes(tex.get('compressed_data') or b'')),
            hash(bytes(tex.get('bumpmap_data') or b'')))


def tag_loaded_texture(tex: Dict): #vers 1
    """Call right after the loader builds a texture from the file."""
    tex['_src_name'] = str(tex.get('name', ''))
    tex['_src_alpha'] = str(tex.get('alpha_name', '') or '')
    tex['_src_sig'] = texture_signature(tex)


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


def _chunk_names(chunk: bytes):
    if len(chunk) < 96 or struct.unpack_from('<I', chunk, 24)[0] not in _D3D_PLATFORMS:
        return None
    nm = chunk[32:64].split(b'\0', 1)[0].decode('ascii', 'ignore')
    al = chunk[64:96].split(b'\0', 1)[0].decode('ascii', 'ignore')
    return nm, al


def _patch_names(chunk: bytes, name: str, alpha: str) -> bytes:
    b = bytearray(chunk)
    b[32:64] = name.encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    b[64:96] = alpha.encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    return bytes(b)


def build_d3d8_chunk(tex: Dict, rw_ver: int, enc_dxt1: Callable, enc_dxt5: Callable = None) -> Optional[bytes]: #vers 1
    """One D3D8 (GTA III/VC layout, also read by SA) texture-native chunk
    for a new/replaced texture: always DXT1 (no alpha) or DXT3 (alpha),
    single mip level, sourced from tex['rgba_data'] (RGBA). Matches the
    layout of real VC files: 88 byte struct + one size-prefixed level +
    empty extension."""
    w, h = int(tex.get('width') or 0), int(tex.get('height') or 0)
    rgba = bytes(tex.get('rgba_data') or b'')
    if w <= 0 or h <= 0 or len(rgba) < w * h * 4:
        return None
    alpha = rgba[3:w * h * 4:4]
    has_alpha = min(alpha) < 255 if alpha else False
    if has_alpha:
        opaque = bytearray(rgba[:w * h * 4])
        opaque[3::4] = b'\xff' * (w * h)
        colour = enc_dxt1(bytes(opaque), w, h)
        bx, by = (w + 3) // 4, (h + 3) // 4
        data = bytearray()
        for j in range(by):
            for i in range(bx):
                bits = 0
                for k in range(16):
                    x, y = min(i * 4 + (k & 3), w - 1), min(j * 4 + (k >> 2), h - 1)
                    bits |= (alpha[y * w + x] >> 4) << (4 * k)
                data += bits.to_bytes(8, 'little') + colour[(j * bx + i) * 8:(j * bx + i) * 8 + 8]
        data, cmp_code, rf = bytes(data), 3, 0x300
    else:
        data, cmp_code, rf = enc_dxt1(rgba, w, h), 1, 0x200
    name = str(tex.get('name', 'texture')).encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    mask = str(tex.get('alpha_name', '') or '').encode('ascii', 'ignore')[:31].ljust(32, b'\0')
    body = (struct.pack('<II', 8, int(tex.get('filter_flags') or 0x1106)) + name + mask
            + struct.pack('<IIHHBBBB', rf, 1 if has_alpha else 0, w, h, 16, 1, 4, cmp_code)
            + struct.pack('<I', len(data)) + data)
    st = struct.pack('<III', 1, len(body), rw_ver) + body
    return struct.pack('<III', _TEX_NATIVE, len(st) + 12, rw_ver) + st + struct.pack('<III', 3, 0, rw_ver)


def rebuild_txd(original: bytes, textures: List[Dict],
                serialize_one: Callable[[Dict], Optional[bytes]]) -> Optional[bytes]: #vers 1
    """Build new TXD bytes. serialize_one(tex) must return one complete
    texture-native chunk (used only for new/replaced textures). Returns
    None if `original` can't be split (caller falls back)."""
    parts = split_txd(original) if original else None
    if not parts:
        return None
    hdr, dstruct, chunks, tail = parts
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
        unchanged = idx is not None and t.get('_src_sig') == texture_signature(t)
        names = _chunk_names(chunks[idx]) if unchanged else None
        if unchanged and names is not None:
            new_name = str(t.get('name', names[0]))
            new_alpha = str(t.get('alpha_name', names[1]) or '') if names[1] else names[1]
            chunk = chunks[idx]
            if new_name != names[0] or new_alpha != names[1]:
                chunk = _patch_names(chunk, new_name, new_alpha)
            out.append(chunk)
        elif unchanged:
            out.append(chunks[idx])       # non-D3D platform: keep verbatim (rename not patchable)
        else:
            c = serialize_one(t)
            if not c:
                return None
            out.append(c)

    count_dev = struct.unpack_from('<HH', dstruct, 12)
    new_struct = struct.pack('<HH', len(out), count_dev[1])
    ver = struct.unpack_from('<I', hdr, 8)[0]
    inner = struct.pack('<III', 1, 4, ver) + new_struct + b''.join(out) + tail
    return struct.pack('<III', _TXD_DICT, len(inner), ver) + inner
