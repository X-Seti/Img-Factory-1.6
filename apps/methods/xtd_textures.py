#this belongs in apps/methods/xtd_textures.py - Version: 3
# X-Seti - October05 2026 - IMG Factory 1.6 - XTD texture dictionaries

"""
GTA IV .wtd (RSC5) read/write; GTA V / RDR2 .ytd (RSC8) read.
"""

##Methods list -
# _565_to_rgb
# _bc4_decode
# _bc5_decode
# _bc7_decode_fallback
# _decode_pixels
# _dxt1_decode
# _dxt3_decode
# _dxt5_decode
# _extract_v_textures
# get_xtd_game
# is_xtd_file
# _iv_decode
# _iv_encode
# _iv_entries
# _iv_level_dims
# _iv_rename
# open_xtd_dict
# parse_iv_wtd
# _parse_rsc5
# _parse_rsc8
# _physical_offset
# _read_cstr
# _read_v_string
# _read_v_texture
# _rsc5_sizes
# _rsc8_seg_size
# _v_scan_textures
# write_iv_wtd

##class XTDDict: -
##class XTDTexture: -

from __future__ import annotations
import struct, zlib
from dataclasses import dataclass, field
from typing import List, Optional, Tuple
from pathlib import Path

#    D3D / DXGI format identifiers                                              
_D3D_FMT = {
    0x31545844: "DXT1",
    0x33545844: "DXT3",
    0x35545844: "DXT5",
    0x00000015: "RGBA8",   # A8R8G8B8 / D3DFMT_A8R8G8B8
    0x00000016: "RGBX8",
}
_DXGI_FMT = {
    0x47: "BC1",    # 71  DXT1
    0x49: "BC2",    # 73  DXT3
    0x4B: "BC3",    # 75  DXT5
    0x4F: "BC4",    # 79
    0x55: "BC5",    # 85
    0x62: "BC7",    # 98
    0x1C: "RGBA8",  # 28  DXGI_FORMAT_R8G8B8A8_UNORM
    0x57: "BGRA8",  # 87  DXGI_FORMAT_B8G8R8A8_UNORM
    0x3D: "R8",     # 61
}

#    RSC header                                                                  
_RSC8_MAGIC = 0x52534338   # 'RSC8'


@dataclass
class XTDTexture:
    name:    str
    width:   int
    height:  int
    fmt:     str          # "DXT1", "DXT5", "RGBA8", "BC7", …
    mips:    int
    rgba:    bytes        # decoded RGBA8888, top mip only
    raw:     bytes        # compressed pixel bytes (top mip)


@dataclass
class XTDDict:
    path:     str
    game:     str         # "IV", "V", "RDR2"
    version:  int
    textures: List[XTDTexture] = field(default_factory=list)
    error:    str = ""


#    Public entry point                                                          

def open_xtd_dict(path: str) -> XTDDict:
    """Parse a .wtd or .ytd file.  Returns XTDDict; check .error if non-empty."""
    data = Path(path).read_bytes()
    if len(data) < 16:
        return XTDDict(path=path, game="?", version=0, error="File too small")

    magic = struct.unpack_from("<I", data, 0)[0]

    if magic == _RSC5_MAGIC:
        return _parse_rsc5(path, data)
    elif magic == _RSC8_MAGIC:
        return _parse_rsc8(path, data)
    else:
        # Try OODLE-compressed YTD (GTA V PC later builds) — we can't decompress
        # without the proprietary oodle DLL, so just report it gracefully
        return XTDDict(path=path, game="?", version=0,
                        error=f"Unknown magic 0x{magic:08X} — may be OODLE-compressed (unsupported)")


#    RSC5 (GTA IV .wtd)                                                         

_RSC5_MAGIC = 0x05435352   # b'RSC\x05'
_IV_FMT = {0x31545844: 'DXT1', 0x33545844: 'DXT3', 0x35545844: 'DXT5',
           21: 'ARGB8888', 22: 'RGB888', 50: 'LUM8'}
_IV_BPP = {'ARGB8888': 4, 'RGB888': 4, 'LUM8': 1}


def _rsc5_sizes(flags: int) -> Tuple[int, int]: #vers 1
    """Virtual and physical segment sizes from RSC5 flags."""
    vs = (flags & 0x7FF) << (((flags >> 11) & 0xF) + 8)
    ps = ((flags >> 15) & 0x7FF) << (((flags >> 26) & 0xF) + 8)
    return vs, ps


def _iv_entries(z: bytes, vs: int) -> List[dict]: #vers 2
    """grcTexturePC records from the decompressed IV dictionary."""
    tp, tc = struct.unpack_from('<IH', z, 0x18)
    out = []
    for i in range(tc):
        o = struct.unpack_from('<I', z, (tp & 0xFFFFFFF) + 4 * i)[0] & 0xFFFFFFF
        name_p = struct.unpack_from('<I', z, o + 0x14)[0] & 0xFFFFFFF
        w, h, fcc = struct.unpack_from('<HHI', z, o + 0x1C)
        levels = z[o + 0x27]
        raw = struct.unpack_from('<I', z, o + 0x48)[0] & 0xFFFFFFF
        name = z[name_p:z.index(b'\0', name_p)].decode('latin1')
        name = name.split(':/', 1)[-1]
        if name.lower().endswith('.dds'):
            name = name[:-4]
        fmt = _IV_FMT.get(fcc)
        if fmt is None:
            raise ValueError(f"{name}: unknown IV texture format {fcc:#x}")
        out.append({'name': name, 'width': w, 'height': h, 'format': fmt,
                    'levels': max(1, levels), 'offset': vs + raw, 'rec': o,
                    'name_off': name_p, 'name_len': z.index(b'\0', name_p) - name_p})
    return out


def _iv_level_dims(e: dict) -> List[Tuple[int, int, int]]: #vers 1
    """(width, height, byte size) per stored level."""
    dims = []
    w, h = e['width'], e['height']
    for _ in range(e['levels']):
        if e['format'] in _IV_BPP:
            size = w * h * _IV_BPP[e['format']]
        else:
            size = ((w + 3) // 4) * ((h + 3) // 4) * (8 if e['format'] == 'DXT1' else 16)
        dims.append((w, h, size))
        w, h = max(1, w // 2), max(1, h // 2)
    return dims


def _iv_decode(fmt: str, data: bytes, w: int, h: int) -> bytes: #vers 1
    """One IV level to RGBA bytes."""
    import numpy as np
    from apps.methods.mobile_texture_decode import (decode_dxt, GL_DXT1A,
                                                    GL_DXT3, GL_DXT5)
    if fmt in ('DXT1', 'DXT3', 'DXT5'):
        enc = {'DXT1': GL_DXT1A, 'DXT3': GL_DXT3, 'DXT5': GL_DXT5}[fmt]
        return np.ascontiguousarray(decode_dxt(data, w, h, enc)).tobytes()
    if fmt == 'LUM8':
        a = np.frombuffer(data, np.uint8, w * h)
        return np.stack([a, a, a, np.full_like(a, 255)], 1).tobytes()
    a = np.frombuffer(data, np.uint8, w * h * 4).reshape(-1, 4)[:, [2, 1, 0, 3]].copy()
    if fmt == 'RGB888':
        a[:, 3] = 255
    return a.tobytes()


def _iv_encode(fmt: str, rgba: bytes, w: int, h: int) -> bytes: #vers 1
    """RGBA bytes to one IV level."""
    import numpy as np
    from apps.methods.txd_dxt_encode import _encode_dxt1, _encode_dxt3, _encode_dxt5
    a = np.frombuffer(rgba, np.uint8, w * h * 4).reshape(-1, 4)
    if fmt == 'DXT1':
        return _encode_dxt1(rgba, w, h, alpha=bool((a[:, 3] < 128).any()))
    if fmt == 'DXT3':
        return _encode_dxt3(rgba, w, h)
    if fmt == 'DXT5':
        return _encode_dxt5(rgba, w, h)
    if fmt == 'LUM8':
        return a[:, :3].mean(1).round().astype(np.uint8).tobytes()
    out = a[:, [2, 1, 0, 3]].copy()
    if fmt == 'RGB888':
        out[:, 3] = 255
    return out.tobytes()


def parse_iv_wtd(data: bytes) -> List[dict]: #vers 1
    """GTA IV .wtd to workshop texture dicts (all mip levels)."""
    magic, _ver, flags = struct.unpack_from('<III', data, 0)
    if magic != _RSC5_MAGIC:
        raise ValueError("Not a GTA IV RSC5 resource")
    z = zlib.decompress(data[12:])
    vs, _ps = _rsc5_sizes(flags)
    texs = []
    for e in _iv_entries(z, vs):
        lv, pos = [], e['offset']
        for i, (w, h, size) in enumerate(_iv_level_dims(e)):
            lv.append({'level': i, 'width': w, 'height': h,
                       'rgba_data': _iv_decode(e['format'], z[pos:pos + size], w, h)})
            pos += size
        rgba = lv[0]['rgba_data']
        alpha = e['format'] != 'RGB888' and e['format'] != 'LUM8' and \
            any(b != 255 for b in rgba[3::4])
        texs.append({'name': e['name'], 'width': e['width'], 'height': e['height'],
                     'depth': 32 if e['format'] in ('ARGB8888', 'RGB888') else
                     (8 if e['format'] == 'LUM8' else 4 if e['format'] == 'DXT1' else 8),
                     'format': e['format'], 'has_alpha': alpha, 'alpha_name': '',
                     'mipmaps': len(lv), 'rgba_data': rgba, 'mipmap_levels': lv,
                     'raster_format_flags': 0, 'platform': 'GTA IV PC',
                     'compressed_size': sum(s for _, _, s in _iv_level_dims(e))})
    return texs


def _iv_rename(z: bytearray, ents: List[dict], names: List[Optional[str]]): #vers 1
    """Rename in place; name hashes re-sorted with the texture array."""
    from apps.methods.gta_dat_parser import iv_hash
    for e, new in zip(ents, names):
        if not new or new == e['name']:
            continue
        raw = f"pack:/{new}.dds".encode('ascii')
        if len(raw) > e['name_len']:
            raise ValueError(f"'{new}': GTA IV name too long "
                             f"(max {e['name_len'] - 10} characters)")
        z[e['name_off']:e['name_off'] + e['name_len']] = raw.ljust(e['name_len'], b'\0')
        e['name'] = new
    hashes = [iv_hash(e['name']) for e in ents]
    if len(set(hashes)) != len(hashes):
        raise ValueError("Two GTA IV textures would share a name")
    hp, tp = struct.unpack_from('<I', z, 0x10)[0], struct.unpack_from('<I', z, 0x18)[0]
    ptrs = [struct.unpack_from('<I', z, (tp & 0xFFFFFFF) + 4 * i)[0] for i in range(len(ents))]
    for i, (h, ptr) in enumerate(sorted(zip(hashes, ptrs))):
        struct.pack_into('<I', z, (hp & 0xFFFFFFF) + 4 * i, h)
        struct.pack_into('<I', z, (tp & 0xFFFFFFF) + 4 * i, ptr)


def write_iv_wtd(original: bytes, textures: List[Optional[dict]],
                 names: Optional[List[Optional[str]]] = None) -> bytes: #vers 2
    """Rewrite edited textures in place; None keeps a texture unchanged."""
    from apps.methods.txd_splice import _level_rgba
    renamed = names and any(names)
    if all(t is None for t in textures) and not renamed:
        return original
    flags = struct.unpack_from('<I', original, 8)[0]
    z = bytearray(zlib.decompress(original[12:]))
    vs, _ps = _rsc5_sizes(flags)
    ents = _iv_entries(z, vs)
    if len(textures) != len(ents):
        raise ValueError("GTA IV dictionaries can't add or remove textures")
    for e, t in zip(ents, textures):
        if t is None:
            continue
        if (t['width'], t['height']) != (e['width'], e['height']):
            raise ValueError(f"'{e['name']}': GTA IV textures keep their size")
        if t.get('format') != e['format']:
            raise ValueError(f"'{e['name']}': GTA IV textures keep their format")
        pos, top = e['offset'], bytes(t['rgba_data'])
        given = {l.get('level'): l for l in (t.get('mipmap_levels') or [])}
        for i, (w, h, size) in enumerate(_iv_level_dims(e)):
            old = bytes(z[pos:pos + size])
            l = given.get(i)
            src = top if i == 0 else None
            if src is None and l and (l.get('width'), l.get('height')) == (w, h) and \
                    bytes(l['rgba_data']) != _iv_decode(e['format'], old, w, h):
                src = bytes(l['rgba_data'])
            if src is None:
                src = _level_rgba(top, e['width'], e['height'], w, h)
            z[pos:pos + size] = _iv_encode(e['format'], src, w, h)
            pos += size
    if renamed:
        _iv_rename(z, ents, names)
    return original[:12] + zlib.compress(bytes(z), 9)


def _parse_rsc5(path: str, data: bytes) -> XTDDict: #vers 1
    """GTA IV .wtd as an XTDDict (Asset Workshop import)."""
    rd = XTDDict(path=path, game="IV", version=struct.unpack_from('<I', data, 4)[0])
    try:
        for t in parse_iv_wtd(data):
            rd.textures.append(XTDTexture(name=t['name'], width=t['width'],
                                          height=t['height'], fmt=t['format'],
                                          mips=t['mipmaps'], rgba=t['rgba_data'], raw=b''))
    except Exception as e:
        rd.error = str(e)
    return rd


#    RSC8 (GTA V / RDR2 .ytd)                                                   

def _parse_rsc8(path: str, data: bytes) -> XTDDict:
    """GTA V / RDR2 .ytd — RSC8."""
    try:
        magic, version, vflags, pflags = struct.unpack_from("<4I", data, 0)

        game = "V" if version <= 46 else "RDR2"

        # RSC8 virtual/physical size encoding (same idea, bigger shifts)
        vsize = _rsc8_seg_size(vflags)
        psize = _rsc8_seg_size(pflags)

        vdata = data[16 : 16 + vsize]
        pdata = data[16 + vsize : 16 + vsize + psize]

        rd = XTDDict(path=path, game=game, version=version)
        _extract_v_textures(vdata, pdata, rd, version)
        return rd
    except Exception as e:
        return XTDDict(path=path, game="V", version=0, error=str(e))


def _rsc8_seg_size(flags: int) -> int:
    """Decode RSC8 segment size from flags field."""
    sizes = [0] * 9
    sizes[0] = (flags >> 27) & 0x1  # x4G
    sizes[1] = (flags >> 26) & 0x1  # x2G
    sizes[2] = (flags >> 25) & 0x1  # x1G
    sizes[3] = (flags >> 24) & 0x1  # x512M
    sizes[4] = (flags >> 17) & 0x7F # x256M blocks
    sizes[5] = (flags >> 11) & 0x3F # x128M
    sizes[6] = (flags >> 7)  & 0xF  # x16M
    sizes[7] = (flags >> 5)  & 0x3  # x8M? (approx)
    sizes[8] = (flags >> 0)  & 0x1F # small

    total = 0
    mults = [0x100000000, 0x80000000, 0x40000000, 0x20000000,
             0x10000000, 0x8000000, 0x4000000, 0x2000000, 0x1000]
    for s, m in zip(sizes, mults):
        total += s * m
    # Clamp to actual data — RSC8 may be over-estimated
    return min(total, 256 * 1024 * 1024)


def _extract_v_textures(vdata: bytes, pdata: bytes, rd: XTDDict, version: int):
    """Walk pgDictionary<grcTextureDX11> in GTA V virtual segment."""
    BASE = 0x60000000  # V virtual base

    def _vptr(ptr: int) -> int:
        if ptr == 0: return -1
        return (ptr & 0x0FFFFFFF)

    if len(vdata) < 0x20:
        _v_scan_textures(vdata, pdata, rd)
        return

    try:
        count = struct.unpack_from("<I", vdata, 0x10)[0]
        tex_ptr_raw = struct.unpack_from("<I", vdata, 0x18)[0]
        tex_arr_off = _vptr(tex_ptr_raw)

        if tex_arr_off < 0 or count == 0 or tex_arr_off + count * 8 > len(vdata):
            _v_scan_textures(vdata, pdata, rd)
            return

        for i in range(min(count, 512)):
            entry_ptr = struct.unpack_from("<Q", vdata, tex_arr_off + i * 8)[0]
            entry_off = _vptr(entry_ptr & 0xFFFFFFFF)
            if entry_off < 0 or entry_off + 0x80 > len(vdata):
                continue
            _read_v_texture(vdata, pdata, entry_off, rd, version)
    except Exception:
        _v_scan_textures(vdata, pdata, rd)


def _read_v_texture(vdata: bytes, pdata: bytes, off: int, rd: XTDDict, version: int):
    """Parse grcTextureDX11 entry."""
    try:
        # grcTextureDX11 layout (GTA V PC, simplified):
        # +00 pgBase (16 bytes on 64-bit)
        # +10 ptr name
        # +18 u8 depth, pad[3]
        # +1C u16 width
        # +1E u16 height
        # +20 u16 depth/layers
        # +22 u8 mips, u8 format (DXGI)
        # ...
        # +28 ptr pixel_data (physical)

        if off + 0x40 > len(vdata):
            return

        name_ptr = struct.unpack_from("<Q", vdata, off + 0x10)[0] & 0xFFFFFFFF
        width    = struct.unpack_from("<H", vdata, off + 0x1C)[0]
        height   = struct.unpack_from("<H", vdata, off + 0x1E)[0]
        mips     = vdata[off + 0x22] if off + 0x23 < len(vdata) else 1
        dxgi_fmt = vdata[off + 0x23] if off + 0x24 < len(vdata) else 0
        pix_ptr  = struct.unpack_from("<Q", vdata, off + 0x28)[0] & 0xFFFFFFFF

        name = _read_v_string(vdata, name_ptr & 0x0FFFFFFF)
        if not name:
            name = f"tex_{len(rd.textures):04d}"

        pix_off = pix_ptr & 0x0FFFFFFF
        fmt = _DXGI_FMT.get(dxgi_fmt, f"DXGI_{dxgi_fmt:02X}")
        raw, rgba = _decode_pixels(pdata, pix_off, width, height, fmt)

        rd.textures.append(XTDTexture(
            name=name, width=width, height=height,
            fmt=fmt, mips=mips, rgba=rgba, raw=raw))
    except Exception:
        pass


def _v_scan_textures(vdata: bytes, pdata: bytes, rd: XTDDict):
    """Fallback scan for grcTextureDX11 in GTA V."""
    seen = set()
    i = 0x40
    while i < len(vdata) - 0x40:
        try:
            w = struct.unpack_from("<H", vdata, i)[0]
            h = struct.unpack_from("<H", vdata, i + 2)[0]
            if (w in (16,32,64,128,256,512,1024,2048) and
                h in (16,32,64,128,256,512,1024,2048) and
                (i, w, h) not in seen):
                seen.add((i, w, h))
                dxgi = vdata[i + 4] if i + 5 < len(vdata) else 0
                fmt = _DXGI_FMT.get(dxgi, "RGBA8")
                # Try physical data nearby
                pix_off = i * 2  # rough heuristic
                raw, rgba = _decode_pixels(pdata, pix_off, w, h, fmt)
                if rgba:
                    rd.textures.append(XTDTexture(
                        name=f"tex_{len(rd.textures):04d}",
                        width=w, height=h, fmt=fmt,
                        mips=1, rgba=rgba, raw=raw))
                if len(rd.textures) >= 256:
                    break
        except Exception:
            pass
        i += 8


#    Helpers                                                                     

def _read_v_string(vdata: bytes, off: int) -> str:
    return _read_cstr(vdata, off)


def _read_cstr(data: bytes, off: int) -> str:
    if off < 0 or off >= len(data):
        return ""
    end = data.find(b'\x00', off)
    if end < 0:
        end = min(off + 64, len(data))
    try:
        s = data[off:end].decode('latin1').strip()
        # Filter non-printable junk
        return s if all(32 <= ord(c) < 127 for c in s) and s else ""
    except Exception:
        return ""


def _physical_offset(ptr: int, pdata: bytes) -> int:
    """Convert physical pointer to pdata offset."""
    off = ptr & 0x0FFFFFFF
    return off if off < len(pdata) else 0


def _decode_pixels(pdata: bytes, off: int, w: int, h: int, fmt: str) -> Tuple[bytes, bytes]:
    """Return (raw_compressed, rgba_decoded). Both may be b'' on failure."""
    if w <= 0 or h <= 0 or w > 4096 or h > 4096:
        return b'', b''
    try:
        if fmt in ("RGBA8", "BGRA8", "RGBX8"):
            size = w * h * 4
            if off + size > len(pdata):
                return b'', b''
            raw = pdata[off:off+size]
            if fmt == "BGRA8":
                # Swap R and B
                arr = bytearray(raw)
                for i in range(0, len(arr), 4):
                    arr[i], arr[i+2] = arr[i+2], arr[i]
                raw = bytes(arr)
            elif fmt == "RGBX8":
                arr = bytearray(raw)
                for i in range(3, len(arr), 4):
                    arr[i] = 255
                raw = bytes(arr)
            return raw, raw

        elif fmt in ("DXT1", "BC1"):
            size = max(1, w//4) * max(1, h//4) * 8
            if off + size > len(pdata):
                return b'', b''
            raw = pdata[off:off+size]
            rgba = _dxt1_decode(raw, w, h)
            return raw, rgba

        elif fmt in ("DXT3", "BC2"):
            size = max(1, w//4) * max(1, h//4) * 16
            if off + size > len(pdata):
                return b'', b''
            raw = pdata[off:off+size]
            rgba = _dxt3_decode(raw, w, h)
            return raw, rgba

        elif fmt in ("DXT5", "BC3"):
            size = max(1, w//4) * max(1, h//4) * 16
            if off + size > len(pdata):
                return b'', b''
            raw = pdata[off:off+size]
            rgba = _dxt5_decode(raw, w, h)
            return raw, rgba

        elif fmt in ("BC4", "R8"):
            size = max(1, w//4) * max(1, h//4) * 8
            if off + size > len(pdata):
                return b'', b''
            raw = pdata[off:off+size]
            rgba = _bc4_decode(raw, w, h)
            return raw, rgba

        elif fmt == "BC5":
            size = max(1, w//4) * max(1, h//4) * 16
            if off + size > len(pdata):
                return b'', b''
            raw = pdata[off:off+size]
            rgba = _bc5_decode(raw, w, h)
            return raw, rgba

        elif fmt == "BC7":
            # BC7 decode is complex — use PIL if available, else return blank
            size = max(1, w//4) * max(1, h//4) * 16
            if off + size > len(pdata):
                return b'', b''
            raw = pdata[off:off+size]
            rgba = _bc7_decode_fallback(raw, w, h)
            return raw, rgba

        else:
            # Unknown format — return blank RGBA
            return b'', bytes(w * h * 4)

    except Exception:
        return b'', b''


#    DXT1 decoder                                                                

def _565_to_rgb(c: int):
    r = ((c >> 11) & 0x1F) * 255 // 31
    g = ((c >> 5)  & 0x3F) * 255 // 63
    b = ((c >> 0)  & 0x1F) * 255 // 31
    return r, g, b


def _dxt1_decode(data: bytes, w: int, h: int) -> bytes:
    out = bytearray(w * h * 4)
    bw = max(1, w // 4)
    bh = max(1, h // 4)
    pos = 0
    for by in range(bh):
        for bx in range(bw):
            if pos + 8 > len(data):
                break
            c0, c1 = struct.unpack_from("<HH", data, pos)
            idx_bits = struct.unpack_from("<I", data, pos + 4)[0]
            pos += 8

            r0,g0,b0 = _565_to_rgb(c0)
            r1,g1,b1 = _565_to_rgb(c1)
            if c0 > c1:
                pal = [(r0,g0,b0,255),(r1,g1,b1,255),
                       ((2*r0+r1)//3,(2*g0+g1)//3,(2*b0+b1)//3,255),
                       ((r0+2*r1)//3,(g0+2*g1)//3,(b0+2*b1)//3,255)]
            else:
                pal = [(r0,g0,b0,255),(r1,g1,b1,255),
                       ((r0+r1)//2,(g0+g1)//2,(b0+b1)//2,255),
                       (0,0,0,0)]

            for py in range(4):
                for px in range(4):
                    ix = bx*4+px; iy = by*4+py
                    if ix < w and iy < h:
                        ci = (idx_bits >> (2*(py*4+px))) & 3
                        r,g,b,a = pal[ci]
                        p = (iy*w+ix)*4
                        out[p:p+4] = bytes([r,g,b,a])
    return bytes(out)


def _dxt3_decode(data: bytes, w: int, h: int) -> bytes:
    out = bytearray(w * h * 4)
    bw = max(1, w // 4)
    bh = max(1, h // 4)
    pos = 0
    for by in range(bh):
        for bx in range(bw):
            if pos + 16 > len(data):
                break
            alpha_block = data[pos:pos+8]
            c0, c1 = struct.unpack_from("<HH", data, pos+8)
            idx_bits = struct.unpack_from("<I", data, pos+12)[0]
            pos += 16

            r0,g0,b0 = _565_to_rgb(c0)
            r1,g1,b1 = _565_to_rgb(c1)
            pal = [(r0,g0,b0),(r1,g1,b1),
                   ((2*r0+r1)//3,(2*g0+g1)//3,(2*b0+b1)//3),
                   ((r0+2*r1)//3,(g0+2*g1)//3,(b0+2*b1)//3)]

            for py in range(4):
                ab = struct.unpack_from("<H", alpha_block, py*2)[0]
                for px in range(4):
                    ix = bx*4+px; iy = by*4+py
                    if ix < w and iy < h:
                        a = ((ab >> (px*4)) & 0xF) * 17
                        ci = (idx_bits >> (2*(py*4+px))) & 3
                        r,g,b = pal[ci]
                        p = (iy*w+ix)*4
                        out[p:p+4] = bytes([r,g,b,a])
    return bytes(out)


def _dxt5_decode(data: bytes, w: int, h: int) -> bytes:
    out = bytearray(w * h * 4)
    bw = max(1, w // 4)
    bh = max(1, h // 4)
    pos = 0
    for by in range(bh):
        for bx in range(bw):
            if pos + 16 > len(data):
                break
            a0 = data[pos]; a1 = data[pos+1]
            abits = int.from_bytes(data[pos+2:pos+8], 'little')
            c0, c1 = struct.unpack_from("<HH", data, pos+8)
            idx_bits = struct.unpack_from("<I", data, pos+12)[0]
            pos += 16

            if a0 > a1:
                apal = [a0, a1,
                        (6*a0+1*a1)//7,(5*a0+2*a1)//7,
                        (4*a0+3*a1)//7,(3*a0+4*a1)//7,
                        (2*a0+5*a1)//7,(1*a0+6*a1)//7]
            else:
                apal = [a0,a1,
                        (4*a0+1*a1)//5,(3*a0+2*a1)//5,
                        (2*a0+3*a1)//5,(1*a0+4*a1)//5,
                        0, 255]

            r0,g0,b0 = _565_to_rgb(c0)
            r1,g1,b1 = _565_to_rgb(c1)
            pal = [(r0,g0,b0),(r1,g1,b1),
                   ((2*r0+r1)//3,(2*g0+g1)//3,(2*b0+b1)//3),
                   ((r0+2*r1)//3,(g0+2*g1)//3,(b0+2*b1)//3)]

            for py in range(4):
                for px in range(4):
                    ix = bx*4+px; iy = by*4+py
                    if ix < w and iy < h:
                        ai  = (abits >> (3*(py*4+px))) & 7
                        a   = apal[ai]
                        ci  = (idx_bits >> (2*(py*4+px))) & 3
                        r,g,b = pal[ci]
                        p = (iy*w+ix)*4
                        out[p:p+4] = bytes([r,g,b,a])
    return bytes(out)


def _bc4_decode(data: bytes, w: int, h: int) -> bytes:
    """BC4 = single channel (R), expand to RGBA greyscale."""
    grey = bytearray(w * h)
    bw = max(1, w // 4); bh = max(1, h // 4)
    pos = 0
    for by in range(bh):
        for bx in range(bw):
            if pos + 8 > len(data): break
            r0 = data[pos]; r1 = data[pos+1]
            rbits = int.from_bytes(data[pos+2:pos+8], 'little')
            pos += 8
            if r0 > r1:
                rpal = [r0,r1,(6*r0+r1)//7,(5*r0+2*r1)//7,
                        (4*r0+3*r1)//7,(3*r0+4*r1)//7,(2*r0+5*r1)//7,(r0+6*r1)//7]
            else:
                rpal = [r0,r1,(4*r0+r1)//5,(3*r0+2*r1)//5,
                        (2*r0+3*r1)//5,(r0+4*r1)//5,0,255]
            for py in range(4):
                for px in range(4):
                    ix = bx*4+px; iy = by*4+py
                    if ix < w and iy < h:
                        ri = (rbits >> (3*(py*4+px))) & 7
                        grey[iy*w+ix] = rpal[ri]
    out = bytearray(w*h*4)
    for i in range(w*h):
        v = grey[i]
        out[i*4:i*4+4] = bytes([v,v,v,255])
    return bytes(out)


def _bc5_decode(data: bytes, w: int, h: int) -> bytes:
    """BC5 = RG normal map, reconstruct B=sqrt(1-R²-G²)."""
    bw = max(1, w // 4); bh = max(1, h // 4)
    out = bytearray(w*h*4)
    pos = 0
    for by in range(bh):
        for bx in range(bw):
            if pos + 16 > len(data): break
            r0=data[pos]; r1=data[pos+1]
            rbits = int.from_bytes(data[pos+2:pos+8],'little')
            g0=data[pos+8]; g1=data[pos+9]
            gbits = int.from_bytes(data[pos+10:pos+16],'little')
            pos += 16
            def _pal(v0,v1):
                if v0>v1:
                    return [v0,v1,(6*v0+v1)//7,(5*v0+2*v1)//7,
                            (4*v0+3*v1)//7,(3*v0+4*v1)//7,(2*v0+5*v1)//7,(v0+6*v1)//7]
                return [v0,v1,(4*v0+v1)//5,(3*v0+2*v1)//5,
                        (2*v0+3*v1)//5,(v0+4*v1)//5,0,255]
            rpal=_pal(r0,r1); gpal=_pal(g0,g1)
            for py in range(4):
                for px in range(4):
                    ix=bx*4+px; iy=by*4+py
                    if ix<w and iy<h:
                        ri=(rbits>>(3*(py*4+px)))&7
                        gi=(gbits>>(3*(py*4+px)))&7
                        r=rpal[ri]; g=gpal[gi]
                        # Reconstruct B from normal
                        import math
                        nx=(r/127.5)-1.0; ny=(g/127.5)-1.0
                        nz2=max(0.0,1.0-nx*nx-ny*ny)
                        b=int((math.sqrt(nz2)*0.5+0.5)*255)
                        p=(iy*w+ix)*4
                        out[p:p+4]=bytes([r,g,b,255])
    return bytes(out)


def _bc7_decode_fallback(data: bytes, w: int, h: int) -> bytes:
    """BC7 decode placeholder — returns magenta checkerboard to signal unsupported."""
    out = bytearray(w * h * 4)
    for y in range(h):
        for x in range(w):
            if (x // 8 + y // 8) % 2 == 0:
                c = bytes([255, 0, 255, 255])
            else:
                c = bytes([180, 0, 180, 255])
            p = (y * w + x) * 4
            out[p:p+4] = c
    return bytes(out)


#    Detection helper                                                            

def is_xtd_file(path: str) -> bool:
    """Quick check — is this file a WTD or YTD?"""
    try:
        with open(path, 'rb') as f:
            magic = struct.unpack("<I", f.read(4))[0]
        return magic in (_RSC5_MAGIC, _RSC8_MAGIC)
    except Exception:
        return False


def get_xtd_game(path: str) -> str:
    """Return 'IV', 'V', 'RDR2', or '' if not a XTD dict."""
    try:
        with open(path, 'rb') as f:
            magic, version = struct.unpack("<II", f.read(8))
        if magic == _RSC5_MAGIC:
            return "IV"
        if magic == _RSC8_MAGIC:
            return "V" if version <= 46 else "RDR2"
    except Exception:
        pass
    return ""
