"""
apps/methods/xtd_textures.py  —  XTD texture dictionary reader
Supports: GTA IV .wtd (RSC7 v13), GTA V .ytd (RSC8 v46/165), RDR2 .ytd

READ-ONLY import source.  Never written back.  Completely undocumented.
Textures extracted here are offered as import candidates inside TXD Workshop.

Format notes:
  RSC7: magic 0x52534337, GTA IV PC, version 13
  RSC8: magic 0x52534338, GTA V / RDR2 PC, version 46 / 165
  Both pack virtual+physical segments after an 8-byte RSC header.
  Texture entries use grcTexturePC (IV) / grcTextureDX11 (V) layout.
  Pixel data is DXT1 / DXT3 / DXT5 / A8R8G8B8 / BC4 / BC5 / BC7.
"""

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
_RSC7_MAGIC = 0x52534337   # 'RSC7'
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

    if magic == _RSC7_MAGIC:
        return _parse_rsc7(path, data)
    elif magic == _RSC8_MAGIC:
        return _parse_rsc8(path, data)
    else:
        # Try OODLE-compressed YTD (GTA V PC later builds) — we can't decompress
        # without the proprietary oodle DLL, so just report it gracefully
        return XTDDict(path=path, game="?", version=0,
                        error=f"Unknown magic 0x{magic:08X} — may be OODLE-compressed (unsupported)")


#    RSC7 (GTA IV .wtd)                                                         

def _parse_rsc7(path: str, data: bytes) -> XTDDict:
    """GTA IV PC .wtd — RSC7 version 13."""
    try:
        magic, version, vflags, pflags = struct.unpack_from("<4I", data, 0)
        vsize = (vflags & 0x7FF) << ((vflags >> 11) & 0xF)
        psize = (pflags & 0x7FF) << ((pflags >> 11) & 0xF)

        # Virtual segment starts at offset 16, physical right after
        vdata = data[16 : 16 + vsize]
        pdata = data[16 + vsize : 16 + vsize + psize]

        rd = XTDDict(path=path, game="IV", version=version)
        _extract_iv_textures(vdata, pdata, rd)
        return rd
    except Exception as e:
        return XTDDict(path=path, game="IV", version=0, error=str(e))


def _extract_iv_textures(vdata: bytes, pdata: bytes, rd: XTDDict):
    """Walk pgDictionary<grcTexturePC> in GTA IV virtual segment."""
    # pgDictionary starts at virtual address 0x50000000 = offset 0 in vdata
    # Layout (offsets from dict base):
    #   +0x00  pgBase (8 bytes: blockmap ptr, refcount)
    #   +0x08  u32 count
    #   +0x0C  hashes ptr  (30-bit virtual ptr)
    #   +0x10  textures ptr (30-bit virtual ptr)
    #   +0x14  u16 count2, u16 count3

    BASE = 0x50000000  # IV virtual base

    def _vptr(ptr: int) -> int:
        """Convert 30-bit virtual pointer to vdata offset."""
        if ptr == 0:
            return -1
        return (ptr & 0x0FFFFFFF) - (BASE & 0x0FFFFFFF)

    if len(vdata) < 0x18:
        return

    count = struct.unpack_from("<I", vdata, 0x08)[0]
    tex_ptr_raw = struct.unpack_from("<I", vdata, 0x10)[0]
    tex_arr_off = _vptr(tex_ptr_raw)

    if tex_arr_off < 0 or tex_arr_off + count * 4 > len(vdata):
        # Fallback: scan for texture signatures
        _iv_scan_textures(vdata, pdata, rd)
        return

    for i in range(min(count, 512)):
        entry_ptr = struct.unpack_from("<I", vdata, tex_arr_off + i * 4)[0]
        entry_off = _vptr(entry_ptr)
        if entry_off < 0 or entry_off + 0x58 > len(vdata):
            continue
        _read_iv_texture(vdata, pdata, entry_off, rd, BASE)


def _read_iv_texture(vdata: bytes, pdata: bytes, off: int, rd: XTDDict, BASE: int):
    """Parse grcTexturePC entry at vdata[off]."""
    try:
        # grcTexturePC layout (GTA IV PC):
        # +00 pgBase (8 bytes)
        # +08 u32 object_id / pad
        # +0C ptr name
        # +10 u8 depth, u8 stride_log2, u16 unknown
        # +14 u32 d3dformat
        # +18 u16 width, u16 height
        # +1C u8 mips, u8 flags, u16 pad
        # +20 ptr pixel_data  (physical segment ptr)
        # ... more fields

        name_ptr = struct.unpack_from("<I", vdata, off + 0x0C)[0]
        d3dfmt   = struct.unpack_from("<I", vdata, off + 0x14)[0]
        width    = struct.unpack_from("<H", vdata, off + 0x18)[0]
        height   = struct.unpack_from("<H", vdata, off + 0x1A)[0]
        mips     = vdata[off + 0x1C] if off + 0x1D < len(vdata) else 1
        pix_ptr  = struct.unpack_from("<I", vdata, off + 0x20)[0]

        # Read name
        name = _read_iv_string(vdata, name_ptr, BASE)
        if not name:
            name = f"tex_{len(rd.textures):04d}"

        # Physical pointer -> pdata offset
        pix_off = _physical_offset(pix_ptr, pdata)
        fmt = _D3D_FMT.get(d3dfmt, f"D3D_{d3dfmt:08X}")
        raw, rgba = _decode_pixels(pdata, pix_off, width, height, fmt)

        rd.textures.append(XTDTexture(
            name=name, width=width, height=height,
            fmt=fmt, mips=mips, rgba=rgba, raw=raw))
    except Exception:
        pass


def _iv_scan_textures(vdata: bytes, pdata: bytes, rd: XTDDict):
    """Brute-force scan for grcTexturePC signatures when dict parse fails."""
    # Look for reasonable width/height pairs preceded by D3D format ID
    BASE = 0x50000000
    seen = set()
    i = 0x50
    while i < len(vdata) - 0x40:
        d3dfmt = struct.unpack_from("<I", vdata, i)[0]
        if d3dfmt in _D3D_FMT:
            try:
                w = struct.unpack_from("<H", vdata, i + 4)[0]
                h = struct.unpack_from("<H", vdata, i + 6)[0]
                if w in (16,32,64,128,256,512,1024,2048) and h in (16,32,64,128,256,512,1024,2048):
                    key = (i, w, h)
                    if key not in seen:
                        seen.add(key)
                        pix_ptr = struct.unpack_from("<I", vdata, i + 0x10)[0]
                        pix_off = _physical_offset(pix_ptr, pdata)
                        fmt = _D3D_FMT[d3dfmt]
                        raw, rgba = _decode_pixels(pdata, pix_off, w, h, fmt)
                        rd.textures.append(XTDTexture(
                            name=f"tex_{len(rd.textures):04d}",
                            width=w, height=h, fmt=fmt, mips=1,
                            rgba=rgba, raw=raw))
                        if len(rd.textures) >= 512:
                            break
            except Exception:
                pass
        i += 4


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

def _read_iv_string(vdata: bytes, ptr: int, BASE: int) -> str:
    off = (ptr & 0x0FFFFFFF) - (BASE & 0x0FFFFFFF)
    return _read_cstr(vdata, off)


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
        return magic in (_RSC7_MAGIC, _RSC8_MAGIC)
    except Exception:
        return False


def get_xtd_game(path: str) -> str:
    """Return 'IV', 'V', 'RDR2', or '' if not a XTD dict."""
    try:
        with open(path, 'rb') as f:
            magic, version = struct.unpack("<II", f.read(8))
        if magic == _RSC7_MAGIC:
            return "IV"
        if magic == _RSC8_MAGIC:
            return "V" if version <= 46 else "RDR2"
    except Exception:
        pass
    return ""
