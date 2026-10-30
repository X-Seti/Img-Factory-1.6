#this belongs in apps/components/Path_Workshop/depends/path_logic_func.py - Version: 3
# X-Seti - October 20 2026 - IMG Factory 1.6 - Path Workshop logic

"""
Path Workshop logic - radar tile loading, game presets, zoom, recent files.
"""

##Methods list -
# _name_iv
# _name_sa
# _name_sol
# _rgba_wh
# decode_dxt1

##class RadarTxdReader: -
# _native_textures
# _ps2_pal_to_rgba
# _raw_to_rgba
# native_kind
# read

##class ImgReader: -
# __init__
# _load
# _load_v1_dir
# find_radar_entries
# get_entry_data
# list_radar_like

##class PathLogicMixin: -
# _apply_grid_zoom
# _apply_preset
# _autodetect
# _clear_recent
# _fit
# _is_iv_img
# _load_iv_radar
# _load_standalone_txd
# _load_texdb_radar
# _on_custom_changed
# _on_game_changed
# _open_file
# _open_radar_image
# _open_recent
# _set_native_tile_size
# _zoom
# get_world_bounds


import sys
import struct
import re
import math
from PyQt6.QtWidgets import QApplication, QFileDialog, QMessageBox, QProgressDialog
from PyQt6.QtCore import Qt
from pathlib import Path

# - Game presets
def _name_sa(idx):  return f"RADAR{idx:02d}" #vers 1

def _name_sol(idx): return f"radar{idx:04d}" #vers 1

def _name_iv(idx):  return f"radar{idx}" #vers 1

IV_TILE_SIZE = 512   # GTA IV radar.img tiles: radarN.wtd, DXT1

# Grid constants (authoritative — do not change without verifying against game files)
# SA: 144 tiles (12x12)  VC/III/LC/LCS/VCS: 64 tiles (8x8)  SOL: 1296 tiles (36x36)
# img_source: 'img'=tiles in .img | 'txd'=single .txd | 'pvr'=.pvr img | 'toc'=toc/tmb/dat
# Bitdepth lookup by img_source / platform
_BITDEPTH = {
    "img_pc":  "DXT1·4bpp",   "img_xbox": "DXT1·4bpp",
    "img_ps2": "PAL8·8bpp",   "pvr":      "PVRTC·4bpp",
    "toc":     "DAT·enc",     "chk":      "GIM·16bpp",
    "xtx":     "XTX·16bpp",   "img":      "DXT1·4bpp",
}

GAME_PRESETS = {
    # PC versions — tiles in gta3.img / gta.img / RadarTex.img rw_ver (Aug 20 2026)
    "III PC":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA III (PC/PS2/Xbox)",
                "rw_ver":0x1003FFFF,
                "hint":"Load gta3.img — contains radar00.txd to radar63.txd"},
    "VC PC":   {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA Vice City (PC/PS2/Xbox)",
                "rw_ver":0x1003FFFF,
                "hint":"Load gta3.img — contains radar00.txd to radar63.txd"},
    "SA PC":   {"cols":12, "rows":12, "count":144,  "name_fn":_name_sa,
                "img_pattern":r"^radar(\d{2}|1[0-3]\d|14[0-3])\.txd$",
                "img_source":"img",  "label":"GTA San Andreas (PC/PS2/Xbox)",
                "rw_ver":0x1803FFFF,
                "hint":"Load gta3.img — contains radar00.txd to radar143.txd (144 tiles, 12x12 grid)"},
    "LCS PC":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA Liberty City Stories (PC/PS2)",
                "rw_ver":0x1003FFFF,
                "hint":"Load gta3.img — contains radar00.txd to radar63.txd"},
    "VCS PC":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA Vice City Stories (PC/PS2)",
                "rw_ver":0x1003FFFF,
                "hint":"Load gta3.img — contains radar00.txd to radar63.txd"},
    "SOL":     {"cols":36, "rows":36, "count":1296,  "name_fn":_name_sol,
                "img_pattern":r"^radar\d{4}\.txd$",
                "img_source":"img",  "label":"GTA State of Liberty (PC)",
                "rw_ver":0x1803FFFF,
                "hint":"Load RadarTex.img — contains radar0000.txd to radar1295.txd"},
    "IV PC":   {"cols":12, "rows":12, "count":144,  "name_fn":_name_iv,
                "img_pattern":r"^radar\d{1,3}\.wtd$",
                "img_source":"iv",   "label":"GTA IV (PC)",
                "rw_ver":0,
                "hint":"Load pc/models/cdimages/radar.img - radar0.wtd to radar143.wtd (12x12, 512px)"},
    # Android versions
    "III And": {"cols":1,  "rows":1,  "count":1,    "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"img",  "label":"GTA III Android",
                "rw_ver":0x1003FFFF,
                "hint":"Load gta3_unc.img — single RADAR.TXD with 256x256 'radardisc' texture"},
    "VC And":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA Vice City Android",
                "rw_ver":0x1003FFFF,
                "hint":"Load from root/texdb — see TXD Workshop for texdb format"},
    "SA And":  {"cols":10, "rows":10, "count":100,  "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"toc",  "label":"GTA San Andreas Android",
                "rw_ver":0x1803FFFF,
                "hint":"Load txd.dxt.toc — SA Android uses TOC/TMB/DAT format (not yet supported)"},
    # iOS versions
    "LCS iOS": {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"pvr",  "label":"GTA LCS iOS",
                "rw_ver":0x1003FFFF,
                "hint":"Load gta3.pvr — contains radar00.txd to radar63.txd (PVRTC format)"},
    "SA iOS":  {"cols":10, "rows":10, "count":100,  "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"toc",  "label":"GTA SA iOS",
                "rw_ver":0x1803FFFF,
                "hint":"Load txd.dxt.toc — iOS SA uses TOC/TMB/DAT format (not yet supported)"},
    # PSP versions
    "LCS PSP": {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"chk",  "label":"GTA LCS PSP",
                "rw_ver":0x1003FFFF,
                "hint":"PSP .chk files use GIM/XTX format — load individual radar .chk files"},
    "VCS PSP": {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"xtx",  "label":"GTA VCS PSP",
                "rw_ver":0x1003FFFF,
                "hint":"PSP .xtx files use GIM/XTX format — load individual radar .xtx files"},
    # Generic
    "Custom":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"img",  "label":"Custom Grid",
                "rw_ver":0x1803FFFF,
                "hint":"Load any .img archive and adjust W/H spinners"},
}
TILE_W = TILE_H = 128


# - DXT1 codec
def _rgba_wh(rgba, default=None): #vers 1
    """(w, h) of a square RGBA tile from its length; default for non-square."""
    n = len(rgba) // 4 if rgba else 0
    s = int(round(n ** 0.5))
    if s * s == n and s:
        return s, s
    return default or (TILE_W, TILE_H)


def decode_dxt1(data, w, h): #vers 1
    out = bytearray(w*h*4); bx=(w+3)//4; by=(h+3)//4; pos=0
    for by2 in range(by):
        for bx2 in range(bx):
            if pos+8>len(data): break
            c0=struct.unpack_from('<H',data,pos)[0]; c1=struct.unpack_from('<H',data,pos+2)[0]
            lut=struct.unpack_from('<I',data,pos+4)[0]; pos+=8
            def u5(v): return ((v>>11)&31)*255//31,((v>>5)&63)*255//63,(v&31)*255//31
            r0,g0,b0=u5(c0); r1,g1,b1=u5(c1)
            if c0>c1: pal=[(r0,g0,b0,255),(r1,g1,b1,255),((2*r0+r1)//3,(2*g0+g1)//3,(2*b0+b1)//3,255),((r0+2*r1)//3,(g0+2*g1)//3,(b0+2*b1)//3,255)]
            else:     pal=[(r0,g0,b0,255),(r1,g1,b1,255),((r0+r1)//2,(g0+g1)//2,(b0+b1)//2,255),(0,0,0,0)]
            for py in range(4):
                for px in range(4):
                    ix=bx2*4+px; iy=by2*4+py
                    if ix<w and iy<h:
                        ci=(lut>>((py*4+px)*2))&3; o=(iy*w+ix)*4; out[o:o+4]=pal[ci]
    return bytes(out)

# - TXD reader/writer

class RadarTxdReader:
    @staticmethod
    def read(data): #vers 7
        """Read first texture from a RW TXD.
        Supports: PC D3D8/D3D9 (8/9), Xbox (5), PS2 (6), iOS/Android (8 ver 0x1005FFFF);
        PSP/PS2 natives and War Drum mobile (III/VC iOS/Android) via shared readers.
        """
        if RadarTxdReader.native_kind(data):
            tex = RadarTxdReader._native_textures(data)[0]
            return tex['rgba_data'], tex['width'], tex['height'], tex['name']
        pos = 12  # skip outer 0x16 container header
        while pos + 12 <= len(data):
            st = struct.unpack_from('<I', data, pos)[0]
            ss = struct.unpack_from('<I', data, pos+4)[0]
            if st == 0x15:  # TextureNative
                th = pos + 24   # skip 0x15 header(12) + inner 0x01 header(12)
                platform = struct.unpack_from('<I', data, th)[0]

                #    Xbox (platform 5)                                          
                if platform == 5:
                    nb_ = data[th+8:th+40]
                    nb_ = nb_[:nb_.index(b'\x00')] if b'\x00' in nb_ else nb_
                    name = re.sub(r'[^\x20-\x7E]', '', nb_.decode('latin1','replace')).strip()
                    ww   = struct.unpack_from('<H', data, th+80)[0]
                    hh   = struct.unpack_from('<H', data, th+82)[0]
                    comp = struct.unpack_from('<B', data, th+87)[0]  # compressionFlags
                    dsz  = struct.unpack_from('<I', data, th+88)[0]
                    pd   = data[th+92:th+92+dsz]
                    # Xbox DXT1: 0x0B (linear) or 0x0C (swizzled)
                    # Xbox DXT3: 0x0E/0x0F, DXT5: 0x10/0x11
                    if comp in (0x0B, 0x0C):
                        rgba = decode_dxt1(pd, ww, hh)
                    elif comp in (0x0E, 0x0F, 0x10, 0x11):
                        from apps.methods.txd_reader import decompress_dxt
                        rgba = decompress_dxt(pd, ww, hh, 'DXT3' if comp < 0x10 else 'DXT5')
                    else:
                        rgba = RadarTxdReader._raw_to_rgba(pd, ww, hh,
                            struct.unpack_from('<I', data, th+72)[0])
                    return rgba, ww, hh, name

                #    PS2 (platform 6 or FourCC "PS2\0")                       
                # Header layout (body starts at th):
                # +0   platformId  +4  filterMode  +8  uv_addr  +12 padding
                # +16  name[32]    +48 maskName[32]
                # +80  rasterFormat  +84 depth  +85 width_log2  +86 height_log2
                # +87  numLevels  +88 rasterType  +89 paletteFormat
                # +90  hasAlpha   +91 isCubeMap   +92 gpuDataSize
                # +96  skyMipMapValue (SA only)    +100 pixel data
                elif platform in (6, 0x00325350):
                    nb_ = data[th+16:th+48]
                    nb_ = nb_[:nb_.index(b'\x00')] if b'\x00' in nb_ else nb_
                    name       = re.sub(r'[^\x20-\x7E]', '', nb_.decode('latin1','replace')).strip()
                    raster_fmt = struct.unpack_from('<I', data, th+80)[0]
                    wlog2      = data[th+85]
                    hlog2      = data[th+86]
                    pal_fmt    = data[th+89]
                    gpu_sz     = struct.unpack_from('<I', data, th+92)[0]
                    # Dimensions: log2 if value in 1-11, else raw (older format)
                    ww = (1 << wlog2) if 1 <= wlog2 <= 11 else wlog2
                    hh = (1 << hlog2) if 1 <= hlog2 <= 11 else hlog2
                    # Pixel data at +100 (SA adds skyMipMapValue u32 at +96)
                    # Always skip +96 u32 to handle both GTA III/VC and SA PS2
                    pd = data[th+100:th+100+gpu_sz] if gpu_sz > 0 else data[th+100:]
                    pix_bits = (raster_fmt >> 8) & 0xF
                    if pal_fmt in (1, 2) or pix_bits == 0:   # PAL8 / PAL4
                        rgba = RadarTxdReader._ps2_pal_to_rgba(pd, ww, hh, pal_fmt)
                    elif pix_bits == 2:                        # RGB565
                        rgba = RadarTxdReader._raw_to_rgba(pd, ww, hh, raster_fmt)
                    elif pix_bits == 5:                        # ARGB8888
                        rgba = RadarTxdReader._raw_to_rgba(pd, ww, hh, raster_fmt)
                    else:
                        rgba = RadarTxdReader._raw_to_rgba(pd, ww, hh, raster_fmt)
                    return rgba, ww, hh, name

                #    PC D3D8 / D3D9 / mobile (platform 8/9): shared reader
                else:
                    from apps.methods.txd_reader import parse_native_texture
                    from apps.methods.txd_versions import detect_txd_version
                    tex = parse_native_texture(data, pos, 0, detect_txd_version(data)[0], levels=False)
                    return bytes(tex['rgba_data']), tex['width'], tex['height'], tex['name']

            if ss == 0:
                break
            pos += 12 + ss
        raise ValueError("No Texture Native section")

    @staticmethod
    def native_kind(data) -> str: #vers 1
        """'inplace' (PSP/PS2 natives), 'mobile' (War Drum III/VC) or '' (PC/Xbox)."""
        from apps.methods.txd_lc_android import detect_lc_android_txd
        if len(data) > 56 and data[52:56] in (b'PSP\0', b'PS2\0'):
            return 'inplace'
        return 'mobile' if detect_lc_android_txd(data) else ''

    @staticmethod
    def _native_textures(data) -> list: #vers 1
        """Every texture of a PSP/PS2 or War Drum TXD, tagged for in-place rewrite."""
        from apps.methods.txd_splice import tag_loaded_texture
        if RadarTxdReader.native_kind(data) == 'mobile':
            from apps.methods.txd_lc_android import parse_lc_android_txd
            texs = parse_lc_android_txd(data)
        else:
            from apps.methods.txd_reader import read_psp_txd
            texs = read_psp_txd(data)
        for t in texs:
            tag_loaded_texture(t)
        return texs

    @staticmethod
    def _raw_to_rgba(pd: bytes, w: int, h: int, raster_fmt: int) -> bytes: #vers 2
        """Convert raw uncompressed pixel data to RGBA8888.
        raster_fmt bits 8-11: 0=PAL, 1=ARGB1555, 2=RGB565, 3=ARGB4444,
                               4=LUM8, 5=ARGB8888, 6=RGB888"""
        pix_bits = (raster_fmt >> 8) & 0xF
        n = w * h
        out = bytearray(n * 4)
        if (pix_bits == 5 or pix_bits == 0) and len(pd) >= n * 4:  # ARGB8888/RGBA8888
            for i in range(min(n, len(pd)//4)):
                b, g, r, a = pd[i*4], pd[i*4+1], pd[i*4+2], pd[i*4+3]
                out[i*4:i*4+4] = [r, g, b, a if a > 0 else 255]
        elif pix_bits == 2 and len(pd) >= n * 2:  # RGB565
            for i in range(min(n, len(pd)//2)):
                v = struct.unpack_from('<H', pd, i*2)[0]
                r = ((v >> 11) & 0x1F) << 3; r |= r >> 5
                g = ((v >> 5)  & 0x3F) << 2; g |= g >> 6
                b = (v & 0x1F) << 3;          b |= b >> 5
                out[i*4:i*4+4] = [r, g, b, 255]
        elif pix_bits == 1 and len(pd) >= n * 2:  # ARGB1555
            for i in range(min(n, len(pd)//2)):
                v = struct.unpack_from('<H', pd, i*2)[0]
                a = 255 if (v >> 15) else 0
                r = ((v >> 10) & 0x1F) << 3; r |= r >> 5
                g = ((v >> 5)  & 0x1F) << 3; g |= g >> 5
                b = (v & 0x1F) << 3;          b |= b >> 5
                out[i*4:i*4+4] = [r, g, b, a]
        elif pix_bits == 3 and len(pd) >= n * 2:  # ARGB4444
            for i in range(min(n, len(pd)//2)):
                v = struct.unpack_from('<H', pd, i*2)[0]
                a = ((v >> 12) & 0xF) * 17
                r = ((v >> 8)  & 0xF) * 17
                g = ((v >> 4)  & 0xF) * 17
                b = (v & 0xF) * 17
                out[i*4:i*4+4] = [r, g, b, a]
        elif pix_bits == 6 and len(pd) >= n * 3:  # RGB888
            for i in range(min(n, len(pd)//3)):
                out[i*4:i*4+3] = pd[i*3+2], pd[i*3+1], pd[i*3]
                out[i*4+3] = 255
        elif len(pd) >= n * 4:  # fallback: treat as RGBA8888
            out[:n*4] = pd[:n*4]
        elif len(pd) >= n * 2:  # fallback: treat as RGB565
            for i in range(min(n, len(pd)//2)):
                v = struct.unpack_from('<H', pd, i*2)[0]
                r = ((v >> 11) & 0x1F) << 3
                g = ((v >> 5)  & 0x3F) << 2
                b = (v & 0x1F) << 3
                out[i*4:i*4+4] = [r, g, b, 255]
        return bytes(out)

    @staticmethod
    def _ps2_pal_to_rgba(pd: bytes, w: int, h: int, pal_fmt: int) -> bytes: #vers 1
        """Decode PS2 paletted texture (PAL8/PAL4) to RGBA8888.
        PS2 palette entries are RGBA (not BGRA). Alpha is 0-128 scale (halved from PC).
        Note: PS2 GS swizzle is NOT undone here — output may appear scrambled for
        swizzled textures. Full unswizzle requires GS block layout tables."""
        if pal_fmt == 1:  # PAL8: 256 * 4 bytes palette, then indices
            pal_size = 256 * 4
            pal = pd[:pal_size]
            idx = pd[pal_size:pal_size + w * h]
        else:  # PAL4: 16 * 4 bytes palette, then 4bpp indices
            pal_size = 16 * 4
            pal = pd[:pal_size]
            # Unpack 4bpp: each byte = two pixels
            raw = pd[pal_size:]
            idx = bytearray()
            for b in raw[:((w * h + 1) // 2)]:
                idx.append(b & 0x0F)
                idx.append((b >> 4) & 0x0F)
            idx = bytes(idx[:w*h])

        out = bytearray(w * h * 4)
        for i, pi in enumerate(idx[:w*h]):
            po = pi * 4
            if po + 3 < len(pal):
                r, g, b, a = pal[po], pal[po+1], pal[po+2], pal[po+3]
                # PS2 alpha is 0-128; scale to 0-255
                a = min(255, a * 2)
                out[i*4:i*4+4] = [r, g, b, a]
        return bytes(out)


class ImgReader:
    def __init__(self,img_path): #vers 1
        self.img_path=img_path; self.entries=[]; self._img_data=b''; self._load()
    def _load(self): #vers 4
        import mmap
        p=Path(self.img_path)
        with open(p, 'rb') as fh:            # mapped: big archives are not read whole
            raw = mmap.mmap(fh.fileno(), 0, access=mmap.ACCESS_READ)
        if raw[:4]==b'VER2':
            # VER2 (GTA SA PC / Android): 8-byte header + 32-byte entries
            # Entry: offset_sectors(4) + streaming_size(2) + size(2) + name(24)
            n=struct.unpack_from('<I',raw,4)[0]
            for i in range(n):
                os2,ss,sz2,nb=struct.unpack_from('<IHH24s',raw,8+i*32)
                # Strip null bytes AND any non-printable trailing chars
                # Strip at first null, then remove any non-printable chars
                nb2 = nb[:nb.index(b'\x00')] if b'\x00' in nb else nb
                name = re.sub(r'[^\x20-\x7E]', '', nb2.decode('latin1','replace')).strip()
                self.entries.append({'name':name,'offset':os2*2048,'size':(sz2 or ss)*2048})
            self._img_data=raw
        elif raw[:4] in (b'VER1', b'ver1'):
            # VER1 explicitly tagged (rare) — treat as V1+dir
            self._load_v1_dir(p, raw)
        else:
            # V1 (GTA III/VC/SOL): separate .dir file, 32-byte entries
            # Entry: offset_sectors(4) + size_sectors(4) + name(24)
            self._load_v1_dir(p, raw)

    def _load_v1_dir(self, p, raw): #vers 1
        """Load V1 IMG from companion .dir file. Also handles embedded-dir V1.5."""
        # Look for .dir companion
        dp = p.with_suffix('.dir')
        if not dp.exists():
            # Try same name but different case
            for candidate in p.parent.iterdir():
                if candidate.stem.lower() == p.stem.lower() and candidate.suffix.lower() == '.dir':
                    dp = candidate; break
        if not dp.exists():
            raise FileNotFoundError(
                f"No .dir file found for {p.name}\n"
                f"V1/SOL IMG archives require a companion .dir file.\n"
                f"Expected: {dp.name}")
        dr = dp.read_bytes()
        for i in range(len(dr)//32):
            os2,sz2,nb=struct.unpack_from('<II24s',dr,i*32)
            nb2 = nb[:nb.index(b'\x00')] if b'\x00' in nb else nb
            name = re.sub(r'[^\x20-\x7E]', '', nb2.decode('latin1','replace')).strip()
            if name:
                self.entries.append({'name':name,'offset':os2*2048,'size':sz2*2048})
        self._img_data=raw

    def get_entry_data(self,e): #vers 2
        return bytes(self._img_data[e['offset']:e['offset']+e['size']])

    def find_radar_entries(self, pat): #vers 2
        """Find entries matching radar pattern. Strips names before matching."""
        p = re.compile(pat, re.IGNORECASE)
        results = []
        for e in self.entries:
            # Strip whitespace and null chars that might survive name parsing
            clean = e['name'].strip().rstrip('\x00').strip()
            if p.match(clean):
                results.append(e)
        return results

    def list_radar_like(self, prefix='radar'): #vers 1
        """Debug helper — list all entries whose name starts with prefix."""
        return [e['name'] for e in self.entries
                if e['name'].lower().startswith(prefix.lower())]


#    World bounds per game (GTA world units covered by the full radar map)      
# Format: (world_min_x, world_max_x, world_min_y, world_max_y)
# Tile grid origin is top-left = (world_min_x, world_max_y) — Y flips screen↔world
_GAME_WORLD_BOUNDS = {
    "III PC":   (-2000.0, 2000.0, -2000.0, 2000.0),
    "VC PC":    (-2000.0, 2000.0, -2000.0, 2000.0),
    "SA PC":    (-3000.0, 3000.0, -3000.0, 3000.0),
    "LCS PC":   (-2000.0, 2000.0, -2000.0, 2000.0),
    "VCS PC":   (-2000.0, 2000.0, -2000.0, 2000.0),
    "SOL":      (-6000.0, 6000.0, -6000.0, 6000.0),
    "III And":  (-2000.0, 2000.0, -2000.0, 2000.0),
    "VC And":   (-2000.0, 2000.0, -2000.0, 2000.0),
    "SA And":   (-3000.0, 3000.0, -3000.0, 3000.0),
    "LCS iOS":  (-2000.0, 2000.0, -2000.0, 2000.0),
    "SA iOS":   (-3000.0, 3000.0, -3000.0, 3000.0),
    "LCS PSP":  (-2000.0, 2000.0, -2000.0, 2000.0),
    "VCS PSP":  (-2000.0, 2000.0, -2000.0, 2000.0),
    "Custom":   (-3000.0, 3000.0, -3000.0, 3000.0),
}


class PathLogicMixin: #vers 1
    """Radar loading, game presets, zoom, recent files."""



    # - Left panel: session list
    def get_world_bounds(self): #vers 2
        """Return (min_x, max_x, min_y, max_y) world-unit coverage for the
        current game preset, adjusted by any world offset set via set_world_offset().
        Used by IPLMapView for radar image overlay."""
        base = (-3000.0, 3000.0, -3000.0, 3000.0)
        for key, preset in GAME_PRESETS.items():
            if preset is self._game_preset:
                base = _GAME_WORLD_BOUNDS.get(key, base)
                break
        ox, oy = getattr(self, '_world_offset', (0.0, 0.0))
        return (base[0]+ox, base[1]+ox, base[2]+oy, base[3]+oy)


    def _on_game_changed(self, game): #vers 3
        if game not in GAME_PRESETS:
            game = "Custom"
        cust = (game == "Custom")
        p = GAME_PRESETS[game]
        for s in [self._cols_spin, self._rows_spin]:
            s.setEnabled(cust)
        # Always sync spinner values to preset (fixes VC staying at SA dimensions)
        if hasattr(self, '_cols_spin'):
            self._cols_spin.blockSignals(True)
            self._cols_spin.setValue(p["cols"])
            self._cols_spin.blockSignals(False)
        if hasattr(self, '_rows_spin'):
            self._rows_spin.blockSignals(True)
            self._rows_spin.setValue(p["rows"])
            self._rows_spin.blockSignals(False)
        if hasattr(self, '_game_combo') and self._game_combo.currentText() != game:
            self._game_combo.blockSignals(True)
            self._game_combo.setCurrentText(game)
            self._game_combo.blockSignals(False)
        self._apply_preset(game)


    def _on_custom_changed(self): #vers 1
        if self._game_combo.currentText() == "Custom":
            c=self._cols_spin.value(); r=self._rows_spin.value()
            GAME_PRESETS["Custom"].update({"cols":c,"rows":r,"count":c*r})
            self._apply_preset("Custom")


    def _apply_preset(self, game): #vers 6
        self._game_preset = GAME_PRESETS[game]
        cols  = self._game_preset["cols"]
        count = self._game_preset["count"]
        label = self._game_preset["label"]
        self._tile_rgba = {}; self._current_idx = -1
        self._tw = TILE_W; self._th = TILE_H   # reset to native on new file
        names = [self._game_preset["name_fn"](i) for i in range(count)]
        self._radar.setup(cols, count, names)

        hint = self._game_preset.get("hint", "")
        self._set_status(f"{label} — {hint}" if hint else
                         f"{label} — {count} tiles ({cols}×{self._game_preset['rows']})")


   # - File ops

    def _open_file(self, path: str = ""): #vers 7
        """Open radar IMG/TXD/PVR file. Pass path to skip the file dialog."""
        source = self._game_preset.get("img_source", "img")
        hint   = self._game_preset.get("hint", "")

        # Formats not yet supported — show info and return
        if source in ("chk", "xtx"):
            label = self._game_preset.get("label", "")
            QMessageBox.information(self, "Format Not Yet Supported",
                f"{label}\n\n{hint}\n\n"
                f"This format ({source.upper()}) is not yet supported in Path Workshop.\n"
                f"Support is planned for a future build.")
            return

        if not path:
            # Build file filter based on source
            if source == "txd":
                filt  = "TXD Files (*.txd);;All Files (*)"
                title = "Open Radar TXD"
            elif source == "toc":
                filt  = "Texture DB (*.txt *.toc *.dat);;All Files (*)"
                title = "Open Radar Texture DB"
            elif source == "pvr":
                filt  = "PVR IMG Archives (*.pvr *.img);;All Files (*)"
                title = "Open Radar PVR"
            else:  # img
                filt  = "IMG Archives (*.img);;All Files (*)"
                title = "Open Radar IMG"
            path,_ = QFileDialog.getOpenFileName(self, title, self._dialog_dir(), filt)
            if not path: return
        self._remember_dir(path)

        # Standalone .txd files
        if path.lower().endswith('.txd'):
            self._set_native_tile_size(128)
            self._load_standalone_txd(path)
            return

        # GTA IV radar.img (V3, usually encrypted)
        if self._is_iv_img(path):
            self._load_iv_radar(path)
            return
        # Mobile texture database (SA iOS/Android: radarNN in gta3.txt/.toc/.dat)
        from apps.methods.mobile_texture_db import detect_mobile_db
        if detect_mobile_db(path):
            self._load_texdb_radar(path)
            return
        self._set_native_tile_size(128)

        # IMG / PVR — both use ImgReader (VER2 or V1+dir)
        try: self._img_reader = ImgReader(path); self._img_path = path
        except Exception as e: QMessageBox.critical(self, "Load Error", str(e)); return

        # Search using a broad pattern first — find ALL radar*.txd entries
        # Then use tile COUNT to auto-detect the game (authoritative)
        fname = Path(path).name.lower()

        # Use broad search pattern to find everything
        broad_pat = r'^radar(\d{2,4})\.txd$'
        entries = self._img_reader.find_radar_entries(broad_pat)

        if not entries:
            # Nothing found — try current preset pattern as fallback
            entries = self._img_reader.find_radar_entries(self._game_preset["img_pattern"])

        if not entries:
            radar_like = self._img_reader.list_radar_like('radar')
            sample = ', '.join(radar_like[:6]) if radar_like else 'none'
            QMessageBox.warning(self, "No Radar Tiles",
                f"No radar TXDs found in {Path(path).name}\n\n"
                f"Radar-like entries: {sample}\n"
                f"Ensure you are loading the correct .img file for your game.")
            return

        def _tile_sort_key(e):
            m = re.search(r'(\d+)', e["name"])
            return int(m.group(1)) if m else 0
        entries.sort(key=_tile_sort_key)

        # Auto-detect game by tile count — most reliable signal
        self._autodetect(len(entries))

        # Cap to detected preset count (removes extra files like radarmap, radardisc etc)
        p = self._game_preset
        entries = entries[:p["count"]]

        # For SOL, override with correct radartex pattern to avoid non-radar files
        if fname == 'radartex.img':
            self._on_game_changed('SOL')
            p = self._game_preset
            entries_sol = self._img_reader.find_radar_entries(r'^radar\d{4}\.txd$')
            if entries_sol:
                entries = entries_sol[:p["count"]]

        self._tile_entries = entries
        prog=QProgressDialog("Loading tiles…","Cancel",0,len(entries),self)
        prog.setWindowModality(Qt.WindowModality.WindowModal); prog.show()
        for i,entry in enumerate(entries):
            prog.setValue(i); QApplication.processEvents()
            if prog.wasCanceled(): break
            try:
                rgba,w,h,_=RadarTxdReader.read(self._img_reader.get_entry_data(entry))
                self._tile_rgba[i]=rgba; self._radar.set_tile(i,rgba,w,h)
            except Exception as e: print(f"WARN tile {i}: {e}",file=sys.stderr)
        prog.setValue(len(entries))
        self.RAD_settings.add_recent(str(path))
        self._set_status(f"Loaded {len(entries)} tiles from {Path(path).name}  — game: {self._game_preset['label']}  grid: {self._game_preset['cols']}×{self._game_preset['rows']}")

    def _set_native_tile_size(self, size: int, special: bool = False): #vers 2
        """Module tile size: IV 512 / texture DB tiles set it; other files reset to 128."""
        import apps.components.Radar_Editor.radar_workshop as _rw_mod
        if special or getattr(self, '_special_tiles', False):
            _rw_mod.TILE_W = _rw_mod.TILE_H = size
            self._tw = self._th = size
        self._special_tiles = special
        if not special:
            self._iv_img, self._iv_entries, self._texdb = None, {}, None

    def _is_iv_img(self, path: str) -> bool: #vers 1
        """True for a GTA IV (V3) IMG archive."""
        from apps.methods.img_core_classes import IMGFile, IMGVersion
        if not path.lower().endswith('.img'):
            return False
        try:
            img = IMGFile(path)
            return bool(img.open()) and img.version in (IMGVersion.VERSION_3, IMGVersion.VERSION_3_ENC)
        except Exception:
            return False

    def _load_iv_radar(self, path: str): #vers 1
        """Open GTA IV radar.img: radarN.wtd tiles on a 12x12 grid."""
        from apps.methods.img_core_classes import IMGFile
        from apps.methods.xtd_textures import parse_iv_wtd
        from PIL import Image
        img = IMGFile(path)
        if not img.open():
            QMessageBox.critical(self, "Load Error", f"Cannot open {Path(path).name}:\n"
                                 f"{getattr(img, 'last_error', '')}")
            return
        entries = {}
        for e in img.entries:
            m = re.fullmatch(r'radar(\d+)\.wtd', e.name.lower())
            if m and int(m.group(1)) < GAME_PRESETS["IV PC"]["count"]:
                entries[int(m.group(1))] = e
        if not entries:
            QMessageBox.warning(self, "No Radar Tiles",
                f"No radarN.wtd entries found in {Path(path).name}")
            return
        self._set_native_tile_size(IV_TILE_SIZE, special=True)
        self._on_game_changed("IV PC")
        self._img_reader, self._img_path = None, path
        self._iv_img, self._iv_entries = img, entries
        self._iv_formats = {}
        self._tile_entries = [{"name": f"radar{i}.wtd", "offset": 0, "size": 0}
                              for i in range(GAME_PRESETS["IV PC"]["count"])]
        prog = QProgressDialog("Loading tiles…", "Cancel", 0, len(entries), self)
        prog.setWindowModality(Qt.WindowModality.WindowModal); prog.show()
        for n, (i, e) in enumerate(sorted(entries.items())):
            prog.setValue(n); QApplication.processEvents()
            if prog.wasCanceled():
                break
            try:
                tex = parse_iv_wtd(img.read_entry_data(e))[0]
                rgba, w, h = tex['rgba_data'], tex['width'], tex['height']
                if (w, h) != (IV_TILE_SIZE, IV_TILE_SIZE):
                    rgba = Image.frombytes("RGBA", (w, h), rgba).resize(
                        (IV_TILE_SIZE, IV_TILE_SIZE)).tobytes()
                self._iv_formats[i] = (tex['name'], w, h)
                self._tile_rgba[i] = rgba
                self._radar.set_tile(i, rgba, IV_TILE_SIZE, IV_TILE_SIZE)
            except Exception as ex:
                print(f"WARN IV tile {i}: {ex}", file=sys.stderr)
        prog.setValue(len(entries))
        self.RAD_settings.add_recent(str(path))
        self._set_status(f"Loaded {len(entries)} GTA IV tiles from {Path(path).name}  grid: 12×12")

    def _load_texdb_radar(self, path: str): #vers 1
        """Radar tiles (radarNN) from a mobile texture DB, grid by tile count."""
        from PIL import Image
        from apps.methods.mobile_texture_db import load_mobile_texture_db
        from apps.methods.mobile_texture_decode import decode_mobile_texture
        db = load_mobile_texture_db(path, load_pixel_data=True)
        tiles = {}
        for t in (db.textures if db else []):
            m = re.fullmatch(r'radar(\d+)', t.name.lower())
            if m and not t.is_affiliate:
                tiles[int(m.group(1))] = t
        if not tiles:
            QMessageBox.warning(self, "No Radar Tiles", f"No radarNN textures in {Path(path).name}")
            return
        size = max(t.width for t in tiles.values())
        self._set_native_tile_size(size, special=True)
        self._autodetect(max(tiles) + 1)
        self._img_reader, self._img_path, self._texdb = None, path, db
        self._texdb_tiles = tiles
        self._tile_entries = [{"name": f"radar{i:02d}", "offset": 0, "size": 0}
                              for i in range(self._game_preset["count"])]
        for i, t in tiles.items():
            if i >= self._game_preset["count"]:
                continue
            rgba = decode_mobile_texture(t)
            if (t.width, t.height) != (size, size):
                rgba = Image.frombytes("RGBA", (t.width, t.height), rgba).resize((size, size)).tobytes()
            self._tile_rgba[i] = rgba
            self._radar.set_tile(i, rgba, size, size)
        self.RAD_settings.add_recent(str(path))
        self._set_status(f"Loaded {len(tiles)} tiles from texture DB {db.name} ({db.platform})  "
                         f"grid: {self._game_preset['cols']}×{self._game_preset['rows']}")

    def _load_standalone_txd(self, path: str): #vers 1
        """Load a standalone .txd file (GTA III / VC radar — single texture file)."""
        try:
            data = Path(path).read_bytes()
            rgba, w, h, tex_name = RadarTxdReader.read(data)
        except Exception as e:
            QMessageBox.critical(self, "TXD Load Error", str(e))
            return

        # Single texture — show as 1x1 grid
        self._img_reader   = None
        self._img_path     = path
        self._tile_entries = [{"name": tex_name, "offset": 0, "size": len(data)}]
        GAME_PRESETS["Custom"].update({"cols": 1, "rows": 1, "count": 1})
        self._on_game_changed("Custom")
        self._cols_spin.setValue(1); self._rows_spin.setValue(1)

        self._tile_rgba[0] = rgba
        self._radar.set_tile(0, rgba, w, h)
        self._set_status(
            f"Loaded standalone TXD: {tex_name}  {w}×{h}px  from {Path(path).name}")


    def _autodetect(self, count): #vers 4
        """Detect preset by tile count — authoritative lookup table.
        SA=144(12x12)  III/VC/LCS/VCS=64(8x8)  SOL=1296(36x36)
        Prefers PC variants over mobile."""
        # Exact count match — prefer PC variants
        pc_order = ["SA PC","III PC","VC PC","LCS PC","VCS PC","SOL",
                    "LCS iOS","III And","VC And","SA And","LCS PSP","VCS PSP"]
        for game in pc_order:
            p = GAME_PRESETS.get(game, {})
            if p.get("count") == count:
                self._on_game_changed(game)
                return

        # Near-match (within 12 tiles of a PC preset)
        for game in pc_order:
            p = GAME_PRESETS.get(game, {})
            if game == "Custom": continue
            if abs(p.get("count", 0) - count) <= 12:
                self._on_game_changed(game)
                self._set_status(
                    f"Loaded {count} tiles (nearest preset: {p['label']} "
                    f"{p['cols']}×{p['rows']}) — adjust W/H if wrong")
                return

        # Unknown — sqrt grid
        cols = max(1, round(math.sqrt(count)))
        rows = (count + cols - 1) // cols
        GAME_PRESETS["Custom"].update({"cols": cols, "rows": rows, "count": count})
        self._on_game_changed("Custom")
        self._set_status(
            f"Loaded {count} tiles — unknown layout, using {cols}×{rows} "
            f"(adjust W/H spinners if wrong)")

    def _open_radar_image(self, path=None): #vers 2
        """Whole-map radar image (PNG/JPG/BMP) cut into the current game's grid."""
        if not path:
            path, _ = QFileDialog.getOpenFileName(self, "Load Radar Image", self._dialog_dir(),
                                                  "Images (*.png *.jpg *.jpeg *.bmp *.tga);;All Files (*)")
        if not path:
            return
        self._remember_dir(path)
        from PIL import Image
        try:
            im = Image.open(path).convert("RGBA")
        except Exception as e:
            QMessageBox.critical(self, "Load Radar Image", f"{Path(path).name}:\n{e}")
            return
        p = self._game_preset
        cols, rows = p["cols"], p.get("rows", p["count"] // max(1, p["cols"]))
        tile = max(1, min(im.width // cols, im.height // rows))
        im = im.resize((tile * cols, tile * rows))
        self._apply_preset(self._game_combo.currentText())
        self._img_reader, self._img_path = None, path
        for i in range(min(p["count"], cols * rows)):
            x, y = (i % cols) * tile, (i // cols) * tile
            rgba = im.crop((x, y, x + tile, y + tile)).tobytes()
            self._tile_rgba[i] = rgba
            self._radar.set_tile(i, rgba, tile, tile)
        self._tw = self._th = tile
        self.RAD_settings.add_recent(str(path))
        self._set_status(f"Loaded radar image {Path(path).name}  {im.width}x{im.height}  as {cols}x{rows} tiles")

    # - Grid nav
    def _zoom(self, f): #vers 4
        """Zoom the radar view around its centre."""
        r = getattr(self, '_radar', None)
        if r is None:
            return
        cx, cy, old = r.width() / 2, r.height() / 2, r._ts()
        r._zoom_factor = max(0.1, min(r.MAX_ZOOM, r._zoom_factor * f))
        new = r._ts()
        r._pan_x = int(cx - (cx - r._pan_x) * new / max(1, old))
        r._pan_y = int(cy - (cy - r._pan_y) * new / max(1, old))
        r.update()

    def _apply_grid_zoom(self): #vers 1
        """Apply current zoom level to radar grid size."""
        if not hasattr(self, '_grid_zoom'): self._grid_zoom = 1.0
        sc = getattr(self, '_radar_scroll', None)
        if sc is None:
            # find scroll area parent
            p = self._radar.parent()
            while p:
                from PyQt6.QtWidgets import QScrollArea
                if isinstance(p, QScrollArea): sc = p; break
                p = p.parent()
        if sc is None: return
        vp_w = sc.viewport().width()
        vp_h = sc.viewport().height()
        preset = self._game_preset
        cols = preset["cols"]; rows = preset.get("rows", 8)
        if self._grid_zoom <= 1.0:
            # Fit mode — let scroll area control size
            self._radar.setMinimumSize(0, 0)
        else:
            # Fixed size mode — set explicit size
            cell = max(4, int(self._grid_zoom * min(vp_w // cols, vp_h // rows)))
            self._radar.setMinimumSize(cell * cols, cell * rows)
        self._radar.update()

    def _fit(self): #vers 3
        """Reset zoom/pan to fit all tiles."""
        if hasattr(self, '_radar'):
            self._radar._zoom_factor = 1.0
            self._radar._pan_x = 0
            self._radar._pan_y = 0
            self._radar.update()
        return


    # - Info / Settings / Theme
    def _open_recent(self, path: str): #vers 2
        """Open a recently used IMG file — uses same logic as _open_file."""
        import os
        if not os.path.isfile(path):
            QMessageBox.warning(self, "File Not Found",
                f"Cannot find:\n{path}\n\nRemoving from recent files.")
            self.RAD_settings._data['recent_files'] = [
                p for p in self.RAD_settings.get_recent() if p != path]
            self.RAD_settings.save()
            return
        # Delegate to _open_file which handles broad-pattern search + autodetect
        self._open_file(path)

    def _clear_recent(self): #vers 1
        self.RAD_settings._data['recent_files'] = []
        self.RAD_settings.save()
        self._set_status("Recent files cleared")
