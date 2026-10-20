#!/usr/bin/env python3
#this belongs in apps/components/Path_Workshop/path_workshop.py - Version: 8
# X-Seti - Apr 2026 - IMG Factory 1.6 - Path Workshop


import os, json, sys, threading, struct, re, math, shutil
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

if sys.platform.startswith('linux'):   # xcb only exists on Linux
    os.environ['QT_QPA_PLATFORM'] = 'xcb'
    os.environ['QSG_RHI_BACKEND'] = 'opengl'

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from apps.methods.ribbon_system import RibbonMixin
from PyQt6.QtWidgets import (
    QAbstractItemView, QApplication, QCheckBox, QComboBox,
    QDialog, QDoubleSpinBox, QFileDialog, QFontComboBox,
    QFormLayout, QFrame, QGroupBox, QHBoxLayout, QLabel,
    QLineEdit, QListWidget, QListWidgetItem, QMenu,
    QMessageBox, QProgressDialog, QPushButton, QScrollArea,
    QSizePolicy, QSlider, QSpinBox, QSplitter, QStatusBar,
    QTabWidget, QTextEdit, QToolButton, QVBoxLayout, QWidget
)

from PyQt6.QtCore import pyqtSignal, Qt, QPoint, QSize, QThread, QTimer
from PyQt6.QtGui import  QAction, QBrush, QColor, QFont, QIcon, QImage, QKeySequence, QPainter, QPainterPath, QPen, QPixmap, QShortcut
from apps.methods.img_factory_settings import get_user_config_dir

from apps.methods.dock_panels import DockPanelsMixin, add_outer_dock, make_outer_mw
from apps.components.Path_Workshop.depends.path_nodes_func import PathNodesMixin


# - Detect standalone vs docked
def _is_standalone():
    import inspect
    frame = inspect.currentframe()
    try:
        for _ in range(10):
            frame = frame.f_back
            if frame is None: break
            if 'imgfactory' in frame.f_code.co_filename.lower(): return False
        return True
    finally:
        del frame

STANDALONE_MODE = _is_standalone()
DEBUG_STANDALONE = False
App_name  = "Path Workshop"
App_build = "Oct 2026"
App_auth  = "X-Seti"
Build     = "Build 2"


#    Infrastructure imports
try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    ICONS_AVAILABLE = False
    class SVGIconFactory:
        @staticmethod
        def settings_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def properties_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def info_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def open_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def save_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def minimize_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def maximize_icon(s=20, c='#fff'): return QIcon()
        @staticmethod
        def close_icon(s=20, c='#fff'): return QIcon()

try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    AppSettings = None

try:
    from apps.gui.tool_menu_mixin import ToolMenuMixin
except ImportError:
    class ToolMenuMixin:
        def get_menu_title(self): return App_name
        def _build_menus_into_qmenu(self, m): pass
        def _get_tool_menu_style(self): return 'dropdown'


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


# - Radar grid widget
class RadarGridWidget(QWidget):

    def _get_ui_color(self, key): #vers 3
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)
    """Radar tiles drawn as one map image, path nodes on top."""

    def __init__(self,parent=None): #vers 4
        super().__init__(parent)
        self._cols=8; self._count=0; self._tiles={}; self._dirty=set()
        self._alpha={}; self._scaled={}      # per tile: has transparency, (size, scaled image)
        self._names=[]
        self._zoom_factor = 1.0      # manual zoom (1.0 = fit)
        self._pan_x = 0; self._pan_y = 0
        self._workshop   = None      # set by PathWorkshop after creation
        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.WheelFocus)
        self.setSizePolicy(QSizePolicy.Policy.Expanding,QSizePolicy.Policy.Expanding)

    def setup(self,cols,count,names=None): #vers 2
        self._cols=max(1,cols); self._count=count; self._tiles={}; self._dirty=set()
        self._alpha={}; self._scaled={}
        self._names=names or [f"Tile {i}" for i in range(count)]; self.update()


    def paint_paths(self, p: QPainter): #vers 3
        """Path links and nodes over the radar image."""
        ws = self._workshop
        recs = getattr(ws, '_pn', None) if ws else None
        if not recs:
            return
        from PyQt6.QtCore import QLineF, QPointF
        w2s, vis = self.world_to_screen, ws._pn_visible
        pts = [w2s(r.x, r.y) for r in recs]
        W, H = self.width(), self.height()
        on = lambda x, y: -20 <= x <= W + 20 and -20 <= y <= H + 20
        for ped, col in ((False, QColor(80, 180, 255)), (True, QColor(90, 220, 110)),
                         (None, QColor(255, 170, 40)), ('trk', QColor(200, 200, 210))):
            lines = [QLineF(*pts[a], *pts[b]) for a, b in ws._pn_edges
                     if recs[a].ped == ped and vis(recs[a]) and (on(*pts[a]) or on(*pts[b]))]
            p.setPen(QPen(col, 1.5)); p.drawLines(lines)
            dots = [QPointF(*pts[i]) for i, r in enumerate(recs) if r.ped == ped and vis(r) and on(*pts[i])]
            p.setPen(QPen(col, 4 if self._zoom_factor > 2 else 3)); p.drawPoints(dots)
        for idx, col in ((ws._selected_node, QColor(255, 60, 60)), (ws._link_from, QColor(255, 230, 40))):
            if 0 <= idx < len(recs):
                x, y = pts[idx]
                p.setPen(QPen(col, 2)); p.setBrush(Qt.BrushStyle.NoBrush)
                p.drawEllipse(QPointF(x, y), 6, 6)

    def _map_size(self): #vers 1
        """Whole radar map size in screen pixels."""
        ts = self._ts()
        rows = (self._count + self._cols - 1) // self._cols if self._count else self._cols
        return self._cols * ts, rows * ts

    def world_to_screen(self, wx, wy): #vers 1
        """World units to widget pixels (north up)."""
        x0, x1, y0, y1 = self._workshop.get_world_bounds()
        mw, mh = self._map_size()
        return (self._pan_x + (wx - x0) / (x1 - x0) * mw,
                self._pan_y + (y1 - wy) / (y1 - y0) * mh)

    def _ts(self): #vers 2
        """Tile size in pixels, accounting for zoom factor."""
        if not self._count or not self._cols: return 32
        rows = (self._count + self._cols - 1) // self._cols
        base = min(max(4, self.width()//self._cols), max(4, self.height()//rows))
        return max(4, int(base * self._zoom_factor))

    def set_tile(self,idx,rgba,w,h): #vers 3
        """Store tile as RGBA8888 QImage; note real transparency for the checkerboard."""
        img = QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888).copy()
        self._tiles[idx] = img
        self._alpha[idx] = bytes(rgba[3::4]).count(255) != w * h
        self._scaled.pop(idx, None)
        self.update()

    def screen_to_world(self, pos) -> tuple: #vers 2
        """Widget pixels to world units."""
        x0, x1, y0, y1 = self._workshop.get_world_bounds()
        mw, mh = self._map_size()
        return (x0 + (pos.x() - self._pan_x) / mw * (x1 - x0),
                y1 - (pos.y() - self._pan_y) / mh * (y1 - y0))

    def paintEvent(self,ev): #vers 6
        """Tiles as one image (no grid), then path nodes."""
        p=QPainter(self)
        p.fillRect(self.rect(), self._get_ui_color('viewport_bg'))
        if not self._count:
            self.paint_paths(p); p.end(); return
        ts=self._ts(); cols=self._cols
        px0, py0 = self._pan_x, self._pan_y
        cb = max(4, ts // 8)   # checkerboard cell size relative to tile size
        chk = QPixmap(cb * 2, cb * 2)
        chk.fill(self._get_ui_color('viewport_text'))
        cp = QPainter(chk)
        cp.fillRect(0, 0, cb, cb, self._get_ui_color('border'))
        cp.fillRect(cb, cb, cb, cb, self._get_ui_color('border'))
        cp.end()
        chk_brush = QBrush(chk)
        empty = self._get_ui_color('viewport_bg')
        vis = ev.rect()
        for idx in range(self._count):
            col=idx%cols; row=idx//cols; x=px0+col*ts; y=py0+row*ts
            if x > vis.right() or y > vis.bottom() or x + ts < vis.left() or y + ts < vis.top():
                continue
            if idx in self._tiles:
                cached = self._scaled.get(idx)
                if cached is None or cached[0] != ts:
                    cached = (ts, self._tiles[idx].scaled(ts, ts, Qt.AspectRatioMode.IgnoreAspectRatio,
                                                          Qt.TransformationMode.FastTransformation))
                    self._scaled[idx] = cached
                if self._alpha.get(idx):
                    p.setBrushOrigin(x, y)
                    p.fillRect(x, y, ts, ts, chk_brush)
                p.drawImage(x, y, cached[1])
            else:
                p.fillRect(x, y, ts, ts, empty)
        self.paint_paths(p)
        p.end()

    def mouseMoveEvent(self,ev): #vers 6
        """Drag pans or moves a node; status shows world position."""
        ws = self._workshop
        if getattr(self, '_pan_start', None) is not None:
            d = ev.pos() - self._pan_start
            self._pan_x = self._pan_start_xy[0] + d.x()
            self._pan_y = self._pan_start_xy[1] + d.y()
            self.update()
            return
        wx, wy = self.screen_to_world(ev.pos())
        if ws and ws._drag is not None:
            ws._drag_node_to(wx, wy)
        elif ws:
            ws._set_status(f"World {wx:.1f}, {wy:.1f}")

    def keyPressEvent(self, ev): #vers 2
        """Arrows pan, Home resets, Delete removes the selected node."""
        k = ev.key()
        if k == Qt.Key.Key_Delete and self._workshop:
            self._workshop._delete_selected_node(); return
        step = max(16, min(self.width(), self.height()) // 4)
        moves = {Qt.Key.Key_Left: (step, 0), Qt.Key.Key_Right: (-step, 0),
                 Qt.Key.Key_Up: (0, step), Qt.Key.Key_Down: (0, -step)}
        if k in moves:
            self._pan_x += moves[k][0]; self._pan_y += moves[k][1]
        elif k == Qt.Key.Key_Home:
            self._zoom_factor = 1.0; self._pan_x = self._pan_y = 0
        else:
            return super().keyPressEvent(ev)
        self.update()

    def resizeEvent(self, ev): #vers 1
        """Repaint at the new tile size (some compositors skip it)."""
        super().resizeEvent(ev)
        self.update()

    def wheelEvent(self, ev): #vers 1
        """Scroll wheel: zoom in/out around cursor."""
        delta = ev.angleDelta().y()
        if delta == 0: return
        factor = 1.12 if delta > 0 else (1/1.12)
        cx, cy = ev.position().x(), ev.position().y()
        old_ts = self._ts()
        self._zoom_factor = max(0.1, min(10.0, self._zoom_factor * factor))
        new_ts = self._ts()
        self._pan_x = int(cx - (cx - self._pan_x) * new_ts / max(1, old_ts))
        self._pan_y = int(cy - (cy - self._pan_y) * new_ts / max(1, old_ts))
        self.update()

    def mousePressEvent(self,ev): #vers 6
        """Middle = pan; select/drag, add, link or zoom by tool."""
        is_left = ev.button() == Qt.MouseButton.LeftButton
        ws = self._workshop
        tool = getattr(ws, '_draw_tool', 'none') if ws else 'none'
        self.setFocus()
        if ev.button() == Qt.MouseButton.MiddleButton:
            self._pan_start = ev.pos()
            self._pan_start_xy = (self._pan_x, self._pan_y)
            return
        if is_left and ws and tool in ('none', 'link'):
            hit = ws._pick_node(ev.pos().x(), ev.pos().y())
            if tool == 'link':
                ws._link_click(hit)
            elif hit >= 0:
                ws._begin_node_drag(hit)
            else:
                ws._select_node(-1)
                self._pan_start = ev.pos()               # empty space: pan
                self._pan_start_xy = (self._pan_x, self._pan_y)
            return
        if tool == 'path_node' and is_left and ws:
            ws._add_node_at(*self.screen_to_world(ev.pos()))
            return
        if tool == 'zoom' and ws:
            factor = 1.3 if is_left else (1/1.3)
            cx, cy = ev.pos().x(), ev.pos().y()
            old_ts = self._ts()
            self._zoom_factor = max(0.1, min(10.0, self._zoom_factor * factor))
            new_ts = self._ts()
            self._pan_x = cx - int((cx - self._pan_x) * new_ts / max(1, old_ts))
            self._pan_y = cy - int((cy - self._pan_y) * new_ts / max(1, old_ts))
            self.update()
            return

    def mouseReleaseEvent(self, ev): #vers 4
        """End pan or node drag."""
        self._pan_start = None
        if self._workshop is not None:
            self._workshop._end_node_drag()


#    Per-tool settings                                                          
class RADSettings:
    """Lightweight JSON settings for Path Workshop.
    Stored at ~/.config/imgfactory/path_workshop.json
    Completely separate from the global AppSettings/theme system.
    """
    MAX_RECENT = 10

    DEFAULTS = {
        'show_menubar':            False,
        'menu_style':              'dropdown',
        'menu_bar_font_size':      9,
        'menu_bar_height':         22,
        'menu_dropdown_font_size': 9,
        'show_statusbar':          True,
        'default_game':            'SA PC',
        'recent_files':            [],        # list of recently opened IMG paths
        'window_x':                -1,
        'window_y':                -1,
        'window_w':                1400,
        'window_h':                800,
    }

    def __init__(self): #vers 2
        cfg_dir = get_user_config_dir()
        cfg_dir.mkdir(parents=True, exist_ok=True)
        self._path = cfg_dir / 'path_workshop.json'
        self._data = dict(self.DEFAULTS)
        self._load()

    def _load(self): #vers 1
        try:
            if self._path.exists():
                loaded = json.loads(self._path.read_text())
                self._data.update({k: v for k, v in loaded.items()
                                   if k in self.DEFAULTS})
        except Exception:
            pass

    def save(self): #vers 1
        try:
            self._path.write_text(json.dumps(self._data, indent=2))
        except Exception:
            pass

    def get(self, key, default=None): #vers 1
        return self._data.get(key, default if default is not None
                              else self.DEFAULTS.get(key))

    def set(self, key, value): #vers 1
        if key in self.DEFAULTS:
            self._data[key] = value

    def add_recent(self, path: str): #vers 1
        """Add path to recent files list (max MAX_RECENT entries)."""
        recents = [p for p in self._data.get('recent_files', []) if p != path]
        recents.insert(0, path)
        self._data['recent_files'] = recents[:self.MAX_RECENT]
        self.save()

    def get_recent(self): #vers 1
        """Return list of recent file paths that still exist."""
        import os
        return [p for p in self._data.get('recent_files', []) if os.path.isfile(p)]


#    Radar Palette Widget                                                        
class _CornerOverlay(QWidget):
    """Transparent overlay that draws corner resize triangles on top of all children.
    Uses setMask() so only the triangle pixels exist — fully transparent elsewhere.
    WA_AlwaysStackOnTop keeps it above all sibling widgets on Wayland/KDE."""

    SIZE = 20   # triangle leg size in pixels

    def __init__(self, parent): #vers 3
        super().__init__(parent)
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, True)
        self.setAttribute(Qt.WidgetAttribute.WA_NoSystemBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_AlwaysStackOnTop, True)
        self.setWindowFlags(Qt.WindowType.Widget)
        self._hover_corner = None
        self._app_settings = None
        self.setGeometry(0, 0, parent.width(), parent.height())
        self._update_mask()

    def _update_mask(self): #vers 1
        """Create a mask covering only the four corner triangles."""
        from PyQt6.QtGui import QRegion, QPolygon
        from PyQt6.QtCore import QPoint
        s = self.SIZE
        w, h = self.width(), self.height()
        region = QRegion()
        for pts in [
            [QPoint(0,0),    QPoint(s,0),    QPoint(0,s)],     # top-left
            [QPoint(w,0),    QPoint(w-s,0),  QPoint(w,s)],     # top-right
            [QPoint(0,h),    QPoint(s,h),    QPoint(0,h-s)],   # bottom-left
            [QPoint(w,h),    QPoint(w-s,h),  QPoint(w,h-s)],   # bottom-right
        ]:
            region = region.united(QRegion(QPolygon(pts)))
        self.setMask(region)

    def update_state(self, hover_corner, app_settings): #vers 1
        self._hover_corner = hover_corner
        self._app_settings = app_settings
        self.update()

    def setGeometry(self, *args): #vers 1
        super().setGeometry(*args)
        self._update_mask()

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        self._update_mask()

    def paintEvent(self, event): #vers 2
        s = self.SIZE
        if self._app_settings:
            try:
                colors = self._app_settings.get_theme_colors()
                accent = QColor(colors.get('accent_primary', '#4682FF'))
            except Exception:
                accent = QColor(70, 130, 255)
        else:
            accent = QColor(70, 130, 255)
        accent.setAlpha(200)
        hover_c = QColor(accent); hover_c.setAlpha(255)
        w, h = self.width(), self.height()
        corners = {
            'top-left':     [(0,0),  (s,0),   (0,s)],
            'top-right':    [(w,0),  (w-s,0), (w,s)],
            'bottom-left':  [(0,h),  (s,h),   (0,h-s)],
            'bottom-right': [(w,h),  (w-s,h), (w,h-s)],
        }
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        for name, pts in corners.items():
            path = QPainterPath()
            path.moveTo(*pts[0]); path.lineTo(*pts[1]); path.lineTo(*pts[2])
            path.closeSubpath()
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(hover_c if self._hover_corner == name else accent))
            painter.drawPath(path)
        painter.end()


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


class PathWorkshop(RibbonMixin, PathNodesMixin, DockPanelsMixin, ToolMenuMixin, QWidget): #vers 3
    _ribbon_name = "path_workshop"
    # Bump when the set of ribbons changes (4 = path edit tools)
    _RIBBON_LAYOUT_VERSION = 4
    _OUTER_LAYOUT_VERSION = 1      # Path Nodes / Node Properties docks

    """Path Workshop: GTA paths drawn over the radar map"""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)

    def _build_menus_into_qmenu(self, pm): #vers 4
        fm = pm.addMenu("File")
        fm.addAction("Load Radar (IMG / TXD)…", self._open_file)
        fm.addAction("Load Radar Image…",       self._open_radar_image)
        fm.addSeparator()
        fm.addAction("Load Paths…",             self._load_paths)
        fm.addAction("Save Paths",              self._save_paths)
        fm.addAction("Undo Path Edit",          self._undo_path_edit)
        fm.addSeparator()
        if hasattr(self, 'RAD_settings'):
            recent = self.RAD_settings.get_recent()
            if recent:
                rm = fm.addMenu("Recent Files")
                for rpath in recent:
                    act = rm.addAction(Path(rpath).name)
                    act.setToolTip(rpath)
                    act.triggered.connect(lambda checked=False, p=rpath: self._open_recent(p))
                rm.addSeparator()
                rm.addAction("Clear Recent", self._clear_recent)
        vm = pm.addMenu("View")
        vm.addAction("Zoom In (+)",    lambda: self._zoom(1.25))
        vm.addAction("Zoom Out (-)",   lambda: self._zoom(0.8))
        vm.addAction("Fit Grid",       self._fit)
        vm.addSeparator()
        vm.addAction("About Path Workshop", self._show_about)

    def __init__(self, parent=None, main_window=None): #vers 3
        super().__init__(parent)

        self.main_window        = main_window
        self.standalone_mode    = (main_window is None)
        self.is_docked          = not self.standalone_mode
        self.button_display_mode = 'both'
        self.dock_display_mode   = None

        # Fonts (mirrors COL Workshop)
        self.title_font   = QFont("Arial", 14)
        self.panel_font   = QFont("Arial", 10)
        self.button_font  = QFont("Arial", 10)
        self.chat_font    = QFont("Courier New", 10)
        self.button_display_mode = 'both'
        self.infobar_font = QFont("Courier New", 9)
        self.section_title_font = QFont("Arial", 11, QFont.Weight.Bold)

        # Window chrome
        self.use_system_titlebar  = False
        self.window_always_on_top = False
        self.dragging             = False
        self.drag_position        = None
        self.resizing             = False
        self.resize_corner        = None
        self.corner_size          = 20
        self.hover_corner         = None

        # AppSettings
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:
                self.app_settings = AppSettings()
            except Exception:
                self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        # Per-tool settings (separate from global theme system)
        self.RAD_settings = RADSettings()
        # Restore window geometry
        if self.standalone_mode:
            wx = self.RAD_settings.get('window_x', -1)
            wy = self.RAD_settings.get('window_y', -1)
            ww = self.RAD_settings.get('window_w', 1400)
            wh = self.RAD_settings.get('window_h', 800)
            self.resize(max(800, ww), max(500, wh))
            if wx >= 0 and wy >= 0:
                self.move(wx, wy)

        # Spacing/margins (template pattern)
        self.contmergina=1; self.contmerginb=1; self.contmerginc=1; self.contmergind=1; self.setspacing=2
        self.panelmergina=5; self.panelmerginb=5; self.panelmerginc=5; self.panelmergind=5; self.panelspacing=5
        self.titlebarheight=45; self.toolbarheight=50
        self.tabmerginsa=5; self.tabmerginsb=0; self.tabmerginsc=5; self.tabmerginsd=0; self.statusheight=22
        self.buticonsizex=20; self.buticonsizey=20
        self.gadiconsizex=20; self.gadiconsizey=20
        self.iconsizex=64; self.iconsizey=64

        # Icon factory
        self.icon_factory = SVGIconFactory()

        self.setWindowTitle(App_name)
        try:
            self.setWindowIcon(SVGIconFactory.path_app_icon(64))
        except Exception:
            pass
        self.resize(1400, 800)
        self.setMinimumSize(800, 500)

        # Radar state
        self._img_reader:   Optional[ImgReader] = None
        self._iv_img = None                  # GTA IV radar.img (IMGFile, V3)
        self._texdb = None                   # mobile texture DB (SA iOS/Android radar)
        self._iv_entries: Dict[int, object] = {}
        self._img_path:     str = ""
        self._game_preset:  dict = GAME_PRESETS["SA PC"]
        self._current_idx:  int  = -1
        self._init_path_system()
        self._draw_tool: str = 'none'        # 'none' / 'zoom' / 'path_node'
        self._tw: int = TILE_W
        self._th: int = TILE_H
        self._tile_rgba:    Dict[int, bytes] = {}
        self._world_offset: tuple = (0.0, 0.0)  # (dx, dy) world repositioning
        self._tile_entries: List[dict] = []

        # Frameless in standalone (custom titlebar), widget when docked
        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos()
            self.move(p.x() + 50, p.y() + 80)

        self.setup_ui()
        self._setup_hotkeys()
        self._apply_theme()
        self._apply_preset("SA PC")

    def get_content_margins(self): #vers 1
        return (self.contmergina,self.contmerginb,self.contmerginc,self.contmergind)

    def get_panel_margins(self): #vers 1
        return (self.panelmergina,self.panelmerginb,self.panelmerginc,self.panelmergind)

    def get_tab_margins(self): #vers 1
        return (self.tabmerginsa,self.tabmerginsb,self.tabmerginsc,self.tabmerginsd)

    #    setup_ui

        # Corner resize overlay set up in showEvent

    def setup_ui(self): #vers 2
        """Titlebar / radar view with ribbons / node docks / status bar."""
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(*self.get_content_margins())
        main_layout.setSpacing(self.setspacing)

        toolbar = self._create_toolbar()
        main_layout.addWidget(toolbar)

        self._draw_btns = {}
        outer = make_outer_mw(self.ribbon_wrap(self._create_right_panel()))
        self._outer_mw = outer
        self._outer_docks = [
            add_outer_dock(self, outer, "Path Nodes", self._create_nodes_panel(), 220),
            add_outer_dock(self, outer, "Node Properties", self._create_props_panel(), 200,
                           Qt.DockWidgetArea.RightDockWidgetArea)]
        main_layout.addWidget(outer, 1)

        self._build_ribbons()
        self.ribbon_restore_state()
        QTimer.singleShot(400, self._restore_outer_layout)
        self.window_closed.connect(self._save_outer_layout)

        self._status_bar = self._create_status_bar()
        main_layout.addWidget(self._status_bar)


    # - Toolbar

    def _create_toolbar(self): #vers 2
        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(self.titlebarheight)
        self.titlebar.setObjectName("titlebar")
        self.titlebar.installEventFilter(self)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(self.toolbarheight)
        self.titlebar = self.toolbar  # alias for drag detection

        # - Theme-aware icon colour
        icon_color = self._get_icon_color()

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(*self.get_panel_margins())
        layout.setSpacing(self.panelspacing)


        self.menu_toggle_btn = QPushButton("Menu")
        self.menu_toggle_btn.setFont(self.button_font)
        self.menu_toggle_btn.setToolTip("Show menu (topbar or dropdown — set in Settings)")
        self.menu_toggle_btn.setMinimumHeight(28)
        self.menu_toggle_btn.setMaximumHeight(28)
        self.menu_toggle_btn.clicked.connect(self._on_menu_btn_clicked)
        # Menu/Settings (Aug 20 2026)
        self.menu_toggle_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.menu_toggle_btn)

        # - Settings button (standalone only — docked uses right-panel button)
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip(App_name + "Workshop Settings")
        self.settings_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.settings_btn)

        layout.addSpacing(8)
        layout.addStretch()

        # - App name + version label (centre-left of toolbar)
        self._title_lbl = QLabel(f"{App_name} - {Build}")
        self._title_lbl.setStyleSheet("")
        self._title_lbl.setVisible(self.standalone_mode)
        layout.addWidget(self._title_lbl)

        layout.addStretch()

        # Game / W / H selector lives in the Grid ribbon (_build_ribbons)

        # - Info button (before Theme)
        self.info_radar_btn = QPushButton()
        self.info_radar_btn.setIcon(SVGIconFactory.info_icon(20, icon_color))
        self.info_radar_btn.setIconSize(QSize(20, 20))
        self.info_radar_btn.setFixedSize(35, 35)
        self.info_radar_btn.setToolTip("About Path Workshop")
        self.info_radar_btn.clicked.connect(self._show_about)
        layout.addWidget(self.info_radar_btn)
        self.info_radar_btn.setVisible(self.standalone_mode)

        # - Theme / Properties
        self.properties_btn = QPushButton()
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(20, icon_color))
        self.properties_btn.setIconSize(QSize(20, 20))
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.setToolTip("Theme Settings")
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        layout.addWidget(self.properties_btn)
        self.properties_btn.setVisible(self.standalone_mode)

        # - Dock button — hidden in standalone (nothing to dock to)
        self.dock_btn = QPushButton()
        try:
            self.dock_btn.setIcon(SVGIconFactory.import_icon(20, icon_color))
            self.dock_btn.setIconSize(QSize(20, 20))
        except Exception:
            self.dock_btn.setText("⊞")
        self.dock_btn.setFixedSize(35, 35)
        self.dock_btn.setToolTip("Dock into IMG Factory")
        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        self.dock_btn.setVisible(not self.standalone_mode)
        layout.addWidget(self.dock_btn)

        # - Tear-off button — only when docked, use icon not bare "T"
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton()
            try:
                self.tearoff_btn.setIcon(SVGIconFactory.export_icon(20, icon_color))
                self.tearoff_btn.setIconSize(QSize(20, 20))
            except Exception:
                self.tearoff_btn.setText("↗")
            self.tearoff_btn.setFixedSize(35, 35)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("Tear off — open as standalone window")
            layout.addWidget(self.tearoff_btn)

        # - Window controls (standalone only)
        if self.standalone_mode:
            for attr, icon_method, slot, tip in [
                ('minimize_btn', 'minimize_icon', self.showMinimized,    "Minimize"),
                ('maximize_btn', 'maximize_icon', self._toggle_maximize, "Maximize"),
                ('close_btn',    'close_icon',    self.close,            "Close"),
            ]:
                btn = QPushButton()
                btn.setIcon(getattr(SVGIconFactory, icon_method)(20, icon_color))
                btn.setIconSize(QSize(20, 20))
                btn.setMinimumWidth(40); btn.setMaximumWidth(40); btn.setMinimumHeight(30)
                btn.clicked.connect(slot)
                btn.setToolTip(tip)
                setattr(self, attr, btn)
                layout.addWidget(btn)

        return self.toolbar


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

    # --- Right panel: radar view
    def _ribbon_config_path(self): #vers 1
        """Dock layout file (ribbons keep their own file)."""
        return get_user_config_dir() / 'path_workshop_docks.json'

    def _create_right_panel(self): #vers 11
        """Radar map view (background for the paths)."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        hl = QHBoxLayout(panel)
        hl.setContentsMargins(0, 0, 0, 0)
        hl.setSpacing(0)
        self._radar = RadarGridWidget()
        self._radar._workshop = self
        sc = QScrollArea()
        sc.setWidget(self._radar)
        sc.setWidgetResizable(True)
        sc.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._radar_scroll = sc
        hl.addWidget(sc, 1)
        return panel

    def _create_status_bar(self): #vers 1
        bar = QFrame()
        bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        bar.setFixedHeight(self.statusheight)
        hl = QHBoxLayout(bar)
        hl.setContentsMargins(*self.get_tab_margins())
        self.status_label = QLabel("Ready — no IMG loaded")
        self.status_label.setFont(self.infobar_font)
        hl.addWidget(self.status_label)
        return bar


    # Settings dialog
    def _show_workshop_settings(self): #vers 4
        """Workshop-local settings — Fonts, Display, Menu tabs."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                     QTabWidget, QWidget, QGroupBox, QFormLayout,
                                     QSpinBox, QComboBox, QLabel, QFontComboBox,
                                     QCheckBox)
        from PyQt6.QtGui import QFont

        dlg = QDialog(self)
        dlg.setWindowTitle(f"{App_name} Settings")
        dlg.setMinimumWidth(520)
        dlg.setMinimumHeight(420)
        layout = QVBoxLayout(dlg)
        tabs = QTabWidget()

        #    Fonts tab                                                          
        fonts_tab = QWidget()
        fl = QVBoxLayout(fonts_tab)

        def _font_row(label, current_font):
            grp = QGroupBox(label)
            hl  = QHBoxLayout(grp)
            combo = QFontComboBox(); combo.setCurrentFont(current_font)
            spin  = QSpinBox(); spin.setRange(7, 32); spin.setValue(current_font.pointSize())
            spin.setSuffix(" pt"); spin.setFixedWidth(70)
            hl.addWidget(combo); hl.addWidget(spin)
            return grp, combo, spin

        title_grp,  title_combo,  title_sz  = _font_row("Title Font",   self.title_font)
        panel_grp,  panel_combo,  panel_sz  = _font_row("Panel Font",   self.panel_font)
        button_grp, button_combo, button_sz = _font_row("Button Font",  self.button_font)
        info_grp,   info_combo,   info_sz   = _font_row("Info Bar Font",self.infobar_font)
        for g in [title_grp, panel_grp, button_grp, info_grp]: fl.addWidget(g)
        fl.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        #    Display tab                                                        
        disp_tab = QWidget()
        dl = QVBoxLayout(disp_tab)

        btn_grp = QGroupBox("Button Display Mode")
        bl = QVBoxLayout(btn_grp)
        mode_combo = QComboBox(); mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        mode_combo.setCurrentIndex({'both':0,'icons':1,'text':2}.get(self.button_display_mode, 0))
        bl.addWidget(mode_combo)
        dl.addWidget(btn_grp)
        dl.addStretch()
        tabs.addTab(disp_tab, "Display")

        #    Menu tab                                                           
        menu_tab = QWidget()
        ml = QVBoxLayout(menu_tab)

        game_grp = QGroupBox("Default Game Preset")
        gl = QHBoxLayout(game_grp)
        game_combo_s = QComboBox(); game_combo_s.addItems(list(GAME_PRESETS))
        game_combo_s.setCurrentText(self._game_combo.currentText())
        gl.addWidget(game_combo_s)
        ml.addWidget(game_grp)

        menu_grp = QGroupBox("Menu Style")
        mgl = QVBoxLayout(menu_grp)
        menu_style_combo = QComboBox()
        menu_style_combo.addItems(["Dropdown (button)", "Topbar (embedded)"])
        menu_style_combo.setCurrentIndex(0 if self.RAD_settings.get('menu_style') == 'dropdown' else 1)
        mgl.addWidget(menu_style_combo)
        ml.addWidget(menu_grp)
        ml.addStretch()
        tabs.addTab(menu_tab, "Menu")

        layout.addWidget(tabs)

        #    Buttons                                                            
        btn_row = QHBoxLayout(); btn_row.addStretch()

        def apply_all():
            self.title_font   = QFont(title_combo.currentFont().family(),  title_sz.value())
            self.panel_font   = QFont(panel_combo.currentFont().family(),  panel_sz.value())
            self.button_font  = QFont(button_combo.currentFont().family(), button_sz.value())
            self.infobar_font = QFont(info_combo.currentFont().family(),   info_sz.value())
            self._apply_title_font()
            self._apply_panel_font()
            self._apply_button_font()
            self._apply_infobar_font()
            self.button_display_mode = {0:'both',1:'icons',2:'text'}[mode_combo.currentIndex()]
            new_game = game_combo_s.currentText()
            if new_game != self._game_combo.currentText():
                self._on_game_changed(new_game)
            style = 'dropdown' if menu_style_combo.currentIndex() == 0 else 'topbar'
            self.RAD_settings.set('menu_style', style)
            self.RAD_settings.set('show_menubar', style == 'topbar')
            self.RAD_settings.save()
            self.set_menu_orientation(style)

        apply_btn = QPushButton("Apply"); apply_btn.clicked.connect(apply_all)
        ok_btn = QPushButton("OK"); ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: (apply_all(), dlg.accept()))
        cancel_btn = QPushButton("Cancel"); cancel_btn.clicked.connect(dlg.reject)
        for b in [cancel_btn, apply_btn, ok_btn]: btn_row.addWidget(b)
        layout.addLayout(btn_row)
        dlg.exec()

    # - Menu options
    def _apply_menu_bar_style(self): #vers 3
        """Apply font size, height and colours to the topbar menubar.
        Uses explicit colours so it stays readable regardless of app theme.
        """
        mb = getattr(self, '_menu_bar', None)
        if not mb:
            return
        bar_h  = self.RAD_settings.get('menu_bar_height', 22)
        bar_fs = self.RAD_settings.get('menu_bar_font_size', 9)
        dd_fs  = self.RAD_settings.get('menu_dropdown_font_size', 9)

        # - Get theme colours if available, otherwise use sensible defaults
        bg   = '#2b2b2b'
        fg   = '#e0e0e0'
        sel  = '#1976d2'
        selfg = '#ffffff'
        border = '#555555'
        try:
            app_settings = getattr(self, 'app_settings', None)
            if not app_settings and self.main_window:
                app_settings = getattr(self.main_window, 'app_settings', None)
            if app_settings:
                tc = app_settings.get_theme_colors() or {}
                bg    = tc.get('bg_primary',   bg)
                fg    = tc.get('text_primary',  fg)
                sel   = tc.get('accent',        sel)
                border = tc.get('border',       border)
        except Exception:
            pass

        # - Height controlled by container — NOT by stylesheet min/max-height
        # - (stylesheet height properties override Qt layout and prevent the bar showing)
        mb.setStyleSheet(f"""
            QMenuBar {{
                background-color: {bg};
                color: {fg};
                border-bottom: 1px solid {border};
                font-size: {bar_fs}pt;
            }}
            QMenuBar::item {{
                background-color: transparent;
                padding: 2px 6px;
            }}
            QMenuBar::item:selected {{
                background-color: {sel};
                color: {selfg};
            }}
            QMenu {{
                background-color: {bg};
                color: {fg};
                border: 1px solid {border};
                font-size: {dd_fs}pt;
            }}
            QMenu::item:selected {{
                background-color: {sel};
                color: {selfg};
            }}
        """)

        # - Size the container based on settings, not isVisible()
        # - (isVisible() is False during __init__ even if widget will be shown)
        c = getattr(self, '_menu_bar_container', None)
        if c:
            show = (self.RAD_settings.get('show_menubar', False) and
                    self.RAD_settings.get('menu_style', 'dropdown') == 'topbar')
            if show:
                c.setMinimumHeight(0)
                c.setMaximumHeight(bar_h)
            # Don't touch height here if hiding — setup_ui and set_menu_orientation handle that


    def set_menu_orientation(self, style: str): #vers 4
        """Switch DP5 menu between 'topbar' (internal) and 'dropdown' (host menubar).
        Called from imgfactory Settings when the orientation radio changes.
        """
        self.RAD_settings.set('menu_style', style)
        self.RAD_settings.set('show_menubar', style == 'topbar')

        # Toggle the container (not the bare QMenuBar) so Qt doesn't promote it
        container = getattr(self, '_menu_bar_container', None) or getattr(self, '_menu_bar', None)
        if container:
            if style == 'topbar':
                container.setMinimumHeight(0)
                container.setMaximumHeight(16777215)
                container.setVisible(True)
                container.updateGeometry()
                # Re-apply style so height/font are correct
                self._apply_menu_bar_style()
            else:
                container.setVisible(False)
                container.setMinimumHeight(0)
                container.setMaximumHeight(0)
                container.setFixedHeight(0)

    def get_menu_title(self) -> str: #vers 1
        """Return menu label for imgfactory menu bar."""
        return "DP5 Paint"

    def _get_tool_menu_style(self) -> str: #vers 1
        """Read menu_style from RAD_settings."""
        return self.RAD_settings.get('menu_style', 'dropdown')

    def _on_menu_btn_clicked(self): #vers 3
        style = self.RAD_settings.get('menu_style')
        if style == 'dropdown':
            self._show_dropdown_menu()
        else:
            on = not self.RAD_settings.get('show_menubar')
            self.RAD_settings.set('show_menubar', on)
            self.RAD_settings.save()
            c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
            if c:
                c.setMinimumHeight(0)
                c.setMaximumHeight(16777215 if on else 0)
                c.setVisible(on)

    def _show_dropdown_menu(self): #vers 2
        """Pop up the radar menus as a single QMenu dropdown."""
        menu = QMenu(self)
        self._build_menus_into_qmenu(menu)
        btn = getattr(self, 'menu_toggle_btn', None)
        if btn:
            menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            menu.exec(self.cursor().pos())


    def _toggle_menubar(self, on: bool): #vers 3
        self.RAD_settings.set('show_menubar', on)
        self.RAD_settings.save()
        c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
        if c:
            c.setMinimumHeight(0)
            c.setMaximumHeight(16777215 if on else 0)
            c.setVisible(on)


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


    # - Font apply helpers (template pattern)
    def _apply_title_font(self): #vers 1
        if hasattr(self, 'title_label'): self.title_label.setFont(self.title_font)


    def _apply_panel_font(self): #vers 1
        for lbl in self.findChildren(QLabel):
            if lbl.objectName() in ('panel_header',) or lbl.text() in ("Tiles","Modified: 0"):
                lbl.setFont(self.panel_font)


    def _apply_button_font(self): #vers 1
        for btn in self.findChildren(QPushButton): btn.setFont(self.button_font)


    def _apply_infobar_font(self): #vers 2
        if hasattr(self,'status_label'): self.status_label.setFont(self.infobar_font)


    # - Docking (template pattern)
    def _update_dock_button_visibility(self): #vers 1
        if hasattr(self,'dock_btn'): self.dock_btn.setVisible(not self.is_docked)
        if hasattr(self,'tearoff_btn'): self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)

    def _setup_hotkeys(self): #vers 3
        """Workshop-local keys (no clash with IMG Factory when docked)."""
        keys = [(QKeySequence.StandardKey.Open, self._open_file),
                (QKeySequence("Ctrl+S"), self._save_paths),
                (QKeySequence("Ctrl+Z"), self._undo_path_edit),
                (QKeySequence(Qt.Key.Key_Plus), lambda: self._zoom(1.25)),
                (QKeySequence(Qt.Key.Key_Equal), lambda: self._zoom(1.25)),
                (QKeySequence(Qt.Key.Key_Minus), lambda: self._zoom(0.8)),
                (QKeySequence("Ctrl+0"), self._fit),
                (QKeySequence(Qt.Key.Key_S), lambda: self._set_path_tool('none')),
                (QKeySequence(Qt.Key.Key_Z), lambda: self._set_path_tool('zoom')),
                (QKeySequence(Qt.Key.Key_N), lambda: self._set_path_tool('path_node')),
                (QKeySequence(Qt.Key.Key_L), lambda: self._set_path_tool('link'))]
        for seq, slot in keys:
            sc = QShortcut(seq, self)
            sc.setContext(Qt.ShortcutContext.WidgetWithChildrenShortcut)
            sc.activated.connect(slot)
        if self.main_window and hasattr(self.main_window,'log_message'):
            self.main_window.log_message(f"{App_name} hotkeys ready")


    def _refresh_icons(self): #Vers 1
        SVGIconFactory.clear_cache()
        # Re-apply theme-aware icon colour to toolbar buttons
        color = self._get_icon_color()
        icon_map = [
            ('settings_btn',    'settings_icon'),
            ('new_session_btn', 'add_icon'),
            ('clear_btn',       'delete_icon'),
            ('properties_btn',  'properties_icon'),
            ('docked_settings_btn', 'settings_icon'),
        ]
        for attr, method in icon_map:
            if hasattr(self, attr):
                btn = getattr(self, attr)
                btn.setIcon(getattr(SVGIconFactory, method)(20, color))
        # Window control buttons
        for attr, method in [('minimize_btn','minimize_icon'),
                              ('maximize_btn','maximize_icon'),
                              ('close_btn','close_icon')]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(getattr(SVGIconFactory, method)(20, color))


   # - File ops

    def _open_file(self, path: str = ""): #vers 6
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
            path,_ = QFileDialog.getOpenFileName(self, title, "", filt)
            if not path: return

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

    def _build_ribbons(self): #vers 5
        """File / View / Grid / Path ribbons."""
        B = self.ribbon_button

        tb = self.ribbon_toolbar("File")
        self.open_btn = B(tb, "open_icon",   "Load radar IMG / TXD  (Ctrl+O)", self._open_file)
        B(tb, "import_icon", "Load radar image (PNG / JPG / BMP)", self._open_radar_image)
        B(tb, "folder_icon", "Load paths: game .dat, .ipl, nodesN.dat, tracks / flight .dat", self._load_paths)
        B(tb, "save_icon",   "Save paths  (Ctrl+S)", self._save_paths)

        tb = self.ribbon_toolbar("View")
        B(tb, "zoom_in_icon",  "Zoom in  (+)",  lambda: self._zoom(1.25))
        B(tb, "zoom_out_icon", "Zoom out  (-)", lambda: self._zoom(0.8))
        B(tb, "fit_grid_icon", "Fit grid  (Ctrl+0)", self._fit)

        tb = self.ribbon_toolbar("Grid")
        self.ribbon_label(tb, "Game")
        self._game_combo = QComboBox()
        self._game_combo.addItems(list(GAME_PRESETS))
        self._game_combo.setCurrentText("SA PC")
        self._game_combo.currentTextChanged.connect(self._on_game_changed)
        self._game_combo.setFixedSize(44, 26)
        tb.addWidget(self._game_combo)
        self._cols_spin = QSpinBox()
        self._cols_spin.setRange(1, 100); self._cols_spin.setValue(8)
        self._cols_spin.setPrefix("W "); self._cols_spin.setEnabled(False)
        self._cols_spin.valueChanged.connect(self._on_custom_changed)
        self._cols_spin.setFixedSize(100, 26)
        tb.addWidget(self._cols_spin)
        self._rows_spin = QSpinBox()
        self._rows_spin.setRange(1, 100); self._rows_spin.setValue(8)
        self._rows_spin.setPrefix("H "); self._rows_spin.setEnabled(False)
        self._rows_spin.valueChanged.connect(self._on_custom_changed)
        self._rows_spin.setFixedSize(100, 26)
        tb.addWidget(self._rows_spin)

        tb = self.ribbon_toolbar("Path")
        for icon, tip, name in (('dp_select_icon', "Select / move node, drag empty space to pan (S)", 'none'),
                                ('add_icon',       "Add node to selected group (N)",  'path_node'),
                                ('get_link_icon',  "Link: click node, then its next node (L)", 'link'),
                                ('zoom_in_icon',   "Zoom tool (Z)",      'zoom')):
            self._draw_btns[name] = B(
                tb, icon, tip, lambda checked=False, t=name: self._set_path_tool(t),
                checkable=True)
        self._draw_btns['none'].setChecked(True)
        B(tb, "delete_icon", "Delete selected node (Del)", self._delete_selected_node)
        B(tb, "undo_icon",   "Undo path edit  (Ctrl+Z)", self._undo_path_edit)
        for text, attr in (("Cars", '_show_car'), ("Peds", '_show_ped')):
            chk = QCheckBox(text); chk.setChecked(True)
            chk.toggled.connect(lambda on, a=attr: (setattr(self, a, on), self._radar.update()))
            tb.addWidget(chk)

    def _set_path_tool(self, tool): #vers 2
        """Select / add / link / zoom tool on the radar view."""
        self._draw_tool = tool
        self._link_from = -1
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)

    def _open_radar_image(self, path=None): #vers 1
        """Whole-map radar image (PNG/JPG/BMP) cut into the current game's grid."""
        if not path:
            path, _ = QFileDialog.getOpenFileName(self, "Load Radar Image", "",
                                                  "Images (*.png *.jpg *.jpeg *.bmp *.tga);;All Files (*)")
        if not path:
            return
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

    # - Tile selection

    # - Export/Import sheet
    # - Grid nav
    def _zoom(self, f): #vers 3
        """Scale the radar grid — uses RadarGridWidget zoom factor."""
        if hasattr(self, '_radar'):
            self._radar._zoom_factor = max(0.1, min(10.0, self._radar._zoom_factor * f))
            self._radar.update()

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

    # - Status
    def _set_status(self, msg): #vers 1
        if hasattr(self,'status_label'): self.status_label.setText(msg)


    # - Theme & icon helpers (template pattern)
    def _apply_theme(self): #vers 1
        try:
            if self.app_settings:
                self.setStyleSheet(self.app_settings.get_stylesheet())
            else:
                self.setStyleSheet("QWidget{background:palette(base);color:palette(mid);} QPushButton{background:palette(button);border:1px solid palette(mid);color:palette(mid);padding:2px 4px;} QPushButton:hover{background:#4a4d50;}")
        except Exception as e:
            print(f"[{App_name}] Theme error: {e}")


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

    # Settings and about

    def _show_about(self): #vers 4
        """Path Workshop quick start."""
        html = ("<p style='font-size:11px; color:#888;'><b>{}</b> &middot; {} &middot; {} &middot; {}</p><hr>"
                "<h3>Quick Start</h3><ol>"
                "<li><b>Load the radar</b>: Open (Ctrl+O) an IMG / TXD with radar tiles, "
                "or <b>Load Radar Image</b> for a whole-map PNG / JPG.</li>"
                "<li><b>Load Paths</b>: game .dat, a single .ipl, nodesN.dat, or tracks / flight .dat (view only).</li>"
                "<li>Select (S) and drag nodes, Add (N) joins the selected group, "
                "Link (L) sets a node's next node, Del deletes.</li>"
                "<li>Ctrl+S saves IPL / IDE path sections and SA nodes files (backups kept).</li></ol>"
                "<h3>Keys</h3><p>+ / - zoom, Ctrl+0 fit, middle-drag or arrows pan, Home resets, Ctrl+Z undo.</p>"
                ).format(App_name, Build, App_build, App_auth)
        QMessageBox.about(self, f"{App_name} - Help", html)

    def _show_info(self): #vers 1
        lines = [
            f"<b>{App_name}</b>  Build {Build}",
            f"Author: {App_auth}  —  {App_build}",
            "",
            "GTA path editor on the radar map",
            "Supports: SA · VC · VCS · LC · LCS · SOL · Custom",
            "",
            f"IMG loaded: {Path(self._img_path).name if self._img_path else 'None'}",
            f"Tiles loaded: {len(self._tile_rgba)}",
        ]
        QMessageBox.information(self, App_name + " — Info", "\n".join(lines))


    def _show_settings_dialog(self): #vers 1  (theme button → launch global theme engine)
        try:
            from apps.utils.app_settings_system import AppSettings, SettingsDialog
            if not hasattr(self, 'app_settings') or self.app_settings is None:
                self.app_settings = AppSettings()
            dlg = SettingsDialog(self.app_settings, self)
            dlg.themeChanged.connect(lambda _: self._apply_theme())
            if dlg.exec():
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error", str(e))



    def _launch_theme_settings(self): #Vers 1
        try:
            if not APPSETTINGS_AVAILABLE:
                return
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error", str(e))


    # Window management (mirrors COL Workshop)

    def _get_icon_color(self): #Vers 1
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            return colors.get('text_primary', '#ffffff')
        return '#ffffff'

    def toggle_dock_mode(self): #Vers 1
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

    def _dock_to_main(self): #vers 2
        """Return a torn-off workshop to its IMG Factory tab."""
        from apps.methods.dock_panels import redock_to_tab
        if not redock_to_tab(self):      # no tab to return to
            return
        self.is_docked = True
        self.standalone_mode = False
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(False)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(False)
        # Hide the same window-chrome-style toolbar elements _create_
        # toolbar itself hides at construction time when starting
        # docked (Aug 20 2026,
        for attr in ('menu_toggle_btn', 'settings_btn', 'toolbar_undo_btn',
                     'info_radar_btn', 'properties_btn'):
            if hasattr(self, attr):
                getattr(self, attr).setVisible(False)
        self.show(); self.raise_()

    def _undock_from_main(self): #vers 2
        self.standalone_mode = True
        self.is_docked = False
        self.setWindowFlags(Qt.WindowType.Window | Qt.WindowType.FramelessWindowHint)
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(True)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(False)
        if hasattr(self, 'settings_btn'):
            self.settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(True)      # D docks back
        # Restore the same window-chrome-style elements _dock_to_main
        # hides (Aug 20 2026, see that method's own comment for the
        # fuller reasoning) - the reverse half of that same fix, so
        # undocking after docking correctly brings these back too,
        # not just the tool's own initial construction-time state.
        for attr in ('menu_toggle_btn', 'toolbar_undo_btn',
                     'info_radar_btn', 'properties_btn'):
            if hasattr(self, attr):
                getattr(self, attr).setVisible(True)
        self.resize(1300, 800)
        self.show(); self.raise_()

    def _toggle_tearoff(self): #Vers 1
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

    def _toggle_maximize(self): #Vers 1
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()

    # Corner resize + dragging (identical pattern to COL Workshop)

    def _update_transform_text_panel_visibility(self): #vers 2
        """Toggle between text+icon panel (wide) and icon-only strip (narrow).
        Reads threshold from IMG Factory settings. Also collapses bottom buttons."""
        tp   = getattr(self, '_transform_text_panel_ref', None)
        ip   = getattr(self, '_transform_icon_panel_ref', None)
        mode = getattr(self, 'button_display_mode', 'both')

        if mode == 'icons':
            if tp: tp.setVisible(False)
            if ip: ip.setVisible(True)
            return
        if mode == 'text':
            if tp: tp.setVisible(True)
            if ip: ip.setVisible(False)
            return

        # Measure right panel width directly
        rp = getattr(self, '_right_panel_ref', None)
        if rp:
            ref_w = rp.width()
        else:
            splitter = getattr(self, '_main_splitter', None)
            ref_w = self.width()
            if splitter and tp:
                w = tp
                while w and w.parent() is not splitter:
                    w = w.parent() if hasattr(w, 'parent') else None
                if w:
                    ref_w = w.width()

        try:
            from apps.methods.imgfactory_ui_settings import get_collapse_threshold
            threshold = get_collapse_threshold(getattr(self, 'main_window', None))
        except Exception:
            threshold = 550
        wide = ref_w >= threshold
        if tp: tp.setVisible(wide)
        if ip: ip.setVisible(not wide)

        # Toggle bottom panel rows the same way
        btr = getattr(self, '_bottom_text_row', None)
        bir = getattr(self, '_bottom_icon_row', None)
        if btr: btr.setVisible(wide)
        if bir: bir.setVisible(not wide)


    def _get_resize_corner(self, pos): #Vers 2
        if not self.standalone_mode:           # docked: no corner resize
            return None
        size = self.corner_size; w = self.width(); h = self.height()
        if pos.x() < size and pos.y() < size:           return "top-left"
        if pos.x() > w - size and pos.y() < size:       return "top-right"
        if pos.x() < size and pos.y() > h - size:       return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:   return "bottom-right"
        return None


    def _update_cursor(self, direction): #Vers 1
        cursors = {
            "top":          Qt.CursorShape.SizeVerCursor,
            "bottom":       Qt.CursorShape.SizeVerCursor,
            "left":         Qt.CursorShape.SizeHorCursor,
            "right":        Qt.CursorShape.SizeHorCursor,
            "top-left":     Qt.CursorShape.SizeFDiagCursor,
            "bottom-right": Qt.CursorShape.SizeFDiagCursor,
            "top-right":    Qt.CursorShape.SizeBDiagCursor,
            "bottom-left":  Qt.CursorShape.SizeBDiagCursor,
        }
        self.setCursor(cursors.get(direction, Qt.CursorShape.ArrowCursor))


    def _get_resize_direction(self, pos): #vers 1
        """Determine resize direction based on mouse position"""
        rect = self.rect()
        margin = self.resize_margin

        left = pos.x() < margin
        right = pos.x() > rect.width() - margin
        top = pos.y() < margin
        bottom = pos.y() > rect.height() - margin

        if left and top:
            return "top-left"
        elif right and top:
            return "top-right"
        elif left and bottom:
            return "bottom-left"
        elif right and bottom:
            return "bottom-right"
        elif left:
            return "left"
        elif right:
            return "right"
        elif top:
            return "top"
        elif bottom:
            return "bottom"

        return None


    def _handle_resize(self, global_pos): #vers 1
        """Handle window resizing"""
        if not self.resize_direction or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.frameGeometry()

        min_width = 800
        min_height = 600

        # Handle horizontal resizing
        if "left" in self.resize_direction:
            new_width = geometry.width() - delta.x()
            if new_width >= min_width:
                geometry.setLeft(geometry.left() + delta.x())
        elif "right" in self.resize_direction:
            new_width = geometry.width() + delta.x()
            if new_width >= min_width:
                geometry.setRight(geometry.right() + delta.x())

        # Handle vertical resizing
        if "top" in self.resize_direction:
            new_height = geometry.height() - delta.y()
            if new_height >= min_height:
                geometry.setTop(geometry.top() + delta.y())
        elif "bottom" in self.resize_direction:
            new_height = geometry.height() + delta.y()
            if new_height >= min_height:
                geometry.setBottom(geometry.bottom() + delta.y())

        self.setGeometry(geometry)
        self.drag_position = global_pos


    def _is_on_draggable_area(self, pos): #Vers 1
        if not hasattr(self, 'titlebar'):
            return False
        if not self.titlebar.rect().contains(pos):
            return False
        for w in self.titlebar.findChildren(QPushButton):
            if w.isVisible() and w.geometry().contains(pos):
                return False
        return True


    def mousePressEvent(self, event): #Vers 1
        if event.button() != Qt.MouseButton.LeftButton:
            return super().mousePressEvent(event)
        pos = event.pos()
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept(); return
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            tb_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(tb_pos):
                handle = self.windowHandle()
                if handle:
                    handle.startSystemMove()
                event.accept(); return
        super().mousePressEvent(event)


    def mouseMoveEvent(self, event): #Vers 1
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept(); return
        else:
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()
            self._refresh_corner_overlay()
            self._update_cursor(corner)
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #Vers 2
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #Vers 1
        if not self.resize_corner or not self.drag_position:
            return
        delta = global_pos - self.drag_position
        geometry = self.initial_geometry
        min_w, min_h = 800, 500
        if self.resize_corner == "bottom-right":
            nw = geometry.width() + delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.resize(nw, nh)
        elif self.resize_corner == "bottom-left":
            nx = geometry.x() + delta.x()
            nw = geometry.width() - delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, geometry.y(), nw, nh)
        elif self.resize_corner == "top-right":
            ny = geometry.y() + delta.y()
            nw = geometry.width() + delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(geometry.x(), ny, nw, nh)
        elif self.resize_corner == "top-left":
            nx = geometry.x() + delta.x()
            ny = geometry.y() + delta.y()
            nw = geometry.width() - delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, ny, nw, nh)


    def paintEvent(self, event): #Vers 2
        super().paintEvent(event)
        # Corner handles drawn by _corner_overlay — see _setup_corner_overlay

    def _setup_corner_overlay(self): #vers 4
        """Create or refresh the corner resize overlay."""
        if not self.standalone_mode:
            return
        # Destroy stale overlay if window was resized before it was created
        existing = getattr(self, '_corner_overlay', None)
        if existing is not None:
            existing.setGeometry(0, 0, self.width(), self.height())
            existing.raise_()
            existing.update()
            return
        overlay = _CornerOverlay(self)
        self._corner_overlay = overlay
        overlay.setGeometry(0, 0, self.width(), self.height())
        overlay.show()
        overlay.raise_()

    def _refresh_corner_overlay(self): #vers 1
        if hasattr(self, '_corner_overlay'):
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
            self._corner_overlay.update_state(
                getattr(self, 'hover_corner', None),
                self.app_settings)
            self._corner_overlay.raise_()

    def showEvent(self, event): #vers 3
        super().showEvent(event)
        QTimer.singleShot(0, self._radar.update)       # first paint after layout settles
        QTimer.singleShot(250, self._radar.update)
        if self.standalone_mode:
            # Small delay ensures window geometry is finalised
            QTimer.singleShot(150, self._setup_corner_overlay)

    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        if hasattr(self,'size_grip'): self.size_grip.move(self.width()-16,self.height()-16)
        self._refresh_corner_overlay()

    def hideEvent(self, event): #vers 1
        """Docked tabs never get closeEvent: keep ribbon positions on hide."""
        self.ribbon_save_state()
        super().hideEvent(event)

    def closeEvent(self, event): #vers 5
        if not self._confirm_discard_paths():
            event.ignore(); return
        self.ribbon_save_state()
        # Save window geometry
        if self.standalone_mode:
            g = self.geometry()
            self.RAD_settings.set('window_x', g.x())
            self.RAD_settings.set('window_y', g.y())
            self.RAD_settings.set('window_w', g.width())
            self.RAD_settings.set('window_h', g.height())
            self.RAD_settings.save()
        self.window_closed.emit()
        event.accept()

    #End of Class


def open_path_workshop(main_window=None): #vers 1
    try:
        w = PathWorkshop(None, main_window); w.show(); return w
    except Exception as e:
        if main_window: QMessageBox.critical(main_window,App_name+" Error",str(e))
        return None

__all__ = ["PathWorkshop", "open_path_workshop"]

# Standalone entry point

if __name__ == "__main__": #Vers 1
    import traceback

    print(App_name + " starting…")
    try:
        app = QApplication(sys.argv)
        w = PathWorkshop()
        w.setWindowTitle(App_name + " – Standalone")
        w.resize(1300, 800); w.show(); sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc();sys.exit(1)
