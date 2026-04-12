#!/usr/bin/env python3
#this belongs in apps/components/Radar_Editor/radar_workshop.py - Version: 20
# X-Seti - Apr 2026 - IMG Factory 1.6 - Radar Workshop
# Based on gui_template.py (GUIWorkshop base)
# Layout: left panel hidden | centre=tile list | right=radar grid preview
# Tool bar uses template pattern: titlebar + toolbar with all standard buttons

import os, json, sys, requests, threading, struct, re, math, shutil
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

os.environ['QT_QPA_PLATFORM'] = 'xcb'
os.environ['QSG_RHI_BACKEND'] = 'opengl'

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

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
App_name  = "Radar Workshop"
App_build = "Apr 2026"
App_auth  = "X-Seti"
Build     = "Build 357"


# ── Infrastructure imports
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

# Grid constants (authoritative — do not change without verifying against game files)
# SA: 144 tiles (12x12)  VC/III/LC/LCS/VCS: 64 tiles (8x8)  SOL: 1296 tiles (36x36)
# img_source: 'img'=tiles in .img | 'txd'=single .txd | 'pvr'=.pvr img | 'toc'=toc/tmb/dat
GAME_PRESETS = {
    # PC versions — tiles in gta3.img / gta.img / RadarTex.img
    "III PC":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA III (PC/PS2/Xbox)",
                "hint":"Load gta3.img — contains radar00.txd to radar63.txd"},
    "VC PC":   {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA Vice City (PC/PS2/Xbox)",
                "hint":"Load gta3.img — contains radar00.txd to radar63.txd"},
    "SA PC":   {"cols":12, "rows":12, "count":144,  "name_fn":_name_sa,
                "img_pattern":r"^radar(\d{2}|1[0-3]\d|14[0-3])\.txd$",
                "img_source":"img",  "label":"GTA San Andreas (PC/PS2/Xbox)",
                "hint":"Load gta3.img — contains radar00.txd to radar143.txd (144 tiles, 12x12 grid)"},
    "LCS PC":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA Liberty City Stories (PC/PS2)",
                "hint":"Load gta3.img — contains radar00.txd to radar63.txd"},
    "VCS PC":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA Vice City Stories (PC/PS2)",
                "hint":"Load gta3.img — contains radar00.txd to radar63.txd"},
    "SOL":     {"cols":36, "rows":36, "count":1296,  "name_fn":_name_sol,
                "img_pattern":r"^radar\d{4}\.txd$",
                "img_source":"img",  "label":"GTA State of Liberty (PC)",
                "hint":"Load RadarTex.img — contains radar0000.txd to radar1295.txd"},
    # Android versions
    "III And": {"cols":1,  "rows":1,  "count":1,    "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"img",  "label":"GTA III Android",
                "hint":"Load gta3_unc.img — single RADAR.TXD with 256x256 'radardisc' texture"},
    "VC And":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"img",  "label":"GTA Vice City Android",
                "hint":"Load from root/texdb — see TXD Workshop for texdb format"},
    "SA And":  {"cols":10, "rows":10, "count":100,  "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"toc",  "label":"GTA San Andreas Android",
                "hint":"Load txd.dxt.toc — SA Android uses TOC/TMB/DAT format (not yet supported)"},
    # iOS versions
    "LCS iOS": {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
                "img_source":"pvr",  "label":"GTA LCS iOS",
                "hint":"Load gta3.pvr — contains radar00.txd to radar63.txd (PVRTC format)"},
    "SA iOS":  {"cols":10, "rows":10, "count":100,  "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"toc",  "label":"GTA SA iOS",
                "hint":"Load txd.dxt.toc — iOS SA uses TOC/TMB/DAT format (not yet supported)"},
    # PSP versions
    "LCS PSP": {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"chk",  "label":"GTA LCS PSP",
                "hint":"PSP .chk files use GIM/XTX format — load individual radar .chk files"},
    "VCS PSP": {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"xtx",  "label":"GTA VCS PSP",
                "hint":"PSP .xtx files use GIM/XTX format — load individual radar .xtx files"},
    # Generic
    "Custom":  {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,
                "img_pattern":r"^radar",
                "img_source":"img",  "label":"Custom Grid",
                "hint":"Load any .img archive and adjust W/H spinners"},
}
TILE_W = TILE_H = 128


# - DXT1 codec
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

def encode_dxt1(rgba, w, h): #vers 1
    from PIL import Image
    img=Image.frombytes('RGBA',(w,h),rgba).convert('RGB'); bx=(w+3)//4; by=(h+3)//4; out=bytearray(); px=img.load()
    def p5(r,g,b): return ((r>>3)<<11)|((g>>2)<<5)|(b>>3)
    for by2 in range(by):
        for bx2 in range(bx):
            pix=[px[bx2*4+pxx,by2*4+py] if bx2*4+pxx<w and by2*4+py<h else (0,0,0) for py in range(4) for pxx in range(4)]
            c0=(max(p[0] for p in pix),max(p[1] for p in pix),max(p[2] for p in pix))
            c1=(min(p[0] for p in pix),min(p[1] for p in pix),min(p[2] for p in pix))
            v0=p5(*c0); v1=p5(*c1)
            if v0<v1: v0,v1=v1,v0; c0,c1=c1,c0
            pal=[c0,c1,tuple((2*a+b)//3 for a,b in zip(c0,c1)),tuple((a+2*b)//3 for a,b in zip(c0,c1))]
            lut=0
            for i,p in enumerate(pix): best=min(range(4),key=lambda k:sum((p[j]-pal[k][j])**2 for j in range(3))); lut|=best<<(i*2)
            out+=struct.pack('<HHI',v0,v1,lut)
    return bytes(out)


# - TXD reader/writer

class RadarTxdReader:
    @staticmethod
    def read(data): #vers 3
        """Read first texture from a RW TXD.
        Supports: PC D3D8/D3D9 (8/9), Xbox (5), PS2 (6), iOS/Android (8 ver 0x1005FFFF).
        """
        pos = 12  # skip outer 0x16 container header
        while pos + 12 <= len(data):
            st = struct.unpack_from('<I', data, pos)[0]
            ss = struct.unpack_from('<I', data, pos+4)[0]
            if st == 0x15:  # TextureNative
                th = pos + 24   # skip 0x15 header(12) + inner 0x01 header(12)
                platform = struct.unpack_from('<I', data, th)[0]

                # ── Xbox (platform 5) ─────────────────────────────────────────
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
                        rgba = decode_dxt1(pd, ww, hh)  # DXT3/5 — approximate
                    else:
                        rgba = RadarTxdReader._raw_to_rgba(pd, ww, hh,
                            struct.unpack_from('<I', data, th+72)[0])
                    return rgba, ww, hh, name

                # ── PS2 (platform 6 or FourCC "PS2\0") ──────────────────────
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

                # ── PC D3D8 (platform 8, older) / D3D9 (platform 9, SA PC) ───
                # ── iOS/Android (platform 8, ver 0x1005FFFF) ─────────────────
                else:
                    nb_ = data[th+8:th+40]
                    nb_ = nb_[:nb_.index(b'\x00')] if b'\x00' in nb_ else nb_
                    name = re.sub(r'[^\x20-\x7E]', '', nb_.decode('latin1','replace')).strip()
                    ww   = struct.unpack_from('<H', data, th+80)[0]
                    hh   = struct.unpack_from('<H', data, th+82)[0]
                    dsz  = struct.unpack_from('<I', data, th+88)[0]
                    pd   = data[th+92:th+92+dsz]
                    d3d_fmt   = data[th+76:th+80]
                    comp_byte = struct.unpack_from('<B', data, th+87)[0]
                    raster_fmt= struct.unpack_from('<I', data, th+72)[0]
                    if d3d_fmt == b'DXT1' or (platform == 8 and comp_byte == 1):
                        rgba = decode_dxt1(pd, ww, hh)
                    elif d3d_fmt in (b'DXT3', b'DXT5') or (platform == 8 and comp_byte in (3,5)):
                        rgba = decode_dxt1(pd, ww, hh)  # DXT3/5 approx
                    else:
                        rgba = RadarTxdReader._raw_to_rgba(pd, ww, hh, raster_fmt)
                    return rgba, ww, hh, name

            if ss == 0:
                break
            pos += 12 + ss
        raise ValueError("No Texture Native section")

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

    @staticmethod
    def write(rgba,w,h,tex_name,rw_ver=0x1003FFFF): #vers 1
        dxt=encode_dxt1(rgba,w,h)
        nb=tex_name.encode('latin1')[:31].ljust(32,b'\x00'); ab=b'\x00'*32
        nat=bytearray()
        nat+=struct.pack('<I',8)+struct.pack('<I',0)+nb+ab+struct.pack('<I',0x200)+b'DXT1'
        nat+=struct.pack('<HH',w,h)+struct.pack('<BBBB',16,1,4,1)+struct.pack('<I',len(dxt))+dxt
        def rws(t,b): return struct.pack('<III',t,len(b),rw_ver)+b
        cb=rws(0x01,struct.pack('<HH',1,0))+rws(0x15,rws(0x01,bytes(nat))+rws(0x03,b''))+rws(0x03,b'')
        c=rws(0x16,cb); return c+b'\x00'*((-len(c))%2048)

# - IMG reader

class ImgReader:
    def __init__(self,img_path): #vers 1
        self.img_path=img_path; self.entries=[]; self._img_data=b''; self._load()
    def _load(self): #vers 3
        p=Path(self.img_path); raw=p.read_bytes()
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

    def get_entry_data(self,e): #vers 1
        return self._img_data[e['offset']:e['offset']+e['size']]

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
    """Full radar grid — no gaps, 1px grid lines, hover=tile name tooltip."""
    tile_clicked        = pyqtSignal(int)
    grid_right_clicked  = pyqtSignal(int, QPoint)   # idx, global pos

    def __init__(self,parent=None): #vers 1
        super().__init__(parent)
        self._cols=8; self._count=0; self._tiles={}; self._dirty=set()
        self._sel=-1; self._hover=-1; self._names=[]
        self.setMouseTracking(True)
        self.setSizePolicy(QSizePolicy.Policy.Expanding,QSizePolicy.Policy.Expanding)

    def setup(self,cols,count,names=None): #vers 1
        self._cols=max(1,cols); self._count=count; self._tiles={}; self._dirty=set()
        self._sel=-1; self._hover=-1
        self._names=names or [f"Tile {i}" for i in range(count)]; self.update()

    def _ts(self): #vers 1
        if not self._count or not self._cols: return 32
        rows=(self._count+self._cols-1)//self._cols
        return min(max(4,self.width()//self._cols),max(4,self.height()//rows))

    def _idx_at(self,pos): #vers 1
        ts=self._ts(); idx=(pos.y()//ts)*self._cols+(pos.x()//ts)
        return idx if 0<=idx<self._count else -1

    def set_tile(self,idx,rgba,w,h): #vers 1
        self._tiles[idx]=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888).copy(); self.update()

    def set_dirty(self,idx,d): #vers 1
        if d: self._dirty.add(idx)
        else: self._dirty.discard(idx)
        self.update()

    def set_selected(self,idx): #vers 1
        self._sel=idx; self.update()

    def paintEvent(self,ev): #vers 1
        if not self._count: return
        ts=self._ts(); cols=self._cols; p=QPainter(self)
        for idx in range(self._count):
            col=idx%cols; row=idx//cols; x=col*ts; y=row*ts
            if idx in self._tiles:
                p.drawImage(x,y,self._tiles[idx].scaled(ts,ts,Qt.AspectRatioMode.IgnoreAspectRatio,Qt.TransformationMode.FastTransformation))
            else:
                p.fillRect(x,y,ts,ts,QColor(40,40,40))
            if idx in self._dirty: p.fillRect(x+ts-6,y,6,6,QColor(255,60,60))
            if idx==self._hover: p.fillRect(x,y,ts,ts,QColor(255,255,255,40))
            if idx==self._sel:
                p.setPen(QPen(QColor(80,180,255),2)); p.drawRect(x+1,y+1,ts-2,ts-2)
        p.setPen(QPen(QColor(60,60,60),1))
        rows=(self._count+cols-1)//cols
        for c in range(cols+1): p.drawLine(c*ts,0,c*ts,rows*ts)
        for r in range(rows+1): p.drawLine(0,r*ts,cols*ts,r*ts)
        p.end()

    def mouseMoveEvent(self,ev): #vers 1
        idx=self._idx_at(ev.pos())
        if idx!=self._hover:
            self._hover=idx
            self.setToolTip(f"[{idx}] {self._names[idx]}" if 0<=idx<len(self._names) else "")
            self.update()

    def leaveEvent(self,ev): #vers 1
        self._hover=-1; self.update()

    def mousePressEvent(self,ev): #vers 2
        idx = self._idx_at(ev.pos())
        if idx < 0: return
        if ev.button() == Qt.MouseButton.LeftButton:
            self._sel = idx; self.tile_clicked.emit(idx); self.update()
        elif ev.button() == Qt.MouseButton.RightButton:
            self._sel = idx; self.tile_clicked.emit(idx); self.update()
            self.grid_right_clicked.emit(idx, ev.globalPosition().toPoint())

# - Tile list item

THUMB = 64   # thumbnail size — doubled from 32

class TileListItem(QListWidgetItem):
    """Tile list entry: 64px thumbnail + name + game badge + tile size."""
    def __init__(self, idx, name, game_label="", tile_w=128, tile_h=128): #vers 2
        super().__init__()
        self.idx       = idx
        self.tile_name = name
        self.game_label= game_label
        self.tile_w    = tile_w
        self.tile_h    = tile_h
        self._update_text()
        self.setSizeHint(QSize(0, THUMB + 8))

    def _game_badge(self) -> str: #vers 1
        """Short badge from game label, e.g. 'GTA San Andreas (PC)' -> '[SA]'."""
        g = self.game_label
        if not g: return ""
        badges = {
            "San Andreas": "[SA]", "Vice City Stories": "[VCS]",
            "Vice City":   "[VC]", "Liberty City Stories": "[LCS]",
            "Liberty City": "[LC]", "State of Liberty": "[SOL]",
            "GTA III":     "[III]", "Android": "[And]",
            "iOS":         "[iOS]", "PSP":     "[PSP]",
        }
        for key, badge in badges.items():
            if key in g:
                return badge
        return ""

    def _update_text(self): #vers 1
        badge = self._game_badge()
        lines = [f"  {self.tile_name}"]
        info_parts = []
        if badge: info_parts.append(badge)
        info_parts.append(f"{self.tile_w}×{self.tile_h}")
        lines.append(f"  {' '.join(info_parts)}")
        self.setText("\n".join(lines))

    def set_thumb(self, rgba, w, h): #vers 2
        self.tile_w = w; self.tile_h = h
        img = QImage(rgba, w, h, w*4, QImage.Format.Format_RGBA8888)
        self.setIcon(QIcon(QPixmap.fromImage(img).scaled(
            THUMB, THUMB,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation)))
        self._update_text()

# - Main Class



# ── Per-tool settings ─────────────────────────────────────────────────────────
class RADSettings:
    """Lightweight JSON settings for Radar Workshop.
    Stored at ~/.config/imgfactory/radar_workshop.json
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

    def __init__(self): #vers 1
        cfg_dir = Path.home() / '.config' / 'imgfactory'
        cfg_dir.mkdir(parents=True, exist_ok=True)
        self._path = cfg_dir / 'radar_workshop.json'
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


# ── Radar Palette Widget ───────────────────────────────────────────────────────
class RadarPaletteWidget(QWidget):
    """Simple colour palette strip — shows colours extracted from the current tile.
    Click a cell to pick that colour. Right-click to set background colour."""

    color_picked   = pyqtSignal(QColor)   # left-click  → foreground
    color_picked_bg = pyqtSignal(QColor)  # right-click → background

    CELL = 14   # cell size px — slightly smaller to fit more colors

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._colors: List[QColor] = []
        self._hover = -1
        self.setMouseTracking(True)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Preferred)

    def set_colors(self, colors: List[QColor]): #vers 3
        self._colors = colors[:64]  # cap at 64 cells
        self.update()
        self.updateGeometry()  # tell layout to re-check sizeHint

    def sizeHint(self): #vers 1
        from PyQt6.QtCore import QSize
        w = max(self.width(), 80)
        cols = max(1, w // self.CELL)
        rows = max(2, (len(self._colors) + cols - 1) // cols)
        return QSize(w, rows * self.CELL + 2)

    def minimumSizeHint(self): #vers 1
        from PyQt6.QtCore import QSize
        return QSize(self.CELL * 2, self.CELL * 2)

    def set_colors_from_rgba(self, rgba: bytes, w: int, h: int, max_colors: int = 48): #vers 2
        """Extract palette from RGBA tile data by sampling unique colours."""
        seen: dict = {}
        step = max(1, (w * h) // 512)   # sample at most 512 pixels
        for i in range(0, len(rgba) - 3, step * 4):
            r, g, b, a = rgba[i], rgba[i+1], rgba[i+2], rgba[i+3]
            if a < 16: continue
            key = (r >> 4, g >> 4, b >> 4)  # 4-bit quantise — captures more distinct colours
            if key not in seen:
                seen[key] = QColor(r, g, b)
                if len(seen) >= max_colors:
                    break
        self.set_colors(list(seen.values()))

    def _cols(self) -> int:
        return max(1, self.width() // self.CELL)

    def _idx_at(self, pos: QPoint) -> int:
        col = pos.x() // self.CELL
        row = pos.y() // self.CELL
        idx = row * self._cols() + col
        return idx if 0 <= idx < len(self._colors) else -1

    def paintEvent(self, ev): #vers 1
        p = QPainter(self)
        cols = self._cols()
        for i, c in enumerate(self._colors):
            col = i % cols
            row = i // cols
            x = col * self.CELL
            y = row * self.CELL
            p.fillRect(x, y, self.CELL, self.CELL, c)
            if i == self._hover:
                p.setPen(QPen(QColor(255, 255, 255, 180), 1))
                p.drawRect(x, y, self.CELL - 1, self.CELL - 1)
        # Fill remaining space grey
        total_cols = self._cols()
        total_rows = max(1, (len(self._colors) + total_cols - 1) // total_cols)
        used_h = total_rows * self.CELL
        if used_h < self.height():
            p.fillRect(0, used_h, self.width(), self.height() - used_h, QColor(50, 50, 50))
        p.end()

    def mouseMoveEvent(self, ev): #vers 1
        idx = self._idx_at(ev.pos())
        if idx != self._hover:
            self._hover = idx
            if idx >= 0:
                self.setToolTip(self._colors[idx].name())
            self.update()

    def leaveEvent(self, ev): #vers 1
        self._hover = -1
        self.update()

    def mousePressEvent(self, ev): #vers 2
        idx = self._idx_at(ev.pos())
        if idx < 0: return
        if ev.button() == Qt.MouseButton.LeftButton:
            self.color_picked.emit(self._colors[idx])
        elif ev.button() == Qt.MouseButton.RightButton:
            self._show_palette_context(idx, ev.globalPosition().toPoint())

    def _show_palette_context(self, idx: int, gpos): #vers 1
        """Right-click context menu on a palette colour cell."""
        from PyQt6.QtWidgets import QMenu, QApplication, QColorDialog
        from PyQt6.QtGui import QClipboard
        if idx < 0 or idx >= len(self._colors): return
        c = self._colors[idx]

        menu = QMenu(self)
        act_fg   = menu.addAction(f"Set as FG  {c.name()}")
        act_bg   = menu.addAction(f"Set as BG  {c.name()}")
        menu.addSeparator()
        act_copy = menu.addAction(f"Copy hex  {c.name()}")
        act_rgb  = menu.addAction(f"Copy RGB  {c.red()},{c.green()},{c.blue()}")
        menu.addSeparator()
        act_edit = menu.addAction("Change colour…")

        chosen = menu.exec(gpos)
        if chosen == act_fg:
            self.color_picked.emit(c)
        elif chosen == act_bg:
            self.color_picked_bg.emit(c)
        elif chosen == act_copy:
            QApplication.clipboard().setText(c.name())
        elif chosen == act_rgb:
            QApplication.clipboard().setText(f"{c.red()},{c.green()},{c.blue()}")
        elif chosen == act_edit:
            new_c = QColorDialog.getColor(c, self, "Change Palette Colour",
                                          QColorDialog.ColorDialogOption.ShowAlphaChannel)
            if new_c.isValid():
                self._colors[idx] = new_c
                self.update()
                # Emit so the main workshop can use it immediately
                self.color_picked.emit(new_c)




class _BoredomPuzzle(QDialog):
    """🧩 Sliding tile puzzle using the loaded radar map tiles."""

    def __init__(self, tile_rgba: dict, cols: int, rows: int,
                 tile_w: int, tile_h: int, parent=None): #vers 1
        super().__init__(parent)
        self.setWindowTitle("🧩 Boredom! — Sliding Puzzle")
        self.setModal(True)
        self._cols   = cols
        self._rows   = rows
        self._tile_w = tile_w
        self._tile_h = tile_h
        self._moves  = 0
        self._solved = False
        try:
            from apps.core.theme_utils import apply_dialog_theme
            apply_dialog_theme(self, parent)
        except Exception: pass

        # Build pixmaps for each tile
        self._pixmaps: dict = {}
        for idx in range(cols * rows):
            rgba = tile_rgba.get(idx)
            if rgba:
                img = QImage(rgba, tile_w, tile_h, tile_w*4, QImage.Format.Format_RGBA8888)
                self._pixmaps[idx] = QPixmap.fromImage(img)
            else:
                pm = QPixmap(tile_w, tile_h); pm.fill(QColor(30, 30, 30))
                self._pixmaps[idx] = pm

        # Puzzle state: list of tile indices, last slot = blank
        n = cols * rows
        self._state = list(range(n))
        self._blank = n - 1     # blank tile position
        self._goal  = list(range(n))
        self._shuffle()

        # Cell size for display (cap at 80px per tile)
        self._cell = min(80, max(32, 640 // max(cols, rows)))
        self.setFixedSize(cols * self._cell + 20,
                          rows * self._cell + 60)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(6, 6, 6, 6)

        self._canvas = QLabel()
        self._canvas.setFixedSize(cols * self._cell, rows * self._cell)
        self._canvas.mousePressEvent = self._on_click
        layout.addWidget(self._canvas)

        btn_row = QHBoxLayout()
        self._info_lbl = QLabel(f"Moves: 0")
        btn_row.addWidget(self._info_lbl, 1)
        shuffle_btn = QPushButton("Shuffle")
        shuffle_btn.clicked.connect(self._shuffle_and_redraw)
        btn_row.addWidget(shuffle_btn)
        close_btn = QPushButton("Give Up")
        close_btn.clicked.connect(self.reject)
        btn_row.addWidget(close_btn)
        layout.addLayout(btn_row)

        self._draw()

    def _shuffle(self): #vers 1
        """Shuffle with 200 random valid moves from solved state."""
        import random
        for _ in range(200):
            neighbours = self._blank_neighbours()
            if neighbours:
                swap = random.choice(neighbours)
                self._state[self._blank], self._state[swap] =                     self._state[swap], self._state[self._blank]
                self._blank = swap

    def _shuffle_and_redraw(self): #vers 1
        n = self._cols * self._rows
        self._state = list(range(n))
        self._blank = n - 1
        self._moves = 0
        self._solved = False
        self._shuffle()
        self._draw()

    def _blank_neighbours(self): #vers 1
        c, r = self._blank % self._cols, self._blank // self._cols
        nb = []
        if c > 0: nb.append(self._blank - 1)
        if c < self._cols-1: nb.append(self._blank + 1)
        if r > 0: nb.append(self._blank - self._cols)
        if r < self._rows-1: nb.append(self._blank + self._cols)
        return nb

    def _on_click(self, ev): #vers 1
        if self._solved: return
        x, y = int(ev.position().x()), int(ev.position().y())
        col, row = x // self._cell, y // self._cell
        clicked = row * self._cols + col
        if 0 <= clicked < len(self._state) and clicked in self._blank_neighbours():
            self._state[self._blank], self._state[clicked] =                 self._state[clicked], self._state[self._blank]
            self._blank = clicked
            self._moves += 1
            self._draw()
            if self._state == self._goal:
                self._solved = True
                self._info_lbl.setText(f"🎉 Solved in {self._moves} moves!")
                QMessageBox.information(self, "Puzzle Solved!",
                    f"You solved it in {self._moves} moves!\n\nThe map is restored.")

    def _draw(self): #vers 1
        cell = self._cell
        pm = QPixmap(self._cols * cell, self._rows * cell)
        pm.fill(QColor(20, 20, 20))
        p = QPainter(pm)
        for pos, tile_idx in enumerate(self._state):
            col = pos % self._cols
            row = pos // self._cols
            x, y = col * cell, row * cell
            if pos == self._blank:
                p.fillRect(x, y, cell, cell, QColor(40, 40, 40))
                continue
            src = self._pixmaps.get(tile_idx)
            if src:
                p.drawPixmap(x, y, src.scaled(
                    cell, cell,
                    Qt.AspectRatioMode.IgnoreAspectRatio,
                    Qt.TransformationMode.FastTransformation))
            # Grid line
            p.setPen(QPen(QColor(0, 0, 0, 120), 1))
            p.drawRect(x, y, cell-1, cell-1)
        p.end()
        self._canvas.setPixmap(pm)
        if not self._solved:
            self._info_lbl.setText(f"Moves: {self._moves}")



class _TileZoomView(QWidget):
    """Tile zoom view — sits inside a _view_tabs tab, scales tile to fill space.
    All sidebar tools in RadarWorkshop operate on self._tile_idx via _current_idx."""

    def __init__(self, tile_idx: int, tile_name: str, rgba: bytes, workshop): #vers 1
        super().__init__()
        self._tile_idx  = tile_idx
        self._tile_name = tile_name
        self._workshop  = workshop
        self._rgba      = rgba
        self._pixmap    = None
        self.setMinimumSize(128, 128)
        self.setSizePolicy(
            QSizePolicy.Policy.Expanding,
            QSizePolicy.Policy.Expanding)
        self._rebuild_pixmap()

    def _rebuild_pixmap(self): #vers 1
        img = QImage(self._rgba, TILE_W, TILE_H, TILE_W*4,
                     QImage.Format.Format_RGBA8888)
        self._pixmap = QPixmap.fromImage(img)
        self.update()

    def refresh(self, rgba: bytes): #vers 1
        self._rgba = rgba
        self._rebuild_pixmap()

    def paintEvent(self, ev): #vers 1
        if not self._pixmap: return
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)
        # Scale to fill, keep aspect ratio, centre
        sz  = self._pixmap.size().scaled(
            self.width(), self.height(),
            Qt.AspectRatioMode.KeepAspectRatio)
        x = (self.width()  - sz.width())  // 2
        y = (self.height() - sz.height()) // 2
        p.drawPixmap(x, y, self._pixmap.scaled(
            sz, Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation))
        # Tile info overlay at bottom
        p.setPen(QColor(255, 255, 255, 180))
        p.setFont(QFont("monospace", 9))
        info = f"{self._tile_idx}: {self._tile_name}  {TILE_W}×{TILE_H}"
        p.drawText(x + 4, y + sz.height() - 6, info)
        p.end()

    def resizeEvent(self, ev): #vers 1
        super().resizeEvent(ev)
        self.update()



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


class RadarWorkshop(ToolMenuMixin, QWidget): #vers 1
    """Radar Workshop – skeleton class"""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    def _build_menus_into_qmenu(self, pm): #vers 2
        fm = pm.addMenu("File")
        fm.addAction("Load IMG…",          self._open_file)
        fm.addAction("Save IMG…",          self._save_file)
        fm.addSeparator()
        fm.addAction("Export Full Map…",   self._export_sheet)
        fm.addAction("Import Full Map…",   self._import_sheet)
        fm.addSeparator()
        # Recent files
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
        em = pm.addMenu("Edit")
        em.addAction("Undo  Ctrl+Z",   self._undo)
        em.addAction("Redo  Ctrl+Y",   self._redo)
        em.addSeparator()
        em.addAction("Map Statistics…", self._show_stats)
        vm = pm.addMenu("View")
        vm.addAction("Zoom In (+)",    lambda: self._zoom(1.25))
        vm.addAction("Zoom Out (-)",   lambda: self._zoom(0.8))
        vm.addAction("Fit Grid",       self._fit)
        vm.addSeparator()
        vm.addAction("🧩 Boredom!",   self._start_boredom)
        vm.addSeparator()
        vm.addAction("About Radar Workshop", self._show_about)

    def __init__(self, parent=None, main_window=None): #Vers 1
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
            self.setWindowIcon(SVGIconFactory.radar_workshop_icon(64))
        except Exception:
            pass
        self.resize(1400, 800)
        self.setMinimumSize(800, 500)

        # Radar state
        self._img_reader:   Optional[ImgReader] = None
        self._img_path:     str = ""
        self._game_preset:  dict = GAME_PRESETS["SA PC"]
        self._current_idx:  int  = -1
        self._clipboard_tile: bytes = None   # copy/paste buffer
        self._undo_stack: list = []          # list of (idx, rgba) snapshots
        self._redo_stack: list = []
        self._tile_rgba:    Dict[int, bytes] = {}
        self._tile_entries: List[dict] = []
        self._dirty_tiles:  set = set()
        self._list_items:   List[TileListItem] = []

        # Frameless in standalone (custom titlebar), widget when docked
        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos()
            self.move(p.x() + 50, p.y() + 80)

        self.setup_ui()
        #self._setup_hotkeys()
        self._apply_theme()
        self._apply_preset("SA PC")

    def get_content_margins(self): #vers 1
        return (self.contmergina,self.contmerginb,self.contmerginc,self.contmergind)

    def get_panel_margins(self): #vers 1
        return (self.panelmergina,self.panelmerginb,self.panelmerginc,self.panelmergind)

    def get_tab_margins(self): #vers 1
        return (self.tabmerginsa,self.tabmerginsb,self.tabmerginsc,self.tabmerginsd)

    # ── setup_ui

    def setup_ui(self): #vers 1
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(*self.get_content_margins())
        main_layout.setSpacing(self.setspacing)

        toolbar = self._create_toolbar()
        main_layout.addWidget(toolbar)

        main_splitter = QSplitter(Qt.Orientation.Horizontal)

        # Left panel — hidden (same pattern as template: None in standalone)
        left = self._create_left_panel()
        centre = self._create_centre_panel()
        right  = self._create_right_panel()

        if left is not None:
            main_splitter.addWidget(left)
            main_splitter.addWidget(centre)
            main_splitter.addWidget(right)
            main_splitter.setStretchFactor(0,1)
            main_splitter.setStretchFactor(1,2)
            main_splitter.setStretchFactor(2,5)
        else:
            main_splitter.addWidget(centre)
            main_splitter.addWidget(right)
            main_splitter.setStretchFactor(0,1)
            main_splitter.setStretchFactor(1,4)

        main_layout.addWidget(main_splitter)

        self._status_bar = self._create_status_bar()
        main_layout.addWidget(self._status_bar)
        # Corner resize overlay set up in showEvent


    # - Toolbar

    def _create_toolbar(self): #Vers 1
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

        # - Game selector
        layout.addSpacing(8)
        layout.addWidget(QLabel("Game:"))
        self._game_combo = QComboBox()
        self._game_combo.addItems(list(GAME_PRESETS))
        self._game_combo.setCurrentText("SA PC")
        self._game_combo.currentTextChanged.connect(self._on_game_changed)
        layout.addWidget(self._game_combo)

        self._cols_spin = QSpinBox()
        self._cols_spin.setRange(1,100); self._cols_spin.setValue(8)
        self._cols_spin.setPrefix("W "); self._cols_spin.setEnabled(False)
        self._cols_spin.valueChanged.connect(self._on_custom_changed)
        layout.addWidget(self._cols_spin)

        self._rows_spin = QSpinBox()
        self._rows_spin.setRange(1,100); self._rows_spin.setValue(8)
        self._rows_spin.setPrefix("H "); self._rows_spin.setEnabled(False)
        self._rows_spin.valueChanged.connect(self._on_custom_changed)
        layout.addWidget(self._rows_spin)

        layout.addStretch()

        # - Info button (before Theme)
        self.info_radar_btn = QPushButton()
        self.info_radar_btn.setIcon(SVGIconFactory.info_icon(20, icon_color))
        self.info_radar_btn.setIconSize(QSize(20, 20))
        self.info_radar_btn.setFixedSize(35, 35)
        self.info_radar_btn.setToolTip("About Radar Workshop")
        self.info_radar_btn.clicked.connect(self._show_about)
        layout.addWidget(self.info_radar_btn)

        # - Theme / Properties
        self.properties_btn = QPushButton()
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(20, icon_color))
        self.properties_btn.setIconSize(QSize(20, 20))
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.setToolTip("Theme Settings")
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        layout.addWidget(self.properties_btn)

        # - Dock button — hidden in standalone (nothing to dock to)
        self.dock_btn = QPushButton("D")
        self.dock_btn.setMinimumWidth(40)
        self.dock_btn.setMaximumWidth(40)
        self.dock_btn.setMinimumHeight(30)
        self.dock_btn.setToolTip("Dock into IMG Factory")
        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        self.dock_btn.setVisible(not self.standalone_mode)
        layout.addWidget(self.dock_btn)

        # - Tear-off button — only when docked
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton("T")
            self.tearoff_btn.setMinimumWidth(40)
            self.tearoff_btn.setMaximumWidth(40)
            self.tearoff_btn.setMinimumHeight(30)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("Tear off to standalone window")
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
    def _create_left_panel(self): #vers 1
        # Hidden — template returns None in standalone
        return None


    # - Centre panel:
    def _create_centre_panel(self): #vers 6
        """Tile list with icon button row: Open/Save/Export/Import/Info."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        vl = QVBoxLayout(panel)
        vl.setContentsMargins(*self.get_panel_margins())
        vl.setSpacing(self.panelspacing)

        # ── Button row ────────────────────────────────────────────────────────
        icon_color = self._get_icon_color()
        btn_row = QHBoxLayout()
        btn_row.setSpacing(2)

        def _cb(icon_fn, tip, slot, enabled=True):
            b = QToolButton()
            b.setFixedSize(28, 28)
            b.setIcon(getattr(SVGIconFactory, icon_fn)(20, icon_color))
            b.setIconSize(QSize(20, 20))
            b.setToolTip(tip)
            b.setEnabled(enabled)
            b.clicked.connect(slot)
            btn_row.addWidget(b)
            return b

        self.open_btn   = _cb('open_icon',   "Load radar IMG (Ctrl+O)",      self._open_file)
        self.save_btn   = _cb('save_icon',   "Save modified tiles (Ctrl+S)", self._save_file, enabled=False)
        self.export_btn = _cb('export_icon', "Export all tiles as PNG sheet", self._export_sheet)
        self.import_btn = _cb('import_icon', "Import PNG sheet of tiles",    self._import_sheet)
        btn_row.addStretch()
        vl.addLayout(btn_row)

        # ── Tile search bar ────────────────────────────────────────────────────
        search_row = QHBoxLayout()
        self._tile_search = QLineEdit()
        self._tile_search.setPlaceholderText("Search tiles…  e.g. 64-95 or radar")
        self._tile_search.setClearButtonEnabled(True)
        self._tile_search.textChanged.connect(self._filter_tile_list)
        search_row.addWidget(self._tile_search)
        vl.addLayout(search_row)

        # ── Tile list ─────────────────────────────────────────────────────────
        self._tile_list = QListWidget()
        self._tile_list.setIconSize(QSize(THUMB, THUMB))
        self._tile_list.setUniformItemSizes(False)   # items have 2 text lines
        self._tile_list.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self._tile_list.currentRowChanged.connect(self._on_list_row)
        self._tile_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tile_list.customContextMenuRequested.connect(self._on_tile_list_context)
        self._tile_list.itemDoubleClicked.connect(lambda item: self._edit_tile_popup(item.idx))
        vl.addWidget(self._tile_list, 1)
        # TODO add bitdepth after tile size
        self._dirty_lbl = QLabel("Modified: 0")
        self._dirty_lbl.setFont(self.infobar_font)
        vl.addWidget(self._dirty_lbl)

        return panel

    def _filter_tile_list(self, text: str): #vers 1
        """Filter tile list by name or index range (e.g. '64-95' or 'radar0')."""
        import re as _re
        text = text.strip()
        range_m = _re.match(r'^(\d+)\s*[-–]\s*(\d+)$', text)
        for i in range(self._tile_list.count()):
            item = self._tile_list.item(i)
            if not text:
                item.setHidden(False)
            elif range_m:
                lo, hi = int(range_m.group(1)), int(range_m.group(2))
                item.setHidden(not (lo <= i <= hi))
            else:
                item.setHidden(text.lower() not in item.text().lower())

    def _push_undo(self, idx: int): #vers 1
        """Push current tile state onto undo stack before a change."""
        if idx not in self._tile_rgba: return
        self._undo_stack.append((idx, self._tile_rgba[idx]))
        if len(self._undo_stack) > 20:
            self._undo_stack.pop(0)
        self._redo_stack.clear()

    def _undo(self): #vers 1
        if not self._undo_stack:
            self._set_status("Nothing to undo"); return
        idx, rgba = self._undo_stack.pop()
        if idx in self._tile_rgba:
            self._redo_stack.append((idx, self._tile_rgba[idx]))
        self._tile_rgba[idx] = rgba
        self._radar.set_tile(idx, rgba, TILE_W, TILE_H)
        if idx < len(self._list_items):
            self._list_items[idx].set_thumb(rgba, TILE_W, TILE_H)
        self._refresh_tile_tab(idx)
        self._set_status(f"Undo — tile {idx}")

    def _redo(self): #vers 1
        if not self._redo_stack:
            self._set_status("Nothing to redo"); return
        idx, rgba = self._redo_stack.pop()
        self._undo_stack.append((idx, self._tile_rgba[idx]))
        self._tile_rgba[idx] = rgba
        self._radar.set_tile(idx, rgba, TILE_W, TILE_H)
        if idx < len(self._list_items):
            self._list_items[idx].set_thumb(rgba, TILE_W, TILE_H)
        self._refresh_tile_tab(idx)
        self._set_status(f"Redo — tile {idx}")


    # --- Right panel: settings
    def _create_right_panel(self): #vers 8
        """Radar grid + bottom palette strip + right sidebar: zoom/tools/swatches."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        hl = QHBoxLayout(panel)
        hl.setContentsMargins(0, 0, 0, 0)
        hl.setSpacing(0)

        # ── Centre: tab widget — Map tab + tile zoom tabs ────────────────────
        self._view_tabs = QTabWidget()
        self._view_tabs.setDocumentMode(True)
        self._view_tabs.setTabsClosable(False)  # Map tab never closable
        self._view_tabs.tabCloseRequested.connect(self._on_view_tab_close)

        # Tab 0: Full radar map
        map_container = QWidget()
        map_layout = QVBoxLayout(map_container)
        map_layout.setContentsMargins(0, 0, 0, 0)
        map_layout.setSpacing(0)

        self._radar = RadarGridWidget()
        self._radar.tile_clicked.connect(self._on_grid_click)
        self._radar.grid_right_clicked.connect(self._on_grid_right_click)
        sc = QScrollArea()
        sc.setWidget(self._radar)
        sc.setWidgetResizable(True)
        sc.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._radar_scroll = sc
        map_layout.addWidget(sc, 1)

        self._view_tabs.addTab(map_container, "🗺 Map")
        self._view_tabs.setTabsClosable(True)
        # Make the Map tab non-closable by removing its close button
        self._view_tabs.tabBar().setTabButton(0, self._view_tabs.tabBar().ButtonPosition.RightSide, None)

        self._view_tabs.currentChanged.connect(self._on_view_tab_changed)
        hl.addWidget(self._view_tabs, 1)

        # ── Right sidebar ─────────────────────────────────────────────────────
        sidebar = QFrame()
        sidebar.setFrameStyle(QFrame.Shape.StyledPanel)
        sidebar.setFixedWidth(80)
        sl = QVBoxLayout(sidebar)
        sl.setContentsMargins(2, 4, 2, 4)
        sl.setSpacing(2)

        icon_color = self._get_icon_color()

        def _nb(icon_fn, tip, slot, checkable=False):
            b = QToolButton()
            b.setFixedSize(72, 28)
            if icon_fn:
                try:
                    b.setIcon(getattr(SVGIconFactory, icon_fn)(18, icon_color))
                    b.setIconSize(QSize(18, 18))
                except Exception: pass
            b.setToolTip(tip)
            b.setCheckable(checkable)
            b.clicked.connect(slot)
            sl.addWidget(b)
            return b

        # ── Zoom tools ────────────────────────────────────────────────────────
        _nb('zoom_in_icon',   "Zoom in (+)",           lambda: self._zoom(1.25))
        _nb('zoom_out_icon',  "Zoom out (-)",          lambda: self._zoom(0.8))
        _nb('fit_grid_icon',  "Fit grid (Ctrl+0)",     self._fit)
        _nb('locate_icon',    "Jump to selected tile", self._jump)

        sl.addSpacing(4)
        sep1 = QFrame(); sep1.setFrameShape(QFrame.Shape.HLine)
        sl.addWidget(sep1)
        sl.addSpacing(4)

        # ── Draw tools ────────────────────────────────────────────────────────
        self._draw_tool = 'pencil'
        self._draw_btns = {}

        def _tool_btn(icon_fn, tip, tool_name):
            b = _nb(icon_fn, tip, lambda checked=False, t=tool_name: self._set_draw_tool(t),
                    checkable=True)
            self._draw_btns[tool_name] = b
            return b

        _tool_btn('paint_icon',    "Pencil — draw pixels (P)",    'pencil')
        _tool_btn('editer_icon',   "Line — draw a line (L)",      'line')
        _tool_btn('fill_icon',     "Fill — flood fill bucket (F)", 'fill')
        _tool_btn('dropper_icon',  "Dropper — pick colour (K)",   'picker')
        self._draw_btns['pencil'].setChecked(True)

        sl.addSpacing(4)
        sep1b = QFrame(); sep1b.setFrameShape(QFrame.Shape.HLine)
        sl.addWidget(sep1b)
        sl.addSpacing(4)

        # ── Transform tools ───────────────────────────────────────────────────
        _nb('rotate_cw_icon',   "Rotate tile +90°",      self._rotate_cw)
        _nb('rotate_ccw_icon',  "Rotate tile -90°",      self._rotate_ccw)
        _nb('flip_horz_icon',   "Flip tile horizontal",  self._flip_horz)
        _nb('flip_vert_icon',   "Flip tile vertical",    self._flip_vert)

        sl.addSpacing(4)
        sep1c = QFrame(); sep1c.setFrameShape(QFrame.Shape.HLine)
        sl.addWidget(sep1c)
        sl.addSpacing(4)

        # ── Edit tile popup ───────────────────────────────────────────────────
        _nb('search_icon', "Open tile editor window (E)",         self._edit_tile_popup)

        sl.addSpacing(4)
        sep2 = QFrame(); sep2.setFrameShape(QFrame.Shape.HLine)
        sl.addWidget(sep2)
        sl.addSpacing(4)

        # ── FG/BG colour swatches ─────────────────────────────────────────────
        sw_lbl = QLabel("FG/BG")
        sw_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        sw_lbl.setStyleSheet("font-size:9px;")
        sl.addWidget(sw_lbl)

        self._fg_color = QColor(255, 255, 255)
        self._bg_color = QColor(0, 0, 0)

        self._fg_btn = QPushButton()
        self._fg_btn.setFixedSize(72, 18)
        self._fg_btn.setToolTip("Foreground (left-click to pick)")
        self._fg_btn.clicked.connect(self._pick_fg_color)
        sl.addWidget(self._fg_btn)

        self._bg_btn = QPushButton()
        self._bg_btn.setFixedSize(72, 18)
        self._bg_btn.setToolTip("Background (right-click palette for BG)")
        self._bg_btn.clicked.connect(self._pick_bg_color)
        sl.addWidget(self._bg_btn)
        self._update_swatch_buttons()

        sl.addSpacing(4)
        sep3 = QFrame(); sep3.setFrameShape(QFrame.Shape.HLine)
        sl.addWidget(sep3)
        sl.addSpacing(2)

        # ── Palette — colours from current tile ───────────────────────────────
        pal_lbl = QLabel("Palette")
        pal_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        pal_lbl.setStyleSheet("font-size:9px;")
        sl.addWidget(pal_lbl)

        self._palette_widget = RadarPaletteWidget()
        self._palette_widget.color_picked.connect(self._on_palette_color)
        self._palette_widget.color_picked_bg.connect(self._on_palette_color_bg)
        # Height sized dynamically based on colour count — min 2 rows
        self._palette_widget.setMinimumHeight(RadarPaletteWidget.CELL * 2)
        sl.addWidget(self._palette_widget)

        sl.addStretch(0)
        hl.addWidget(sidebar)

        return panel

    def _set_draw_tool(self, tool: str): #vers 2
        """Switch active draw tool, update button states and cursor."""
        self._draw_tool = tool
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)
        # Update cursor on the radar grid
        cursors = {
            'pencil': Qt.CursorShape.CrossCursor,
            'line':   Qt.CursorShape.CrossCursor,
            'fill':   Qt.CursorShape.PointingHandCursor,
            'picker': Qt.CursorShape.WhatsThisCursor,
        }
        if hasattr(self, '_radar'):
            self._radar.setCursor(cursors.get(tool, Qt.CursorShape.ArrowCursor))

    def _pick_fg_color(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        c = QColorDialog.getColor(self._fg_color, self, "Foreground Colour")
        if c.isValid():
            self._fg_color = c
            self._update_swatch_buttons()

    def _pick_bg_color(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        c = QColorDialog.getColor(self._bg_color, self, "Background Colour")
        if c.isValid():
            self._bg_color = c
            self._update_swatch_buttons()

    def _update_swatch_buttons(self): #vers 1
        if hasattr(self, '_fg_btn'):
            self._fg_btn.setStyleSheet(
                f"background-color: {self._fg_color.name()}; border: 1px solid #888;")
        if hasattr(self, '_bg_btn'):
            self._bg_btn.setStyleSheet(
                f"background-color: {self._bg_color.name()}; border: 1px solid #888;")

    def _on_palette_color(self, color: QColor): #vers 1
        """Palette cell left-clicked — set as foreground colour."""
        self._fg_color = color
        self._update_swatch_buttons()

    def _on_palette_color_bg(self, color: QColor): #vers 1
        """Palette cell right-clicked — set as background colour."""
        self._bg_color = color
        self._update_swatch_buttons()

    # ── Tile transforms ───────────────────────────────────────────────────────
    def _get_current_rgba(self): #vers 1
        """Return bytearray of current tile RGBA, or None."""
        idx = self._current_idx
        if idx < 0 or idx not in self._tile_rgba:
            return None, -1
        return bytearray(self._tile_rgba[idx]), idx

    def _apply_tile_transform(self, transform_fn): #vers 1
        """Apply a pixel transform to the current tile and refresh."""
        from PIL import Image
        rgba, idx = self._get_current_rgba()
        if rgba is None:
            self._set_status("No tile selected"); return
        img = Image.frombytes("RGBA", (TILE_W, TILE_H), bytes(rgba))
        img = transform_fn(img)
        new_rgba = img.tobytes()
        self._tile_rgba[idx] = new_rgba
        self._dirty_tiles.add(idx)
        self._radar.set_tile(idx, new_rgba, TILE_W, TILE_H)
        self._radar.set_dirty(idx, True)
        if idx < len(self._list_items):
            self._list_items[idx].set_thumb(new_rgba, TILE_W, TILE_H)
        if hasattr(self, '_palette_widget'):
            self._palette_widget.set_colors_from_rgba(new_rgba, TILE_W, TILE_H)
        self._dirty_lbl.setText(f"Modified: {len(self._dirty_tiles)}")
        self._refresh_tile_tab(self._current_idx)
        self._set_status(f"Tile {idx} transformed — {len(self._dirty_tiles)} modified")

    def _rotate_cw(self): #vers 1
        from PIL import Image
        self._apply_tile_transform(lambda img: img.rotate(-90, expand=False))

    def _rotate_ccw(self): #vers 1
        from PIL import Image
        self._apply_tile_transform(lambda img: img.rotate(90, expand=False))

    def _flip_horz(self): #vers 1
        from PIL import Image
        self._apply_tile_transform(lambda img: img.transpose(Image.Transpose.FLIP_LEFT_RIGHT))

    def _flip_vert(self): #vers 1
        from PIL import Image
        self._apply_tile_transform(lambda img: img.transpose(Image.Transpose.FLIP_TOP_BOTTOM))


    def _create_right_panel_old(self): #Vers 1
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(80)
        panel.setMaximumWidth(180)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(8, 8, 8, 8)
        layout.setSpacing(10)

        header = QLabel("Paint Gadgets")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        layout.addWidget(header)

        layout.addStretch()

        # Settings button — only shown when docked (toolbar is hidden in docked mode)
        self.docked_settings_btn = QPushButton()
        self.docked_settings_btn.setFont(self.button_font)
        self.docked_settings_btn.setIcon(self.icon_factory.settings_icon())
        self.docked_settings_btn.setText("Settings / Options")
        self.docked_settings_btn.setIconSize(QSize(18, 18))
        self.docked_settings_btn.clicked.connect(self._show_workshop_settings)
        self.docked_settings_btn.setToolTip(App_name + " Workshop Settings")
        self.docked_settings_btn.setVisible(not self.standalone_mode)
        layout.addWidget(self.docked_settings_btn)

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

        # ── Fonts tab ─────────────────────────────────────────────────────────
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

        # ── Display tab ───────────────────────────────────────────────────────
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

        # ── Menu tab ──────────────────────────────────────────────────────────
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

        # ── Buttons ───────────────────────────────────────────────────────────
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


    def _apply_preset(self, game): #vers 4
        self._game_preset = GAME_PRESETS[game]
        cols  = self._game_preset["cols"]
        count = self._game_preset["count"]
        label = self._game_preset["label"]
        self._tile_rgba = {}; self._dirty_tiles = set(); self._current_idx = -1
        names = [self._game_preset["name_fn"](i) for i in range(count)]
        self._radar.setup(cols, count, names)

        # Block tile_list signals entirely while rebuilding — prevents
        # addItem() firing currentRowChanged(0) → _on_list_row(0) which
        # would set _current_idx=0 (the RADAR00 bug)
        self._tile_list.blockSignals(True)
        self._tile_list.clear()
        self._tile_list.setIconSize(QSize(THUMB, THUMB))
        self._list_items = []
        for i in range(count):
            item = TileListItem(i, names[i], game_label=label)
            self._tile_list.addItem(item)
            self._list_items.append(item)
        self._tile_list.blockSignals(False)
        self._tile_list.setCurrentRow(-1)   # no selection after load

        # Close any stale tile-zoom tabs from a previous file
        if hasattr(self, '_view_tabs'):
            self._view_tabs.blockSignals(True)
            while self._view_tabs.count() > 1:
                self._view_tabs.removeTab(self._view_tabs.count() - 1)
            self._view_tabs.blockSignals(False)
            self._view_tabs.setCurrentIndex(0)

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


    def _apply_infobar_font(self): #vers 1
        if hasattr(self,'status_label'): self.status_label.setFont(self.infobar_font)
        if hasattr(self,'_dirty_lbl'):   self._dirty_lbl.setFont(self.infobar_font)


    # - Docking (template pattern)
    def _update_dock_button_visibility(self): #vers 1
        if hasattr(self,'dock_btn'): self.dock_btn.setVisible(not self.is_docked)
        if hasattr(self,'tearoff_btn'): self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)

    def _setup_hotkeys(self): #vers 1
        self.hotkey_open  = QShortcut(QKeySequence.StandardKey.Open,  self); self.hotkey_open.activated.connect(self._open_file)
        self.hotkey_save  = QShortcut(QKeySequence.StandardKey.Save,  self); self.hotkey_save.activated.connect(self._save_file)
        self.hotkey_close = QShortcut(QKeySequence.StandardKey.Close, self); self.hotkey_close.activated.connect(self.close)
        self.hotkey_find  = QShortcut(QKeySequence.StandardKey.Find,  self)
        self.hotkey_help  = QShortcut(QKeySequence.StandardKey.HelpContents, self)
        # Zoom keys
        QShortcut(QKeySequence(Qt.Key.Key_Plus),  self).activated.connect(lambda: self._zoom(1.25))
        QShortcut(QKeySequence(Qt.Key.Key_Equal), self).activated.connect(lambda: self._zoom(1.25))
        QShortcut(QKeySequence(Qt.Key.Key_Minus), self).activated.connect(lambda: self._zoom(0.8))
        QShortcut(QKeySequence("Ctrl++"),         self).activated.connect(lambda: self._zoom(1.25))
        QShortcut(QKeySequence("Ctrl+-"),         self).activated.connect(lambda: self._zoom(0.8))
        QShortcut(QKeySequence("Ctrl+0"),         self).activated.connect(self._fit)
        # Tile edit popup
        QShortcut(QKeySequence(Qt.Key.Key_E), self).activated.connect(self._edit_tile_popup)
        QShortcut(QKeySequence("Ctrl+Z"), self).activated.connect(self._undo)
        QShortcut(QKeySequence("Ctrl+Y"), self).activated.connect(self._redo)
        QShortcut(QKeySequence("Ctrl+Shift+Z"), self).activated.connect(self._redo)
        # Draw tool keys
        QShortcut(QKeySequence(Qt.Key.Key_P), self).activated.connect(lambda: self._set_draw_tool('pencil'))
        QShortcut(QKeySequence(Qt.Key.Key_L), self).activated.connect(lambda: self._set_draw_tool('line'))
        QShortcut(QKeySequence(Qt.Key.Key_F), self).activated.connect(lambda: self._set_draw_tool('fill'))
        QShortcut(QKeySequence(Qt.Key.Key_K), self).activated.connect(lambda: self._set_draw_tool('picker'))
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

    def _open_file(self, path: str = ""): #vers 4
        """Open radar IMG/TXD/PVR file. Pass path to skip the file dialog."""
        source = self._game_preset.get("img_source", "img")
        hint   = self._game_preset.get("hint", "")

        # Formats not yet supported — show info and return
        if source in ("toc", "chk", "xtx"):
            label = self._game_preset.get("label", "")
            QMessageBox.information(self, "Format Not Yet Supported",
                f"{label}\n\n{hint}\n\n"
                f"This format ({source.upper()}) is not yet supported in Radar Workshop.\n"
                f"Support is planned for a future build.")
            return

        if not path:
            # Build file filter based on source
            if source == "txd":
                filt  = "TXD Files (*.txd);;All Files (*)"
                title = "Open Radar TXD"
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
            self._load_standalone_txd(path)
            return

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
                if i<len(self._list_items): self._list_items[i].set_thumb(rgba,w,h)
            except Exception as e: print(f"WARN tile {i}: {e}",file=sys.stderr)
        prog.setValue(len(entries)); self._dirty_tiles=set()
        self.save_btn.setEnabled(True)
        self.RAD_settings.add_recent(str(path))
        self._set_status(f"Loaded {len(entries)} tiles from {Path(path).name}  — game: {self._game_preset['label']}  grid: {self._game_preset['cols']}×{self._game_preset['rows']}")

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
        if self._list_items:
            self._list_items[0].set_thumb(rgba, w, h)
        self._dirty_tiles = set()
        self.save_btn.setEnabled(True)
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

    def _save_file(self): #vers 1
        if not self._img_reader or not self._dirty_tiles:
            QMessageBox.information(self,"Nothing to Save","No tiles modified."); return
        path,_=QFileDialog.getSaveFileName(self,"Save Radar IMG",self._img_path,"IMG Archives (*.img);;All Files (*)")
        if not path: return
        try:
            data=bytearray(self._img_reader._img_data)
            for idx in sorted(self._dirty_tiles):
                if idx>=len(self._tile_entries): continue
                e=self._tile_entries[idx]; rgba=self._tile_rgba.get(idx)
                if not rgba: continue
                new=RadarTxdReader.write(rgba,TILE_W,TILE_H,e["name"].replace(".txd","").replace(".TXD",""))
                off=e["offset"]; sz=e["size"]; data[off:off+sz]=(new+b"\x00"*max(0,sz-len(new)))[:sz]
            Path(path).write_bytes(bytes(data))
            sd=Path(self._img_path).with_suffix(".dir"); dd=Path(path).with_suffix(".dir")
            if sd.exists() and path!=self._img_path: shutil.copy2(sd,dd)
            for i in range(len(self._tile_entries)): self._radar.set_dirty(i,False)
            self._dirty_tiles=set(); self._dirty_lbl.setText("Modified: 0")
            self._set_status(f"Saved to {Path(path).name}")
        except Exception as e: QMessageBox.critical(self,"Save Error",str(e))

    # - Tile selection

    def _edit_tile_popup(self, idx: int = -1): #vers 3
        """Open a tile as a zoom tab in the main view area.
        Sidebar tools work directly — no separate window needed."""
        if idx < 0:
            idx = self._current_idx
        if idx < 0:
            self._set_status("Select a tile first"); return
        if idx not in self._tile_rgba:
            self._set_status(f"Tile {idx} not yet loaded — click it in the grid first"); return
        name = (self._tile_entries[idx]["name"]
                if idx < len(self._tile_entries) else f"tile_{idx}")
        self._set_status(f"Opening tile {idx}: {name}")
        self._open_tile_tab(idx)

    def _open_tile_tab(self, idx: int): #vers 1
        """Open tile idx as a zoom tab in the main view area.
        Tab is named 'idx: TILENAM'. Switching tabs auto-selects the tile."""
        if not hasattr(self, '_view_tabs'): return

        # Check if already open
        for i in range(1, self._view_tabs.count()):
            w = self._view_tabs.widget(i)
            if getattr(w, '_tile_idx', -1) == idx:
                self._view_tabs.setCurrentIndex(i)
                return

        name = (self._tile_entries[idx]["name"]
                if idx < len(self._tile_entries) else f"tile_{idx}")
        rgba = self._tile_rgba[idx]

        # Create a tile canvas widget that scales to fill the tab
        tile_view = _TileZoomView(idx, name, rgba, self)
        tab_label = f"{idx}: {name}"
        self._view_tabs.addTab(tile_view, tab_label)
        self._view_tabs.setCurrentWidget(tile_view)

    def _on_view_tab_changed(self, tab_idx: int): #vers 2
        """When user switches tabs — sync tile selection if it's a tile tab."""
        if tab_idx == 0:
            return  # Map tab — keep whatever tile is currently selected
        w = self._view_tabs.widget(tab_idx)
        tile_idx = getattr(w, '_tile_idx', -1)
        if tile_idx >= 0:
            # Update _current_idx without triggering another tab switch
            self._current_idx = tile_idx
            self._radar.set_selected(tile_idx)
            # Block signals so setCurrentRow doesn't re-fire _on_list_row
            self._tile_list.blockSignals(True)
            self._tile_list.setCurrentRow(tile_idx)
            self._tile_list.blockSignals(False)

    def _on_view_tab_close(self, tab_idx: int): #vers 1
        """Close a tile tab (Map tab at index 0 is never closable)."""
        if tab_idx == 0: return
        self._view_tabs.removeTab(tab_idx)

    def _refresh_tile_tab(self, idx: int): #vers 1
        """Refresh a tile tab's pixmap after a transform or import."""
        if not hasattr(self, '_view_tabs'): return
        if idx not in self._tile_rgba: return
        rgba = self._tile_rgba[idx]
        for i in range(1, self._view_tabs.count()):
            w = self._view_tabs.widget(i)
            if getattr(w, '_tile_idx', -1) == idx:
                w.refresh(rgba)
                break

    def _editor_refresh(self, tabs=None): #vers 2
        """Refresh the current tile tab — called after transforms."""
        self._refresh_tile_tab(self._current_idx)

    def _on_tile_list_context(self, pos): #vers 1
        """Right-click context menu on tile list item."""
        item = self._tile_list.itemAt(pos)
        if item is None: return
        idx = item.idx

        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        icon_color = self._get_icon_color()

        act_copy   = menu.addAction(
            SVGIconFactory.open_icon(16, icon_color),   "Copy tile")
        act_paste  = menu.addAction(
            SVGIconFactory.save_icon(16, icon_color),   "Paste tile")
        act_paste.setEnabled(hasattr(self, '_clipboard_tile') and self._clipboard_tile is not None)
        menu.addSeparator()
        act_export = menu.addAction(
            SVGIconFactory.export_icon(16, icon_color), "Export tile as PNG…")
        act_import = menu.addAction(
            SVGIconFactory.import_icon(16, icon_color), "Import tile from PNG…")
        menu.addSeparator()
        act_delete = menu.addAction(
            SVGIconFactory.delete_icon(16, icon_color), "Delete tile (reset to blank)")

        chosen = menu.exec(self._tile_list.mapToGlobal(pos))
        if chosen is None: return

        if chosen == act_copy:
            self._clipboard_tile = self._tile_rgba.get(idx)
            self._set_status(f"Copied tile {idx}")
        elif chosen == act_paste:
            if hasattr(self, '_clipboard_tile') and self._clipboard_tile:
                rgba = self._clipboard_tile
                self._tile_rgba[idx] = rgba
                self._dirty_tiles.add(idx)
                self._radar.set_tile(idx, rgba, TILE_W, TILE_H)
                self._radar.set_dirty(idx, True)
                if idx < len(self._list_items):
                    self._list_items[idx].set_thumb(rgba, TILE_W, TILE_H)
                self._refresh_tile_tab(idx)
                self.save_btn.setEnabled(True)
                self._dirty_lbl.setText(f"Modified: {len(self._dirty_tiles)}")
                self._set_status(f"Pasted to tile {idx}")
        elif chosen == act_export:
            self._export_single_tile(idx)
        elif chosen == act_import:
            self._import_single_tile(idx)
        elif chosen == act_delete:
            self._delete_single_tile(idx)

    def _export_single_tile(self, idx: int): #vers 1
        """Export a single tile as PNG."""
        if idx not in self._tile_rgba:
            QMessageBox.information(self, "No Data", "Tile not loaded."); return
        name = self._tile_entries[idx]["name"] if idx < len(self._tile_entries) else f"tile_{idx}"
        stem = Path(name).stem
        path, _ = QFileDialog.getSaveFileName(
            self, f"Export tile {stem}", f"{stem}.png",
            "PNG Image (*.png);;BMP Image (*.bmp)")
        if not path: return
        try:
            from PIL import Image
            rgba = self._tile_rgba[idx]
            img  = Image.frombytes("RGBA", (TILE_W, TILE_H), rgba)
            if path.lower().endswith('.bmp'): img = img.convert("RGB")
            img.save(path)
            self._set_status(f"Exported tile {idx} → {Path(path).name}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _import_single_tile(self, idx: int): #vers 1
        """Import a single tile from PNG/BMP."""
        path, _ = QFileDialog.getOpenFileName(
            self, f"Import tile {idx}", "",
            "PNG Image (*.png);;BMP Image (*.bmp);;All Images (*)")
        if not path: return
        try:
            from PIL import Image
            img  = Image.open(path).convert("RGBA")
            if img.size != (TILE_W, TILE_H):
                img = img.resize((TILE_W, TILE_H), Image.LANCZOS)
            rgba = img.tobytes()
            self._tile_rgba[idx]  = rgba
            self._dirty_tiles.add(idx)
            self._radar.set_tile(idx, rgba, TILE_W, TILE_H)
            self._radar.set_dirty(idx, True)
            if idx < len(self._list_items):
                self._list_items[idx].set_thumb(rgba, TILE_W, TILE_H)
            if hasattr(self, '_palette_widget') and self._current_idx == idx:
                self._palette_widget.set_colors_from_rgba(rgba, TILE_W, TILE_H)
            self.save_btn.setEnabled(True)
            self._dirty_lbl.setText(f"Modified: {len(self._dirty_tiles)}")
            self._set_status(f"Imported tile {idx} from {Path(path).name}")
        except Exception as e:
            QMessageBox.critical(self, "Import Error", str(e))

    def _delete_single_tile(self, idx: int): #vers 1
        """Reset a tile to blank (black RGBA)."""
        from PyQt6.QtWidgets import QMessageBox as _MB
        if _MB.question(self, "Delete Tile",
                f"Reset tile {idx} to blank?",
                _MB.StandardButton.Yes | _MB.StandardButton.No) != _MB.StandardButton.Yes:
            return
        rgba = bytes(TILE_W * TILE_H * 4)  # all zeros = transparent black
        self._tile_rgba[idx]  = rgba
        self._dirty_tiles.add(idx)
        self._radar.set_tile(idx, rgba, TILE_W, TILE_H)
        self._radar.set_dirty(idx, True)
        if idx < len(self._list_items):
            self._list_items[idx].set_thumb(rgba, TILE_W, TILE_H)
        self.save_btn.setEnabled(True)
        self._dirty_lbl.setText(f"Modified: {len(self._dirty_tiles)}")
        self._set_status(f"Tile {idx} reset to blank")

    def _on_list_row(self, row): #vers 3
        if row < 0: return
        self._current_idx = row
        self._radar.set_selected(row)
        name = (self._tile_entries[row]["name"] if row < len(self._tile_entries)
                else self._game_preset["name_fn"](row))
        # Determine format/bitdepth from current preset
        lbl = self._game_preset.get("label", "")
        if "PS2" in lbl:   fmt = "PAL8 · 8bpp"
        elif "Xbox" in lbl: fmt = "DXT1 · 4bpp"
        elif "iOS" in lbl or "PVR" in lbl: fmt = "PVRTC · 4bpp"
        else:               fmt = "DXT1 · 4bpp"
        mod = "  [modified]" if row in self._dirty_tiles else ""
        self._set_status(f"Tile {row}  |  {name}  |  {TILE_W}×{TILE_H}  {fmt}{mod}")
        # Update palette from tile colours
        if hasattr(self, '_palette_widget') and row in self._tile_rgba:
            self._palette_widget.set_colors_from_rgba(self._tile_rgba[row], TILE_W, TILE_H)

    def _on_grid_click(self, idx): #vers 3
        """Grid tile clicked — set _current_idx directly then sync list."""
        self._current_idx = idx          # set first, before any signal fires
        self._radar.set_selected(idx)
        self._tile_list.blockSignals(True)
        self._tile_list.setCurrentRow(idx)
        self._tile_list.blockSignals(False)
        self._on_list_row(idx)           # update palette, status bar etc.

    def _on_grid_right_click(self, idx: int, gpos): #vers 1
        """Right-click on grid tile — context menu with edit/colour/export."""
        from PyQt6.QtWidgets import QMenu
        name = (self._tile_entries[idx]["name"]
                if idx < len(self._tile_entries) else f"tile_{idx}")
        icon_c = self._get_icon_color()

        menu = QMenu(self)
        menu.addSection(f"Tile {idx}: {name}")
        act_edit   = menu.addAction(SVGIconFactory.search_icon(16, icon_c),  "Open in tile editor (E)")
        menu.addSeparator()
        act_copy   = menu.addAction(SVGIconFactory.open_icon(16, icon_c),    "Copy tile")
        act_paste  = menu.addAction(SVGIconFactory.save_icon(16, icon_c),    "Paste tile")
        act_paste.setEnabled(bool(getattr(self, '_clipboard_tile', None)))
        menu.addSeparator()
        act_swap   = menu.addAction("Replace dominant colour with FG")
        act_fill   = menu.addAction("Fill tile solid with FG colour")
        menu.addSeparator()
        act_export = menu.addAction(SVGIconFactory.export_icon(16, icon_c),  "Export tile as PNG…")
        act_import = menu.addAction(SVGIconFactory.import_icon(16, icon_c),  "Import tile from PNG…")
        menu.addSeparator()
        act_delete = menu.addAction(SVGIconFactory.delete_icon(16, icon_c),  "Reset tile to blank")

        chosen = menu.exec(gpos)
        if chosen is None: return
        if chosen == act_edit:
            self._current_idx = idx; self._edit_tile_popup(idx)
        elif chosen == act_copy:
            self._clipboard_tile = self._tile_rgba.get(idx)
            self._set_status(f"Copied tile {idx}: {name}")
        elif chosen == act_paste:
            if getattr(self, '_clipboard_tile', None):
                self._apply_tile_data(idx, self._clipboard_tile)
                self._set_status(f"Pasted to tile {idx}: {name}")
        elif chosen == act_swap:
            self._grid_swap_color(idx, self._fg_color)
        elif chosen == act_fill:
            self._grid_fill_solid(idx, self._fg_color)
        elif chosen == act_export:
            self._export_single_tile(idx)
        elif chosen == act_import:
            self._import_single_tile(idx)
        elif chosen == act_delete:
            self._delete_single_tile(idx)

    def _apply_tile_data(self, idx: int, rgba: bytes): #vers 1
        """Apply rgba bytes to a tile slot — updates grid, list thumb, dirty."""
        self._push_undo(idx)
        self._tile_rgba[idx] = rgba
        self._dirty_tiles.add(idx)
        self._radar.set_tile(idx, rgba, TILE_W, TILE_H)
        self._radar.set_dirty(idx, True)
        if idx < len(self._list_items):
            self._list_items[idx].set_thumb(rgba, TILE_W, TILE_H)
        self._refresh_tile_tab(idx)
        self.save_btn.setEnabled(True)
        self._dirty_lbl.setText(f"Modified: {len(self._dirty_tiles)}")

    def _grid_swap_color(self, idx: int, new_color: QColor): #vers 1
        """Replace the most dominant colour in a tile with new_color."""
        if idx not in self._tile_rgba: return
        from collections import Counter
        rgba = bytearray(self._tile_rgba[idx])
        nr, ng, nb_v = new_color.red(), new_color.green(), new_color.blue()
        buckets = Counter()
        for i in range(0, len(rgba)-3, 4):
            if rgba[i+3] > 16:
                buckets[(rgba[i]>>4, rgba[i+1]>>4, rgba[i+2]>>4)] += 1
        if not buckets: return
        dom = buckets.most_common(1)[0][0]
        dr, dg, db_v = dom[0]<<4, dom[1]<<4, dom[2]<<4
        for i in range(0, len(rgba)-3, 4):
            if (rgba[i+3] > 16 and abs(rgba[i]-dr) < 32
                    and abs(rgba[i+1]-dg) < 32 and abs(rgba[i+2]-db_v) < 32):
                rgba[i], rgba[i+1], rgba[i+2] = nr, ng, nb_v
        self._apply_tile_data(idx, bytes(rgba))
        self._set_status(f"Swapped dominant colour in tile {idx}")

    def _grid_fill_solid(self, idx: int, color: QColor): #vers 1
        """Fill entire tile with a solid colour."""
        if idx not in self._tile_rgba: return
        r, g, b = color.red(), color.green(), color.blue()
        self._apply_tile_data(idx, bytes([r, g, b, 255] * (TILE_W * TILE_H)))
        self._set_status(f"Filled tile {idx} with FG colour")

    # - Export/Import sheet
    def _export_sheet(self): #vers 2
        """Export all loaded tiles as a single combined PNG or BMP map image."""
        if not self._tile_rgba:
            QMessageBox.information(self, "Nothing to Export", "Load a radar IMG first.")
            return
        preset = self._game_preset
        cols   = preset["cols"]
        rows   = preset.get("rows", (len(self._tile_rgba) + cols - 1) // cols)
        default_name = f"radar_map_{preset['cols']}x{preset['rows']}.png"
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Full Radar Map", default_name,
            "PNG Image (*.png);;BMP Image (*.bmp);;All Images (*)")
        if not path: return
        try:
            from PIL import Image
            sheet = Image.new("RGBA", (cols * TILE_W, rows * TILE_H), (0, 0, 0, 255))
            exported = 0
            for idx, rgba in self._tile_rgba.items():
                if len(rgba) == TILE_W * TILE_H * 4:
                    x = (idx % cols) * TILE_W
                    y = (idx // cols) * TILE_H
                    sheet.paste(Image.frombytes("RGBA", (TILE_W, TILE_H), rgba), (x, y))
                    exported += 1
            # BMP doesn't support alpha — convert to RGB
            if path.lower().endswith('.bmp'):
                sheet = sheet.convert("RGB")
            sheet.save(path)
            w_px, h_px = cols * TILE_W, rows * TILE_H
            self._set_status(
                f"Exported {exported} tiles → {Path(path).name}  ({w_px}×{h_px}px)")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _import_sheet(self): #vers 2
        """Import a full map PNG/BMP and slice it into tiles using preset grid dimensions."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Full Radar Map", "",
            "PNG Image (*.png);;BMP Image (*.bmp);;All Images (*)")
        if not path: return
        try:
            from PIL import Image
            sheet  = Image.open(path).convert("RGBA")
            preset = self._game_preset
            cols   = preset["cols"]
            rows   = preset["rows"]
            count  = preset["count"]
            # Tile size from the image (may differ from TILE_W/H if different game)
            tw = sheet.width  // cols
            th = sheet.height // rows
            if tw <= 0 or th <= 0:
                QMessageBox.warning(self, "Import Error",
                    f"Image size {sheet.width}×{sheet.height} is too small for "
                    f"{cols}×{rows} grid.")
                return
            imported = 0
            for idx in range(min(count, cols * rows)):
                col_i = idx % cols
                row_i = idx // cols
                tile  = sheet.crop((col_i * tw, row_i * th,
                                    (col_i+1) * tw, (row_i+1) * th))
                if tw != TILE_W or th != TILE_H:
                    tile = tile.resize((TILE_W, TILE_H), Image.LANCZOS)
                rgba = tile.tobytes()
                self._tile_rgba[idx] = rgba
                self._dirty_tiles.add(idx)
                self._radar.set_tile(idx, rgba, TILE_W, TILE_H)
                self._radar.set_dirty(idx, True)
                if idx < len(self._list_items):
                    self._list_items[idx].set_thumb(rgba, TILE_W, TILE_H)
                imported += 1
            if hasattr(self, '_palette_widget') and 0 in self._tile_rgba:
                self._palette_widget.set_colors_from_rgba(
                    self._tile_rgba[0], TILE_W, TILE_H)
            self.save_btn.setEnabled(True)
            self._dirty_lbl.setText(f"Modified: {len(self._dirty_tiles)}")
            self._set_status(
                f"Imported {imported} tiles from {Path(path).name}  "
                f"(sliced {cols}×{rows} from {sheet.width}×{sheet.height}px)")
        except Exception as e:
            QMessageBox.critical(self, "Import Error", str(e))

    # - Grid nav
    def _zoom(self, f): #vers 2
        """Scale the radar grid by adjusting its zoom level."""
        if not hasattr(self, '_grid_zoom'): self._grid_zoom = 1.0
        self._grid_zoom = max(0.1, min(8.0, self._grid_zoom * f))
        self._apply_grid_zoom()

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

    def _fit(self): #vers 2
        """Reset to fit-in-window zoom."""
        self._grid_zoom = 1.0
        self._apply_grid_zoom()
    def _jump(self): #vers 1
        if self._current_idx>=0:
            self._tile_list.scrollToItem(self._tile_list.item(self._current_idx),QAbstractItemView.ScrollHint.PositionAtCenter)

    # - Status
    def _set_status(self, msg): #vers 1
        if hasattr(self,'status_label'): self.status_label.setText(msg)


    # - Theme & icon helpers (template pattern)
    def _apply_theme(self): #vers 1
        try:
            if self.app_settings:
                self.setStyleSheet(self.app_settings.get_stylesheet())
            else:
                self.setStyleSheet("QWidget{background:#2b2b2b;color:#e0e0e0;} QPushButton{background:#3c3f41;border:1px solid #555;color:#e0e0e0;padding:2px 4px;} QPushButton:hover{background:#4a4d50;}")
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

    def _show_stats(self): #vers 1
        """Map statistics — unique colours, duplicate tiles, modified count."""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QTextEdit, QPushButton
        if not self._tile_rgba:
            QMessageBox.information(self, "No Data", "Load an IMG file first."); return

        total = len(self._tile_entries)
        loaded = len(self._tile_rgba)
        modified = len(self._dirty_tiles)

        # Unique colours across all tiles
        seen_colors: set = set()
        tile_hashes: dict = {}
        for idx, rgba in self._tile_rgba.items():
            import hashlib
            h = hashlib.md5(rgba).hexdigest()
            tile_hashes[idx] = h
            step = 4
            for i in range(0, len(rgba)-3, step*4):
                r,g,b,a = rgba[i],rgba[i+1],rgba[i+2],rgba[i+3]
                if a > 16:
                    seen_colors.add((r>>3, g>>3, b>>3))

        # Find duplicate tiles
        from collections import defaultdict
        hash_groups = defaultdict(list)
        for idx, h in tile_hashes.items():
            hash_groups[h].append(idx)
        duplicates = {h:idxs for h,idxs in hash_groups.items() if len(idxs) > 1}

        lines = [
            f"<b>Tiles:</b> {loaded} loaded of {total} total",
            f"<b>Modified:</b> {modified}",
            f"<b>Unique colours:</b> ~{len(seen_colors)} (sampled)",
            f"<b>Duplicate tile groups:</b> {len(duplicates)}",
        ]
        if duplicates:
            lines.append("<br><b>Top duplicates:</b>")
            for h, idxs in list(duplicates.items())[:8]:
                names = [self._tile_entries[i]["name"] if i < len(self._tile_entries)
                         else f"tile_{i}" for i in idxs[:4]]
                lines.append(f"  {', '.join(names)}{'…' if len(idxs)>4 else ''}")

        dlg = QDialog(self)
        dlg.setWindowTitle("Map Statistics")
        dlg.resize(420, 320)
        try:
            from apps.core.theme_utils import apply_dialog_theme
            apply_dialog_theme(dlg, self.main_window)
        except Exception: pass
        layout = QVBoxLayout(dlg)
        t = QTextEdit(); t.setReadOnly(True)
        t.setHtml("<br>".join(lines))
        layout.addWidget(t)
        ok = QPushButton("Close"); ok.clicked.connect(dlg.accept)
        layout.addWidget(ok)
        dlg.exec()

    def _start_boredom(self): #vers 1
        """🧩 Boredom! — sliding puzzle using radar tiles."""
        if not self._tile_rgba:
            QMessageBox.information(self, "Boredom!", "Load radar tiles first!"); return
        cols = self._game_preset.get("cols", 8)
        rows = self._game_preset.get("rows", 8)
        if cols * rows > 256:
            QMessageBox.information(self, "Boredom!",
                f"SOL ({cols}x{rows}) is too large for a puzzle! "
                "Load a VC/III (8x8) or SA (12x12) map instead."); return
        dlg = _BoredomPuzzle(self._tile_rgba, cols, rows, TILE_W, TILE_H, self)
        dlg.exec()

    def _show_about(self): #vers 2
        """Show Radar Workshop instructions and info dialog."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QTabWidget,
                                     QWidget, QTextEdit, QPushButton, QHBoxLayout)
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Radar Workshop — Help & Information")
        dlg.setMinimumSize(520, 560)
        dlg.resize(560, 600)
        layout = QVBoxLayout(dlg)
        try:
            from apps.core.theme_utils import apply_dialog_theme
            apply_dialog_theme(dlg, self.main_window)
        except Exception: pass

        tabs = QTabWidget()

        def _tab(html):
            t = QTextEdit(); t.setReadOnly(True); t.setHtml(html); return t

        # ── Quick Start ────────────────────────────────────────────────────────
        # App_name App_build App_auth Build = TODO - add auther info above Quickstart
        quickstart = """<p style="font-size:11px; color:#888; margin-bottom:8px;">
<b>{}</b> &nbsp;·&nbsp; {}&nbsp;·&nbsp; {}&nbsp;·&nbsp; {}
</p>
<hr>
<h3>Quick Start</h3>""".format(App_name, Build, App_build, App_auth) + """
<ol>
<li>
<li><b>Open an IMG file</b> — click the <b>Open</b> button or press <b>Ctrl+O</b>.<br>
    Load <code>gta3.img</code> for GTA III / VC / SA / LCS / VCS, or <code>RadarTex.img</code> for SOL.</li>
<li><b>Game auto-detected</b> by tile count — 64 tiles = 8×8 grid, 144 = 12×12, 1296 = 36×36.</li>
<li><b>Click a tile</b> in the left list or the grid to select it.</li>
<li><b>Double-click a tile</b> (or press <b>E</b>) to open a zoomed 512px edit popup — useful for SOL.</li>
<li><b>Draw</b> on the selected tile using the tools on the right sidebar.</li>
<li><b>Save</b> changes back to the IMG file with the Save button (Ctrl+S).</li>
</ol>
<h3>Keyboard Shortcuts</h3>
<table border=0 cellpadding=3>
<tr><td><b>Ctrl+O</b></td><td>Open IMG file</td></tr>
<tr><td><b>Ctrl+S</b></td><td>Save modified tiles back to IMG</td></tr>
<tr><td><b>+  /  -</b></td><td>Zoom in / Zoom out</td></tr>
<tr><td><b>Ctrl+0</b></td><td>Fit grid to window</td></tr>
<tr><td><b>E</b></td><td>Edit current tile in popup</td></tr>
<tr><td><b>P</b></td><td>Pencil draw tool</td></tr>
<tr><td><b>L</b></td><td>Line draw tool</td></tr>
<tr><td><b>F</b></td><td>Fill (flood fill) tool</td></tr>
<tr><td><b>K</b></td><td>Dropper (colour picker)</td></tr>
</table>"""

        # ── Tile List ──────────────────────────────────────────────────────────
        tilelist = """<h3>Tile List (Left Panel)</h3>
<p>Shows all radar tiles with 64×64 thumbnail, name, game badge [SA] / [VC] etc., and tile size.</p>
<ul>
<li><b>Click</b> — select and view tile in the grid.</li>
<li><b>Double-click</b> — open zoomed edit popup (512×512).</li>
<li><b>Right-click</b> — context menu:
  <ul>
  <li><b>Export tile as PNG/BMP</b> — save single tile to file.</li>
  <li><b>Import tile from PNG/BMP</b> — replace tile (auto-resizes to 128×128).</li>
  <li><b>Delete tile</b> — reset to blank transparent black.</li>
  </ul>
</li>
</ul>
<h3>Right Sidebar</h3>
<ul>
<li><b>🔍+ / 🔍−</b> — Zoom in / Zoom out on the grid.</li>
<li><b>Fit / Jump</b> — fit grid to window / scroll to selected tile.</li>
<li><b>Pencil / Line / Fill / Dropper</b> — draw tools (also P/L/F/K keys).</li>
<li><b>Edit tile</b> — open zoomed 512px popup for detailed editing.</li>
<li><b>Rotate ±90° / Flip H/V</b> — transform the current tile (uses PIL).</li>
<li><b>FG/BG swatches</b> — foreground / background colour; click to pick.</li>
<li><b>Palette</b> — colours extracted from current tile. Left-click = FG, right-click = BG.</li>
</ul>"""

        # ── Export/Import ──────────────────────────────────────────────────────
        exportinfo = """<h3>Export / Import Full Map</h3>
<p>The <b>Export</b> button saves all loaded tiles assembled into one full-size PNG or BMP.</p>
<ul>
<li>SA export = 1536×1536px (12×12 tiles × 128px)</li>
<li>VC/III/LCS/VCS = 1024×1024px (8×8 × 128px)</li>
<li>SOL = 4608×4608px (36×36 × 128px)</li>
</ul>
<p>The <b>Import</b> button loads a PNG/BMP and slices it into tiles automatically using the current grid dimensions. Any size image is accepted — it's resampled to fit.</p>
<h3>Recent Files</h3>
<p>The <b>File &gt; Recent Files</b> menu remembers the last 10 opened IMG archives. Window size and position are also saved automatically.</p>"""

        # ── Formats ───────────────────────────────────────────────────────────
        formats = """<h3>Supported Formats</h3>
<table border=1 cellpadding=4 cellspacing=0>
<tr><th>Game</th><th>File</th><th>Tiles</th><th>Grid</th><th>Format</th></tr>
<tr><td>GTA III PC/PS2/Xbox</td><td>gta3.img</td><td>64</td><td>8×8</td><td>DXT1 TXD</td></tr>
<tr><td>GTA VC PC/PS2/Xbox</td><td>gta3.img</td><td>64</td><td>8×8</td><td>DXT1 TXD</td></tr>
<tr><td>GTA SA PC/PS2/Xbox</td><td>gta3.img</td><td>144</td><td>12×12</td><td>DXT1 TXD</td></tr>
<tr><td>GTA LCS PC/PS2</td><td>gta3.img</td><td>64</td><td>8×8</td><td>DXT1 TXD</td></tr>
<tr><td>GTA VCS PC/PS2</td><td>gta3.img</td><td>64</td><td>8×8</td><td>DXT1 TXD</td></tr>
<tr><td>GTA SOL PC</td><td>RadarTex.img</td><td>1296</td><td>36×36</td><td>DXT1 TXD</td></tr>
<tr><td>GTA III Android</td><td>gta3_unc.img</td><td>1</td><td>1×1</td><td>Single 256×256 TXD</td></tr>
<tr><td>GTA LCS iOS</td><td>gta3.pvr</td><td>64</td><td>8×8</td><td>PVRTC TXD</td></tr>
</table>
<h3>Not Yet Supported</h3>
<ul>
<li>SA/LCS Android &amp; iOS — TOC/TMB/DAT encrypted format</li>
<li>LCS/VCS PSP — GIM/XTX ('xet\0') format</li>
<li>VC Android — texdb format</li>
</ul>"""

        tabs.addTab(_tab(quickstart), "Quick Start")
        tabs.addTab(_tab(tilelist),   "Tile List & Tools")
        tabs.addTab(_tab(exportinfo), "Export / Import")
        tabs.addTab(_tab(formats),    "Formats")
        layout.addWidget(tabs, 1)

        btn_row = QHBoxLayout()
        btn_row.addStretch()
        ok = QPushButton("Close"); ok.clicked.connect(dlg.accept); ok.setFixedWidth(80)
        btn_row.addWidget(ok)
        layout.addLayout(btn_row)
        dlg.exec()

    def _show_info(self): #vers 1
        lines = [
            f"<b>{App_name}</b>  Build {Build}",
            f"Author: {App_auth}  —  {App_build}",
            "",
            "GTA radar tile editor",
            "Supports: SA · VC · VCS · LC · LCS · SOL · Custom",
            "",
            f"IMG loaded: {Path(self._img_path).name if self._img_path else 'None'}",
            f"Tiles loaded: {len(self._tile_rgba)}",
            f"Modified tiles: {len(self._dirty_tiles)}",
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


    def _show_settings_context_menu(self, pos): #vers 1
        menu = QMenu(self)
        menu.addAction("Move Window",      self._enable_move_mode)
        menu.addAction("Maximize/Restore", self._toggle_maximize)
        menu.addAction("Minimize",         self.showMinimized)
        menu.exec(self.properties_btn.mapToGlobal(pos))



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

    def _dock_to_main(self): #Vers 1
        self.is_docked = True
        self.standalone_mode = False
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(False)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(False)
        self.show(); self.raise_()

    def _undock_from_main(self): #Vers 1
        self.standalone_mode = True
        self.is_docked = False
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(True)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(False)
        if hasattr(self, 'settings_btn'):
            self.settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(False)
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


    def _get_resize_corner(self, pos): #Vers 1
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

    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        self._refresh_corner_overlay()

    def showEvent(self, event): #vers 2
        super().showEvent(event)
        if self.standalone_mode:
            # Small delay ensures window geometry is finalised
            QTimer.singleShot(150, self._setup_corner_overlay)

    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        if hasattr(self,'size_grip'): self.size_grip.move(self.width()-16,self.height()-16)
        self._refresh_corner_overlay()

    def closeEvent(self, event): #Vers 2
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


def open_radar_workshop(main_window=None):
    try:
        w = RadarWorkshop(None, main_window); w.show(); return w
    except Exception as e:
        if main_window: QMessageBox.critical(main_window,App_name+" Error",str(e))
        return None

__all__ = ["RadarWorkshop", "open_radar_workshop"]

# Standalone entry point

if __name__ == "__main__": #Vers 1
    import traceback

    print(App_name + " starting…")
    try:
        app = QApplication(sys.argv)
        w = RadarWorkshop()
        w.setWindowTitle(App_name + " – Standalone")
        w.resize(1300, 800); w.show(); sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc();sys.exit(1)
