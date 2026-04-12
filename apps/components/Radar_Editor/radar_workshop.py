#!/usr/bin/env python3
#this belongs in apps/components/Radar_Editor/radar_workshop.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6 - Radar Workshop
# Based on gui_template.py (GUIWorkshop base)
# Layout: left panel hidden | centre=tile list | right=radar grid preview
# Tool bar uses template pattern: titlebar + toolbar with all standard buttons

import os, json, sys, requests, threading, struct, re, math, shutil
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple


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
Build     = "1"


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


# - Shared infrastructure (already imported above)
if False:
    ICONS_AVAILABLE = False
    class SVGIconFactory:
        def __getattr__(self, name):
            return lambda *a, **k: QIcon()
        @staticmethod
        def clear_cache(): pass

# - Game presets
def _name_sa(idx):  return f"RADAR{idx:02d}" #vers 1

def _name_sol(idx): return f"radar{idx:04d}" #vers 1

GAME_PRESETS = {
    "SA":     {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA San Andreas"},
    "VC":     {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA Vice City"},
    "VCS":    {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA Vice City Stories"},
    "LC":     {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA Liberty City (III)"},
    "LCS":    {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA Liberty City Stories"},
    "SOL":    {"cols":36, "rows":36, "count":1296,  "name_fn":_name_sol, "img_pattern":r"^radar\d{4}\.txd$",                    "label":"GTA State of Liberty"},
    "Custom": {"cols":8,  "rows":8,  "count":64,   "name_fn":_name_sa,  "img_pattern":r"^radar",                               "label":"Custom Grid"},
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
    def read(data): #vers 1
        pos=12
        while pos+12<=len(data):
            st=struct.unpack_from('<I',data,pos)[0]; ss=struct.unpack_from('<I',data,pos+4)[0]
            if st==0x15:
                th=pos+24; name=data[th+8:th+40].rstrip(b'\x00').decode('latin1','replace')
                ww=struct.unpack_from('<H',data,th+80)[0]; hh=struct.unpack_from('<H',data,th+82)[0]
                comp=struct.unpack_from('<B',data,th+87)[0]; dsz=struct.unpack_from('<I',data,th+88)[0]
                pd=data[th+92:th+92+dsz]
                rgba=decode_dxt1(pd,ww,hh) if comp==1 else bytes(pd[:ww*hh*4])
                return rgba,ww,hh,name
            pos+=12+ss
            if ss==0: break
        raise ValueError("No Texture Native section")

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
    def _load(self): #vers 1
        p=Path(self.img_path); raw=p.read_bytes()
        if raw[:4]==b'VER2':
            n=struct.unpack_from('<I',raw,4)[0]
            for i in range(n):
                os2,sz2,nb=struct.unpack_from('<II32s',raw,8+i*40)
                self.entries.append({'name':nb.rstrip(b'\x00').decode('latin1','replace'),'offset':os2*2048,'size':sz2*2048})
            self._img_data=raw
        else:
            dp=p.with_suffix('.dir')
            if not dp.exists(): raise FileNotFoundError(f"No .dir for {p.name}")
            dr=dp.read_bytes()
            for i in range(len(dr)//32):
                os2,sz2,nb=struct.unpack_from('<II24s',dr,i*32)
                self.entries.append({'name':nb.rstrip(b'\x00').decode('latin1','replace'),'offset':os2*2048,'size':sz2*2048})
            self._img_data=raw

    def get_entry_data(self,e): #vers 1
        return self._img_data[e['offset']:e['offset']+e['size']]

    def find_radar_entries(self,pat): #vers 1
        p=re.compile(pat,re.IGNORECASE); return [e for e in self.entries if p.match(e['name'])]


# - Radar grid widget
class RadarGridWidget(QWidget):
    """Full radar grid — no gaps, 1px grid lines, hover=tile name tooltip."""
    tile_clicked = pyqtSignal(int)

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

    def mousePressEvent(self,ev): #vers 1
        if ev.button()==Qt.MouseButton.LeftButton:
            idx=self._idx_at(ev.pos())
            if idx>=0: self._sel=idx; self.tile_clicked.emit(idx); self.update()

# - Tile list item

THUMB=32

class TileListItem(QListWidgetItem):
    def __init__(self,idx,name):
        super().__init__(); self.idx=idx
        self.setText(f"  {name}"); self.setSizeHint(QSize(0,THUMB+4))
    def set_thumb(self,rgba,w,h):
        img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
        self.setIcon(QIcon(QPixmap.fromImage(img).scaled(THUMB,THUMB,Qt.AspectRatioMode.KeepAspectRatio,Qt.TransformationMode.SmoothTransformation)))

# - Main Class

class RadarWorkshop(ToolMenuMixin, QWidget): #vers 1
    """Radar tile editor — based on gui_template GUIWorkshop pattern."""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    def get_menu_title(self): return App_name  # vers 1
    def _build_menus_into_qmenu(self, pm): #vers 1
        fm=pm.addMenu("File")
        fm.addAction("Load IMG…",    self._open_file)
        fm.addAction("Save IMG…",    self._save_file)
        fm.addSeparator()
        fm.addAction("Export Sheet…",self._export_sheet)
        fm.addAction("Import Sheet…",self._import_sheet)
        vm=pm.addMenu("View")
        vm.addAction("Zoom In",  lambda: self._zoom(1.25))
        vm.addAction("Zoom Out", lambda: self._zoom(0.8))
        vm.addAction("Fit Grid", self._fit)

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
        #if ICONS_AVAILABLE:
        #    self.setWindowIcon(SVGIconFactory.ai_app_icon())
        self.resize(1400, 800)
        self.setMinimumSize(800, 500)

        # Radar state
        self._img_reader:   Optional[ImgReader] = None
        self._img_path:     str = ""
        self._game_preset:  dict = GAME_PRESETS["SA"]
        self._current_idx:  int  = -1
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
        self._apply_preset("SA")

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

        # - Settings button
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip(App_name + " Workshop Settings")
        self.settings_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # - Game selector
        layout.addSpacing(8)
        layout.addWidget(QLabel("Game:"))
        self._game_combo = QComboBox()
        self._game_combo.addItems(list(GAME_PRESETS))
        self._game_combo.setCurrentText("SA")
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
    def _create_centre_panel(self): #vers 5
        """Tile list — [thumb 32px] filename, 1 column."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        vl = QVBoxLayout(panel)
        vl.setContentsMargins(*self.get_panel_margins())
        vl.setSpacing(self.panelspacing)

        # Header row: label + file buttons
        hdr_row = QHBoxLayout()
        hdr = QLabel("Tiles")
        hdr.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        hdr_row.addWidget(hdr)
        hdr_row.addStretch()

        icon_color = self._get_icon_color()

        self.open_btn = QPushButton()
        self.open_btn.setIcon(SVGIconFactory.open_icon(20, icon_color))
        self.open_btn.setIconSize(QSize(20, 20))
        self.open_btn.setToolTip("Load radar IMG (Ctrl+O)")
        self.open_btn.clicked.connect(self._open_file)
        hdr_row.addWidget(self.open_btn)

        self.save_btn = QPushButton()
        self.save_btn.setIcon(SVGIconFactory.save_icon(20, icon_color))
        self.save_btn.setIconSize(QSize(20, 20))
        self.save_btn.setToolTip("Save IMG (Ctrl+S)")
        self.save_btn.setEnabled(False)
        self.save_btn.clicked.connect(self._save_file)
        hdr_row.addWidget(self.save_btn)

        self.export_btn = QPushButton()
        self.export_btn.setIcon(SVGIconFactory.export_icon(20, icon_color))
        self.export_btn.setIconSize(QSize(20, 20))
        self.export_btn.setToolTip("Export all tiles as PNG sheet")
        self.export_btn.clicked.connect(self._export_sheet)
        hdr_row.addWidget(self.export_btn)

        self.import_btn = QPushButton()
        self.import_btn.setIcon(SVGIconFactory.import_icon(20, icon_color))
        self.import_btn.setIconSize(QSize(20, 20))
        self.import_btn.setToolTip("Import PNG sheet")
        self.import_btn.clicked.connect(self._import_sheet)
        hdr_row.addWidget(self.import_btn)

        self.info_btn = QPushButton()
        self.info_btn.setIcon(SVGIconFactory.info_icon(20, icon_color))
        self.info_btn.setIconSize(QSize(20, 20))
        self.info_btn.setToolTip("Workshop Info")
        self.info_btn.clicked.connect(self._show_info)
        hdr_row.addWidget(self.info_btn)

        vl.addLayout(hdr_row)

        self._tile_list = QListWidget()
        self._tile_list.setIconSize(QSize(THUMB, THUMB))
        self._tile_list.setUniformItemSizes(True)
        self._tile_list.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self._tile_list.currentRowChanged.connect(self._on_list_row)
        vl.addWidget(self._tile_list, 1)

        self._dirty_lbl = QLabel("Modified: 0")
        self._dirty_lbl.setFont(self.infobar_font)
        vl.addWidget(self._dirty_lbl)

        return panel


    # --- Right panel: settings
    def _create_right_panel(self): #vers 6
        """Radar grid preview + nav bar."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        hl = QHBoxLayout(panel)
        hl.setContentsMargins(0,0,0,0)
        hl.setSpacing(0)

        self._radar = RadarGridWidget()
        self._radar.tile_clicked.connect(self._on_grid_click)
        sc = QScrollArea()
        sc.setWidget(self._radar)
        sc.setWidgetResizable(True)
        sc.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hl.addWidget(sc, 1)

        # Right nav bar (vertical icon strip)
        nav = QFrame()
        nav.setFrameStyle(QFrame.Shape.StyledPanel)
        nav.setMaximumWidth(44); nav.setMinimumWidth(44)
        nl = QVBoxLayout(nav)
        nl.setContentsMargins(2,4,2,4); nl.setSpacing(2)
        icon_color = self._get_icon_color()

        def _nb(tip, slot):
            b = QToolButton(); b.setFixedSize(36,36); b.setToolTip(tip); b.clicked.connect(slot); nl.addWidget(b); return b

        _nb("Zoom in",       lambda: self._zoom(1.25))
        _nb("Zoom out",      lambda: self._zoom(0.8))
        _nb("Fit grid",      self._fit)
        _nb("Jump to tile",  self._jump)
        nl.addStretch()

        # Game selector (mini)
        gl = QLabel("Game"); gl.setAlignment(Qt.AlignmentFlag.AlignCenter); gl.setStyleSheet("font-size:9px;"); nl.addWidget(gl)
        self._game_combo2 = QComboBox()
        self._game_combo2.addItems(list(GAME_PRESETS)); self._game_combo2.setCurrentText("SA")
        self._game_combo2.setMaximumWidth(40)
        self._game_combo2.currentTextChanged.connect(self._on_game_changed)
        nl.addWidget(self._game_combo2)

        hl.addWidget(nav)
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
    def _show_workshop_settings(self): #vers 3
        """Workshop-local settings dialog — Fonts, Display, Radar tabs."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                     QTabWidget, QWidget, QGroupBox, QFormLayout,
                                     QSpinBox, QComboBox, QLabel, QFontComboBox)
        from PyQt6.QtGui import QFont

        self.s = RAD_settings
        dlg = QDialog(self)
        dlg.setWindowTitle(f"{App_name} Settings")
        dlg.setMinimumWidth(520)
        dlg.setMinimumHeight(420)
        layout = QVBoxLayout(dlg)
        tabs = QTabWidget()

        # - TAB 1: Fonts
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

        # - TAB 2: Display
        disp_tab = QWidget()
        dl = QVBoxLayout(disp_tab)

        btn_grp = QGroupBox("Button Display Mode")
        bl = QVBoxLayout(btn_grp)
        mode_combo = QComboBox(); mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        mode_combo.setCurrentIndex({'both':0,'icons':1,'text':2}.get(self.button_display_mode, 0))
        bl.addWidget(mode_combo)
        dl.addWidget(btn_grp)

        thumb_grp = QGroupBox("Tile List Thumbnail Size")
        thl = QHBoxLayout(thumb_grp)
        thumb_spin = QSpinBox(); thumb_spin.setRange(16, 128); thumb_spin.setValue(THUMB); thumb_spin.setSuffix(" px")
        thl.addWidget(thumb_spin)
        dl.addWidget(thumb_grp)
        dl.addStretch()
        tabs.addTab(disp_tab, "Display")

        # - TAB 3: Radar
        radar_tab = QWidget()
        rl = QVBoxLayout(radar_tab)

        game_grp = QGroupBox("Default Game Preset")
        gl = QHBoxLayout(game_grp)
        game_combo_s = QComboBox(); game_combo_s.addItems(list(GAME_PRESETS))
        game_combo_s.setCurrentText(self._game_combo.currentText())
        gl.addWidget(game_combo_s)
        rl.addWidget(game_grp)
        rl.addStretch()
        tabs.addTab(radar_tab, "Menu")
        old_icon_sz = self.RAD_settings.get('tool_icon_size')
        old_cols    = self.RAD_settings.get('tool_columns')

        dlg = DP5SettingsDialog(self.RAD_settings, self)
        if dlg.exec():
            # Apply menu bar style changes live
            self._apply_menu_bar_style()
            # Also apply orientation change if it changed
            self.set_menu_orientation(self.RAD_settings.get('menu_style', 'topbar'))

            # Apply changed settings live
            show_left = self.RAD_settings.get('show_bitmap_list')
            if hasattr(self, '_left_panel'):
                self._left_panel.setVisible(show_left)
            if hasattr(self, '_status_bar'):
                self._status_bar.setVisible(self.RAD_settings.get('show_statusbar'))

            if self.dp5_canvas:
                self.dp5_canvas.show_grid = self.RAD_settings.get('show_pixel_grid')
                self.dp5_canvas.grid_color = QColor(self.RAD_settings.get('grid_color'))
                self.dp5_canvas.show_cell_grid = self.RAD_settings.get('show_cell_grid')
                self.dp5_canvas.update()
                if self.RAD_settings.get('zoom_to_fit_resize'):
                    self._fit_canvas_to_viewport()
            self._set_platform(self.RAD_settings.get('platform_mode'))

            show_mb = (self.RAD_settings.get('show_menubar') and
                       self.RAD_settings.get('menu_style') == 'topbar')
            c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
            if c:
                c.setMinimumHeight(0)
                c.setMaximumHeight(16777215 if show_mb else 0)
                c.setVisible(show_mb)

            # If icon size or column count changed, rebuild the right panel
            new_icon_sz = self.RAD_settings.get('tool_icon_size')
            new_cols    = self.RAD_settings.get('tool_columns')

        layout.addWidget(tabs)

        # - Buttons
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
            # Switch game preset if changed
            new_game = game_combo_s.currentText()
            if new_game != self._game_combo.currentText():
                self._on_game_changed(new_game)

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(apply_all)
        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: (apply_all(), dlg.accept()))
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dlg.reject)
        for b in [cancel_btn, apply_btn, ok_btn]: btn_row.addWidget(b)
        layout.addLayout(btn_row)

        if new_icon_sz != old_icon_sz or new_cols != old_cols:
            self._rebuild_right_panel()

        self._set_status("Settings saved.")
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

        # Get theme colours if available, otherwise use sensible defaults
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

        # Height controlled by container — NOT by stylesheet min/max-height
        # (stylesheet height properties override Qt layout and prevent the bar showing)
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

        # Size the container based on settings, not isVisible()
        # (isVisible() is False during __init__ even if widget will be shown)
        c = getattr(self, '_menu_bar_container', None)
        if c:
            show = (self.RAD_settings.get('show_menubar', False) and
                    self.RAD_settings.get('menu_style', 'dropdown') == 'topbar')
            if show:
                c.setMinimumHeight(0)
                c.setMaximumHeight(bar_h)
            # Don't touch height here if hiding — setup_ui and set_menu_orientation handle that



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


    def _show_dropdown_menu(self): #vers 1
        """Pop up the canvas menus as a single QMenu dropdown."""
        menu = QMenu(self)
        self._build_canvas_menus(menu)
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


    def _on_game_changed(self, game): #vers 1
        cust = (game == "Custom")
        for s in [self._cols_spin, self._rows_spin]: s.setEnabled(cust)
        for c in [self._game_combo, self._game_combo2]:
            if c.currentText() != game:
                c.blockSignals(True); c.setCurrentText(game); c.blockSignals(False)
        self._apply_preset(game)


    def _on_custom_changed(self): #vers 1
        if self._game_combo.currentText() == "Custom":
            c=self._cols_spin.value(); r=self._rows_spin.value()
            GAME_PRESETS["Custom"].update({"cols":c,"rows":r,"count":c*r})
            self._apply_preset("Custom")


    def _apply_preset(self, game): #vers 1
        self._game_preset=GAME_PRESETS[game]; cols=self._game_preset["cols"]; count=self._game_preset["count"]
        self._tile_rgba={}; self._dirty_tiles=set(); self._current_idx=-1
        names=[self._game_preset["name_fn"](i) for i in range(count)]
        self._radar.setup(cols,count,names)
        self._tile_list.clear(); self._list_items=[]
        for i in range(count):
            item=TileListItem(i,names[i]); self._tile_list.addItem(item); self._list_items.append(item)
        self._set_status(f"{self._game_preset['label']} — {count} tiles ({cols}×{self._game_preset['rows']})")


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




    # - Hotkeys (template pattern)
    def _setup_hotkeys(self): #vers 1
        self.hotkey_open  = QShortcut(QKeySequence.StandardKey.Open,  self); self.hotkey_open.activated.connect(self._open_file)
        self.hotkey_save  = QShortcut(QKeySequence.StandardKey.Save,  self); self.hotkey_save.activated.connect(self._save_file)
        self.hotkey_close = QShortcut(QKeySequence.StandardKey.Close, self); self.hotkey_close.activated.connect(self.close)
        self.hotkey_find  = QShortcut(QKeySequence.StandardKey.Find,  self)
        self.hotkey_help  = QShortcut(QKeySequence.StandardKey.HelpContents, self)
        if self.main_window and hasattr(self.main_window,'log_message'):
            self.main_window.log_message(f"{App_name} hotkeys ready")


    # _apply_theme defined below




   # - File ops

    def _open_file(self): #vers 1
        path,_=QFileDialog.getOpenFileName(self,"Open Radar IMG","","IMG Archives (*.img);;All Files (*)")
        if not path: return
        try: self._img_reader=ImgReader(path); self._img_path=path
        except Exception as e: QMessageBox.critical(self,"Load Error",str(e)); return
        entries=self._img_reader.find_radar_entries(self._game_preset["img_pattern"])
        if not entries:
            QMessageBox.warning(self,"No Tiles",f"No radar TXDs found in {Path(path).name}"); return
        entries.sort(key=lambda e:e["name"].lower()); self._tile_entries=entries
        self._autodetect(len(entries))
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
        self._set_status(f"Loaded {len(entries)} tiles from {Path(path).name}")

    def _autodetect(self, count): #vers 1
        for game,p in GAME_PRESETS.items():
            if game!="Custom" and p["count"]==count: self._on_game_changed(game); return
        cols=max(1,round(math.sqrt(count))); rows=(count+cols-1)//cols
        GAME_PRESETS["Custom"].update({"cols":cols,"rows":rows,"count":count})
        self._on_game_changed("Custom")
        self._cols_spin.setValue(cols); self._rows_spin.setValue(rows)

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
    def _on_list_row(self, row): #vers 1
        if row<0: return
        self._current_idx=row; self._radar.set_selected(row)
        name=(self._tile_entries[row]["name"] if row<len(self._tile_entries) else self._game_preset["name_fn"](row))
        self._set_status(f"Tile {row}  |  {name}  |  {TILE_W}×{TILE_H} DXT1"+("  [modified]" if row in self._dirty_tiles else ""))

    def _on_grid_click(self, idx): #vers 1
        self._tile_list.setCurrentRow(idx); self._on_list_row(idx)

    # - Export/Import sheet
    def _export_sheet(self): #vers 1
        if not self._tile_rgba: QMessageBox.information(self,"Nothing","Load an IMG first."); return
        path,_=QFileDialog.getSaveFileName(self,"Export Sheet","radar_sheet.png","PNG Images (*.png)")
        if not path: return
        try:
            from PIL import Image
            cols=self._game_preset["cols"]; count=len(self._tile_rgba); rows=(count+cols-1)//cols
            sheet=Image.new("RGBA",(cols*TILE_W,rows*TILE_H),(0,0,0,0))
            for idx,rgba in self._tile_rgba.items():
                if len(rgba)==TILE_W*TILE_H*4:
                    sheet.paste(Image.frombytes("RGBA",(TILE_W,TILE_H),rgba),((idx%cols)*TILE_W,(idx//cols)*TILE_H))
            sheet.save(path); self._set_status(f"Exported {count} tiles → {Path(path).name}")
        except Exception as e: QMessageBox.critical(self,"Export Error",str(e))

    def _import_sheet(self): #vers 1
        path,_=QFileDialog.getOpenFileName(self,"Import Sheet","","PNG Images (*.png);;All Images (*)")
        if not path: return
        try:
            from PIL import Image
            sheet=Image.open(path).convert("RGBA"); cols=self._game_preset["cols"]
            tw=sheet.width//cols; count=min(cols*(sheet.height//tw),self._game_preset["count"])
            for idx in range(count):
                col=idx%cols; row=idx//cols
                tile=sheet.crop((col*tw,row*tw,(col+1)*tw,(row+1)*tw))
                if tw!=TILE_W: tile=tile.resize((TILE_W,TILE_H))
                rgba=tile.tobytes(); self._tile_rgba[idx]=rgba; self._dirty_tiles.add(idx)
                self._radar.set_tile(idx,rgba,TILE_W,TILE_H); self._radar.set_dirty(idx,True)
                if idx<len(self._list_items): self._list_items[idx].set_thumb(rgba,TILE_W,TILE_H)
            self._dirty_lbl.setText(f"Modified: {len(self._dirty_tiles)}")
            self._set_status(f"Imported {count} tiles from {Path(path).name}")
        except Exception as e: QMessageBox.critical(self,"Import Error",str(e))

    # - Grid nav
    def _zoom(self, f): #vers 1
        self._radar.resize(max(200,int(self._radar.width()*f)),max(200,int(self._radar.height()*f)))
    def _fit(self): #vers 1
        sc=self._radar.parentWidget()
        if sc: self._radar.resize(sc.width()-4,sc.height()-4)
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


    def paintEvent(self, event): #Vers 1
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            accent = QColor(colors.get('accent_primary', '#1976d2'))
        else:
            accent = QColor(100, 150, 255)
        accent.setAlpha(180)
        hover = QColor(accent); hover.setAlpha(255)
        w, h, size = self.width(), self.height(), self.corner_size
        corners = {
            'top-left':     [(0,0),(size,0),(0,size)],
            'top-right':    [(w,0),(w-size,0),(w,size)],
            'bottom-left':  [(0,h),(size,h),(0,h-size)],
            'bottom-right': [(w,h),(w-size,h),(w,h-size)],
        }
        for name, pts in corners.items():
            path = QPainterPath()
            path.moveTo(*pts[0]); path.lineTo(*pts[1]); path.lineTo(*pts[2])
            path.closeSubpath()
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(hover if self.hover_corner == name else accent))
            painter.drawPath(path)
        painter.end()

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)

    def _resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        if hasattr(self,'size_grip'): self.size_grip.move(self.width()-16,self.height()-16)

    def closeEvent(self, event): #Vers 1
        self.window_closed.emit()
        event.accept()

    #End of Class


def open_radar_workshop(main_window=None):
    try:
        w = RadarWorkshop(None, main_window); w.show(); return w
    except Exception as e:
        if main_window: QMessageBox.critical(main_window,App_name+" Error",str(e))
        return None

__all__=["RadarWorkshop","open_radar_workshop"]

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
