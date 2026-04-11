#!/usr/bin/env python3
#this belongs in apps/components/Radar_Editor/radar_workshop.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6 - Radar Workshop
# Radar tile editor — three-panel layout
# Centre: tile list [thumb] filename  |  Right: full radar grid preview
# Left vertical bar: edit tools  |  Right vertical bar: nav tools + game selector

import struct, sys, re, math, shutil
from pathlib import Path
from typing import Dict, List, Optional, Tuple

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QLabel, QPushButton, QFrame, QComboBox, QSpinBox, QScrollArea,
    QFileDialog, QMessageBox, QProgressDialog, QStatusBar,
    QListWidget, QListWidgetItem, QSizePolicy, QToolButton,
    QAbstractItemView
)
from PyQt6.QtCore import Qt, pyqtSignal, QSize, QPoint
from PyQt6.QtGui import QColor, QIcon, QImage, QPainter, QPen, QPixmap, QFont

App_name = "Radar Workshop"
Build    = "1"

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    ICONS_AVAILABLE = False
    class SVGIconFactory:
        @staticmethod
        def __getattr__(n): return lambda *a,**k: QIcon()

try:
    from apps.gui.tool_menu_mixin import ToolMenuMixin
except ImportError:
    class ToolMenuMixin:
        """Fallback no-op mixin when tool_menu_mixin is unavailable."""
        def get_menu_title(self): return "Radar Workshop"
        def _build_menus_into_qmenu(self, parent_menu): pass
        def _get_tool_menu_style(self): return "dropdown"

# ── Game presets ──────────────────────────────────────────────────────────────
def _radar_name_sa(idx):  return f"RADAR{idx:02d}"
def _radar_name_sol(idx): return f"radar{idx:04d}"

GAME_PRESETS = {
    "SA":     {"cols":8,  "rows":8,  "count":64,   "name_fn":_radar_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA San Andreas"},
    "VC":     {"cols":8,  "rows":8,  "count":64,   "name_fn":_radar_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA Vice City"},
    "VCS":    {"cols":8,  "rows":8,  "count":64,   "name_fn":_radar_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA Vice City Stories"},
    "LC":     {"cols":8,  "rows":8,  "count":64,   "name_fn":_radar_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA Liberty City (III)"},
    "LCS":    {"cols":8,  "rows":8,  "count":64,   "name_fn":_radar_name_sa,  "img_pattern":r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$", "label":"GTA Liberty City Stories"},
    "SOL":    {"cols":36, "rows":36, "count":1296,  "name_fn":_radar_name_sol, "img_pattern":r"^radar\d{4}\.txd$",                     "label":"GTA State of Liberty"},
    "Custom": {"cols":8,  "rows":8,  "count":64,   "name_fn":_radar_name_sa,  "img_pattern":r"^radar",                                  "label":"Custom Grid"},
}
TILE_W = TILE_H = 128

# ── DXT1 codec ────────────────────────────────────────────────────────────────
def decode_dxt1(data, width, height):
    out = bytearray(width * height * 4)
    bx  = (width+3)//4; by = (height+3)//4; pos = 0
    for block_y in range(by):
        for block_x in range(bx):
            if pos+8 > len(data): break
            c0 = struct.unpack_from("<H",data,pos)[0]; c1 = struct.unpack_from("<H",data,pos+2)[0]
            lut = struct.unpack_from("<I",data,pos+4)[0]; pos += 8
            def u565(v): return ((v>>11)&0x1F)*255//31, ((v>>5)&0x3F)*255//63, (v&0x1F)*255//31
            r0,g0,b0=u565(c0); r1,g1,b1=u565(c1)
            if c0>c1: pal=[(r0,g0,b0,255),(r1,g1,b1,255),((2*r0+r1)//3,(2*g0+g1)//3,(2*b0+b1)//3,255),((r0+2*r1)//3,(g0+2*g1)//3,(b0+2*b1)//3,255)]
            else:     pal=[(r0,g0,b0,255),(r1,g1,b1,255),((r0+r1)//2,(g0+g1)//2,(b0+b1)//2,255),(0,0,0,0)]
            for py in range(4):
                for px in range(4):
                    ix=block_x*4+px; iy=block_y*4+py
                    if ix<width and iy<height:
                        ci=(lut>>((py*4+px)*2))&3; o=(iy*width+ix)*4; out[o:o+4]=pal[ci]
    return bytes(out)

def encode_dxt1(rgba, width, height):
    from PIL import Image
    img=Image.frombytes("RGBA",(width,height),rgba).convert("RGB"); bx=(width+3)//4; by=(height+3)//4; out=bytearray(); px=img.load()
    def p565(r,g,b): return ((r>>3)<<11)|((g>>2)<<5)|(b>>3)
    for by2 in range(by):
        for bx2 in range(bx):
            pix=[px[bx2*4+pxx,by2*4+py] if bx2*4+pxx<width and by2*4+py<height else (0,0,0) for py in range(4) for pxx in range(4)]
            c0=(max(p[0] for p in pix),max(p[1] for p in pix),max(p[2] for p in pix))
            c1=(min(p[0] for p in pix),min(p[1] for p in pix),min(p[2] for p in pix))
            v0=p565(*c0); v1=p565(*c1)
            if v0<v1: v0,v1=v1,v0; c0,c1=c1,c0
            pal=[c0,c1,tuple((2*a+b)//3 for a,b in zip(c0,c1)),tuple((a+2*b)//3 for a,b in zip(c0,c1))]
            lut=0
            for i,p in enumerate(pix):
                best=min(range(4),key=lambda k:sum((p[j]-pal[k][j])**2 for j in range(3))); lut|=best<<(i*2)
            out+=struct.pack("<HHI",v0,v1,lut)
    return bytes(out)

# ── TXD reader/writer ──────────────────────────────────────────────────────────
class RadarTxdReader:
    @staticmethod
    def read(data):
        pos=12
        while pos+12<=len(data):
            st=struct.unpack_from("<I",data,pos)[0]; ss=struct.unpack_from("<I",data,pos+4)[0]
            if st==0x15:
                th=pos+24
                tex_name=data[th+8:th+40].rstrip(b"\x00").decode("latin1","replace")
                w=struct.unpack_from("<H",data,th+80)[0]; h=struct.unpack_from("<H",data,th+82)[0]
                comp=struct.unpack_from("<B",data,th+87)[0]; dsz=struct.unpack_from("<I",data,th+88)[0]
                pd=data[th+92:th+92+dsz]
                rgba=decode_dxt1(pd,w,h) if comp==1 else bytes(pd[:w*h*4])
                return rgba,w,h,tex_name
            pos+=12+ss
            if ss==0: break
        raise ValueError("No Texture Native section")

    @staticmethod
    def write(rgba,width,height,tex_name,rw_ver=0x1003FFFF):
        dxt=encode_dxt1(rgba,width,height)
        nb=tex_name.encode("latin1")[:31].ljust(32,b"\x00"); ab=b"\x00"*32
        native=bytearray()
        native+=struct.pack("<I",8)+struct.pack("<I",0)+nb+ab+struct.pack("<I",0x200)+b"DXT1"
        native+=struct.pack("<HH",width,height)+struct.pack("<BBBB",16,1,4,1)+struct.pack("<I",len(dxt))+dxt
        def rws(t,b): return struct.pack("<III",t,len(b),rw_ver)+b
        cb=rws(0x01,struct.pack("<HH",1,0))+rws(0x15,rws(0x01,bytes(native))+rws(0x03,b""))+rws(0x03,b"")
        c=rws(0x16,cb)
        return c+b"\x00"*((-len(c))%2048)

# ── IMG reader ────────────────────────────────────────────────────────────────
class ImgReader:
    def __init__(self,img_path):
        self.img_path=img_path; self.entries=[]; self._img_data=b""; self._load()
    def _load(self):
        p=Path(self.img_path); raw=p.read_bytes()
        if raw[:4]==b"VER2":
            n=struct.unpack_from("<I",raw,4)[0]
            for i in range(n):
                os2,sz2,nb=struct.unpack_from("<II32s",raw,8+i*40)
                self.entries.append({"name":nb.rstrip(b"\x00").decode("latin1","replace"),"offset":os2*2048,"size":sz2*2048})
            self._img_data=raw
        else:
            dp=p.with_suffix(".dir")
            if not dp.exists(): raise FileNotFoundError(f"No .dir for {p.name}")
            dr=dp.read_bytes(); n=len(dr)//32
            for i in range(n):
                os2,sz2,nb=struct.unpack_from("<II24s",dr,i*32)
                self.entries.append({"name":nb.rstrip(b"\x00").decode("latin1","replace"),"offset":os2*2048,"size":sz2*2048})
            self._img_data=raw
    def get_entry_data(self,e): return self._img_data[e["offset"]:e["offset"]+e["size"]]
    def find_radar_entries(self,pattern):
        pat=re.compile(pattern,re.IGNORECASE); return [e for e in self.entries if pat.match(e["name"])]

# ── Radar grid widget ─────────────────────────────────────────────────────────
class RadarGridWidget(QWidget):
    tile_clicked = pyqtSignal(int)
    def __init__(self,parent=None):
        super().__init__(parent)
        self._cols=8; self._count=0; self._tiles={}; self._dirty=set()
        self._selected=-1; self._hover=-1; self._names=[]
        self.setMouseTracking(True)
        self.setSizePolicy(QSizePolicy.Policy.Expanding,QSizePolicy.Policy.Expanding)
    def setup(self,cols,count,names=None):
        self._cols=max(1,cols); self._count=count; self._tiles={}; self._dirty=set()
        self._selected=-1; self._hover=-1
        self._names=names or [f"Tile {i}" for i in range(count)]; self.update()
    def _ts(self):
        if not self._count or not self._cols: return 32
        rows=(self._count+self._cols-1)//self._cols
        return min(max(4,self.width()//self._cols),max(4,self.height()//rows))
    def _idx_at(self,pos):
        ts=self._ts(); idx=(pos.y()//ts)*self._cols+(pos.x()//ts)
        return idx if 0<=idx<self._count else -1
    def set_tile(self,idx,rgba,w,h):
        self._tiles[idx]=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888).copy(); self.update()
    def set_dirty(self,idx,d):
        if d: self._dirty.add(idx)
        else: self._dirty.discard(idx)
        self.update()
    def set_selected(self,idx): self._selected=idx; self.update()
    def paintEvent(self,ev):
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
            if idx==self._selected:
                p.setPen(QPen(QColor(80,180,255),2)); p.drawRect(x+1,y+1,ts-2,ts-2)
        p.setPen(QPen(QColor(60,60,60),1))
        rows=(self._count+cols-1)//cols
        for c in range(cols+1): p.drawLine(c*ts,0,c*ts,rows*ts)
        for r in range(rows+1): p.drawLine(0,r*ts,cols*ts,r*ts)
        p.end()
    def mouseMoveEvent(self,ev):
        idx=self._idx_at(ev.pos())
        if idx!=self._hover:
            self._hover=idx
            self.setToolTip(f"[{idx}] {self._names[idx]}" if 0<=idx<len(self._names) else "")
            self.update()
    def leaveEvent(self,ev): self._hover=-1; self.update()
    def mousePressEvent(self,ev):
        if ev.button()==Qt.MouseButton.LeftButton:
            idx=self._idx_at(ev.pos())
            if idx>=0: self._selected=idx; self.tile_clicked.emit(idx); self.update()

THUMB=32

class TileListItem(QListWidgetItem):
    def __init__(self,idx,name):
        super().__init__(); self.idx=idx; self.name=name
        self.setText(f"  {name}"); self.setSizeHint(QSize(0,THUMB+4))
    def set_thumb(self,rgba,w,h):
        img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
        self.setIcon(QIcon(QPixmap.fromImage(img).scaled(THUMB,THUMB,Qt.AspectRatioMode.KeepAspectRatio,Qt.TransformationMode.SmoothTransformation)))

# ── RadarWorkshop ─────────────────────────────────────────────────────────────
class RadarWorkshop(ToolMenuMixin, QWidget):
    workshop_closed = pyqtSignal()

    def get_menu_title(self): return "Radar Workshop"  # vers 1
    def _build_menus_into_qmenu(self,pm):  # vers 1
        fm=pm.addMenu("File"); fm.addAction("Load IMG…",self._load_img); fm.addAction("Save IMG…",self._save_img)
        fm.addSeparator(); fm.addAction("Export Sheet…",self._export_sheet); fm.addAction("Import Sheet…",self._import_sheet)
        vm=pm.addMenu("View"); vm.addAction("Zoom In",lambda:self._zoom(1.25)); vm.addAction("Zoom Out",lambda:self._zoom(0.8)); vm.addAction("Fit",self._fit)

    def __init__(self,parent=None,main_window=None):
        super().__init__(parent)
        self.main_window=main_window; self.standalone_mode=(main_window is None)
        self.setWindowTitle(f"{App_name}  [Build {Build}]")
        if self.standalone_mode: self.setWindowFlags(Qt.WindowType.Window); self.resize(1400,860)
        else: self.setWindowFlags(Qt.WindowType.Widget)
        self._img_reader=None; self._img_path=""; self._game_preset=GAME_PRESETS["SA"]
        self._current_idx=-1; self._tile_rgba={}; self._tile_entries=[]; self._dirty_tiles=set(); self._list_items=[]
        self._build_ui(); self._apply_preset("SA")

    def _build_ui(self):
        root=QVBoxLayout(self); root.setContentsMargins(0,0,0,0); root.setSpacing(0)
        tb=self._build_toolbar(); tb.setVisible(self.standalone_mode); root.addWidget(tb)
        spl=QSplitter(Qt.Orientation.Horizontal); spl.setChildrenCollapsible(True)
        lb=self._build_left_bar(); lb.setMaximumWidth(44); lb.setMinimumWidth(44); spl.addWidget(lb)
        spl.addWidget(self._build_list_panel())
        spl.addWidget(self._build_radar_panel())
        spl.setSizes([44,220,1100]); spl.setStretchFactor(0,0); spl.setStretchFactor(1,0); spl.setStretchFactor(2,1)
        root.addWidget(spl,1)
        self._status=QStatusBar(); self._status.setMaximumHeight(22); root.addWidget(self._status)
        self._set_status("No IMG loaded")

    def _build_toolbar(self):
        bar=QWidget(); bar.setFixedHeight(34); hl=QHBoxLayout(bar); hl.setContentsMargins(4,2,4,2); hl.setSpacing(4)
        def b(t,tip,s): btn=QPushButton(t); btn.setToolTip(tip); btn.clicked.connect(s); btn.setFixedHeight(28); hl.addWidget(btn); return btn
        b("Load IMG…","Load radar IMG",self._load_img)
        self._save_btn=b("Save IMG…","Save modified tiles",self._save_img); self._save_btn.setEnabled(False)
        hl.addSpacing(8)
        b("Export Sheet…","Export all tiles as PNG sheet",self._export_sheet)
        b("Import Sheet…","Import PNG sheet",self._import_sheet)
        hl.addStretch()
        hl.addWidget(QLabel("Game:"))
        self._game_combo=QComboBox(); self._game_combo.addItems(list(GAME_PRESETS)); self._game_combo.setCurrentText("SA")
        self._game_combo.currentTextChanged.connect(self._on_game_changed); hl.addWidget(self._game_combo)
        self._cols_spin=QSpinBox(); self._cols_spin.setRange(1,100); self._cols_spin.setValue(8); self._cols_spin.setPrefix("W "); self._cols_spin.setEnabled(False)
        self._cols_spin.valueChanged.connect(self._on_custom_changed); hl.addWidget(self._cols_spin)
        self._rows_spin=QSpinBox(); self._rows_spin.setRange(1,100); self._rows_spin.setValue(8); self._rows_spin.setPrefix("H "); self._rows_spin.setEnabled(False)
        self._rows_spin.valueChanged.connect(self._on_custom_changed); hl.addWidget(self._rows_spin)
        return bar

    def _build_left_bar(self):
        bar=QFrame(); bar.setFrameStyle(QFrame.Shape.StyledPanel)
        vl=QVBoxLayout(bar); vl.setContentsMargins(2,4,2,4); vl.setSpacing(2)
        def nb(tip,s,en=True):
            bt=QToolButton(); bt.setFixedSize(36,36); bt.setToolTip(tip); bt.clicked.connect(s); bt.setEnabled(en); vl.addWidget(bt); return bt
        self._push_btn=nb("Push edits to tile",self._push_tile,False)
        self._revert_btn=nb("Revert tile",self._revert_tile,False)
        vl.addStretch()
        return bar

    def _build_list_panel(self):
        p=QWidget(); vl=QVBoxLayout(p); vl.setContentsMargins(0,0,0,0); vl.setSpacing(0)
        hdr=QLabel("  Tiles"); hdr.setFixedHeight(22); hdr.setStyleSheet("font-weight:bold; padding-left:4px;"); vl.addWidget(hdr)
        self._tile_list=QListWidget()
        self._tile_list.setIconSize(QSize(THUMB,THUMB)); self._tile_list.setUniformItemSizes(True)
        self._tile_list.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self._tile_list.currentRowChanged.connect(self._on_list_row); vl.addWidget(self._tile_list,1)
        self._dirty_lbl=QLabel("  Modified: 0"); self._dirty_lbl.setFixedHeight(20)
        self._dirty_lbl.setStyleSheet("color:#aaa; font-size:10px;"); vl.addWidget(self._dirty_lbl)
        return p

    def _build_radar_panel(self):
        p=QWidget(); hl=QHBoxLayout(p); hl.setContentsMargins(0,0,0,0); hl.setSpacing(0)
        self._radar=RadarGridWidget(); self._radar.tile_clicked.connect(self._on_grid_click)
        sc=QScrollArea(); sc.setWidget(self._radar); sc.setWidgetResizable(True)
        sc.setAlignment(Qt.AlignmentFlag.AlignCenter); hl.addWidget(sc,1)
        hl.addWidget(self._build_nav_bar())
        return p

    def _build_nav_bar(self):
        bar=QFrame(); bar.setFrameStyle(QFrame.Shape.StyledPanel); bar.setMaximumWidth(44); bar.setMinimumWidth(44)
        vl=QVBoxLayout(bar); vl.setContentsMargins(2,4,2,4); vl.setSpacing(2)
        def nb(tip,s):
            bt=QToolButton(); bt.setFixedSize(36,36); bt.setToolTip(tip); bt.clicked.connect(s); vl.addWidget(bt); return bt
        nb("Zoom in",lambda:self._zoom(1.25)); nb("Zoom out",lambda:self._zoom(0.8)); nb("Fit",self._fit); nb("Jump to selected",self._jump)
        vl.addStretch()
        gl=QLabel("Game"); gl.setAlignment(Qt.AlignmentFlag.AlignCenter); gl.setStyleSheet("font-size:9px;"); vl.addWidget(gl)
        self._game_combo2=QComboBox(); self._game_combo2.addItems(list(GAME_PRESETS)); self._game_combo2.setCurrentText("SA")
        self._game_combo2.setMaximumWidth(40); self._game_combo2.currentTextChanged.connect(self._on_game_changed); vl.addWidget(self._game_combo2)
        return bar

    # ── Preset/game ────────────────────────────────────────────────────────────
    def _on_game_changed(self,game):
        cust=(game=="Custom")
        if hasattr(self,"_cols_spin"): self._cols_spin.setEnabled(cust); self._rows_spin.setEnabled(cust)
        for c in [self._game_combo, self._game_combo2]:
            if c.currentText()!=game: c.blockSignals(True); c.setCurrentText(game); c.blockSignals(False)
        self._apply_preset(game)

    def _on_custom_changed(self):
        if self._game_combo.currentText()=="Custom":
            cols=self._cols_spin.value(); rows=self._rows_spin.value()
            GAME_PRESETS["Custom"].update({"cols":cols,"rows":rows,"count":cols*rows}); self._apply_preset("Custom")

    def _apply_preset(self,game):
        self._game_preset=GAME_PRESETS[game]; cols=self._game_preset["cols"]; count=self._game_preset["count"]
        self._tile_rgba={}; self._dirty_tiles=set(); self._current_idx=-1
        names=[self._game_preset["name_fn"](i) for i in range(count)]
        self._radar.setup(cols,count,names)
        self._tile_list.clear(); self._list_items=[]
        for i in range(count):
            item=TileListItem(i,names[i]); self._tile_list.addItem(item); self._list_items.append(item)
        self._push_btn.setEnabled(False); self._revert_btn.setEnabled(False)
        self._set_status(f"{self._game_preset['label']} — {count} tiles ({cols}×{self._game_preset['rows']})")

    # ── Load ───────────────────────────────────────────────────────────────────
    def _load_img(self):
        path,_=QFileDialog.getOpenFileName(self,"Open Radar IMG","","IMG Archives (*.img);;All Files (*)")
        if not path: return
        try: self._img_reader=ImgReader(path); self._img_path=path
        except Exception as e: QMessageBox.critical(self,"Load Error",str(e)); return
        entries=self._img_reader.find_radar_entries(self._game_preset["img_pattern"])
        if not entries:
            QMessageBox.warning(self,"No Tiles",f"No radar TXDs found in {Path(path).name}"); return
        entries.sort(key=lambda e:e["name"].lower()); self._tile_entries=entries
        self._autodetect(len(entries))
        prog=QProgressDialog("Loading…","Cancel",0,len(entries),self); prog.setWindowModality(Qt.WindowModality.WindowModal); prog.show()
        for i,entry in enumerate(entries):
            prog.setValue(i); QApplication.processEvents()
            if prog.wasCanceled(): break
            try:
                rgba,w,h,_=RadarTxdReader.read(self._img_reader.get_entry_data(entry))
                self._tile_rgba[i]=rgba; self._radar.set_tile(i,rgba,w,h)
                if i<len(self._list_items): self._list_items[i].set_thumb(rgba,w,h)
            except Exception as e: print(f"WARN tile {i}: {e}",file=sys.stderr)
        prog.setValue(len(entries)); self._dirty_tiles=set()
        if hasattr(self,"_save_btn"): self._save_btn.setEnabled(True)
        self._set_status(f"Loaded {len(entries)} tiles from {Path(path).name}")

    def _autodetect(self,count):
        for game,p in GAME_PRESETS.items():
            if game!="Custom" and p["count"]==count: self._on_game_changed(game); return
        cols=max(1,round(math.sqrt(count))); rows=(count+cols-1)//cols
        GAME_PRESETS["Custom"].update({"cols":cols,"rows":rows,"count":count})
        self._on_game_changed("Custom")
        if hasattr(self,"_cols_spin"): self._cols_spin.setValue(cols); self._rows_spin.setValue(rows)

    # ── Selection ──────────────────────────────────────────────────────────────
    def _on_list_row(self,row):
        if row<0: return
        self._current_idx=row; self._radar.set_selected(row)
        self._push_btn.setEnabled(True); self._revert_btn.setEnabled(row<len(self._tile_entries))
        name=(self._tile_entries[row]["name"] if row<len(self._tile_entries) else self._game_preset["name_fn"](row))
        self._set_status(f"Tile {row}  |  {name}  |  {TILE_W}x{TILE_H} DXT1"+("  [modified]" if row in self._dirty_tiles else ""))

    def _on_grid_click(self,idx):
        self._tile_list.setCurrentRow(idx); self._on_list_row(idx)

    # ── Tile ops ───────────────────────────────────────────────────────────────
    def _push_tile(self):
        QMessageBox.information(self,"Push Tile","Edit the tile externally, then use Import Sheet to update.")

    def _revert_tile(self):
        idx=self._current_idx
        if idx<0 or idx>=len(self._tile_entries): return
        try:
            rgba,w,h,_=RadarTxdReader.read(self._img_reader.get_entry_data(self._tile_entries[idx]))
            self._tile_rgba[idx]=rgba; self._dirty_tiles.discard(idx)
            self._radar.set_tile(idx,rgba,w,h); self._radar.set_dirty(idx,False)
            if idx<len(self._list_items): self._list_items[idx].set_thumb(rgba,w,h)
            self._dirty_lbl.setText(f"  Modified: {len(self._dirty_tiles)}")
        except Exception as e: QMessageBox.warning(self,"Revert Error",str(e))

    # ── Save ───────────────────────────────────────────────────────────────────
    def _save_img(self):
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
                off=e["offset"]; sz=e["size"]; padded=new+b"\x00"*max(0,sz-len(new)); data[off:off+sz]=padded[:sz]
            Path(path).write_bytes(bytes(data))
            sd=Path(self._img_path).with_suffix(".dir"); dd=Path(path).with_suffix(".dir")
            if sd.exists() and path!=self._img_path: shutil.copy2(sd,dd)
            for i in range(len(self._tile_entries)): self._radar.set_dirty(i,False)
            self._dirty_tiles=set(); self._dirty_lbl.setText("  Modified: 0")
            self._set_status(f"Saved to {Path(path).name}")
        except Exception as e: QMessageBox.critical(self,"Save Error",str(e))

    # ── Export/Import ──────────────────────────────────────────────────────────
    def _export_sheet(self):
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
            sheet.save(path); self._set_status(f"Exported {count} tiles to {Path(path).name}")
        except Exception as e: QMessageBox.critical(self,"Export Error",str(e))

    def _import_sheet(self):
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
            self._dirty_lbl.setText(f"  Modified: {len(self._dirty_tiles)}")
            self._set_status(f"Imported {count} tiles from {Path(path).name}")
        except Exception as e: QMessageBox.critical(self,"Import Error",str(e))

    # ── Grid nav ───────────────────────────────────────────────────────────────
    def _zoom(self,f):
        self._radar.resize(max(200,int(self._radar.width()*f)),max(200,int(self._radar.height()*f)))
    def _fit(self):
        sc=self._radar.parentWidget()
        if sc: self._radar.resize(sc.width()-4,sc.height()-4)
    def _jump(self):
        if self._current_idx>=0:
            self._tile_list.scrollToItem(self._tile_list.item(self._current_idx),QAbstractItemView.ScrollHint.PositionAtCenter)

    def _set_status(self,msg): self._status.showMessage(msg)

    def closeEvent(self,ev):
        if self._dirty_tiles:
            r=QMessageBox.question(self,"Unsaved Changes",f"{len(self._dirty_tiles)} tile(s) unsaved. Close anyway?",
                                   QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No)
            if r!=QMessageBox.StandardButton.Yes: ev.ignore(); return
        self.workshop_closed.emit(); ev.accept()


def open_radar_workshop(main_window=None):
    try:
        w=RadarWorkshop(None,main_window); w.show(); return w
    except Exception as e:
        if main_window: QMessageBox.critical(main_window,App_name+" Error",str(e))
        return None

__all__=["RadarWorkshop","open_radar_workshop"]

if __name__=="__main__":
    import traceback
    app=QApplication(sys.argv); app.setApplicationName(App_name)
    try:
        w=RadarWorkshop(); w.show(); sys.exit(app.exec())
    except Exception as e:
        traceback.print_exc(); sys.exit(1)
