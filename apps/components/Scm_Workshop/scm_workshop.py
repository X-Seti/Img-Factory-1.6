# apps/components/Scm_Workshop/scm_workshop.py — Version 1
# X-Seti / Claudia — IMG Factory 1.6 — SCM Script Workshop
# GTA III / VC / SA main.scm browser, coord searcher and patcher.

import os, struct
from typing import List, Optional
from pathlib import Path

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QFrame, QLabel,
    QPushButton, QLineEdit, QTableWidget, QTableWidgetItem,
    QAbstractItemView, QTextEdit, QDoubleSpinBox, QGroupBox,
    QProgressBar, QCheckBox, QFileDialog, QMessageBox,
    QTabWidget, QDialog, QDialogButtonBox, QFormLayout
)
from PyQt6.QtCore import Qt, QSize, QThread, pyqtSignal
from PyQt6.QtGui import QFont, QIcon

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
except ImportError:
    class SVGIconFactory:
        @staticmethod
        def open_icon(sz=20, c=None): return QIcon()
        @staticmethod
        def save_icon(sz=20, c=None): return QIcon()
        @staticmethod
        def export_icon(sz=20, c=None): return QIcon()
        @staticmethod
        def get_terminal_icon(*a, **kw): return QIcon()


class SCMCoordHit:
    __slots__ = ('offset','x','y','z','context','patched')
    def __init__(self, offset, x, y, z, context=''):
        self.offset=offset; self.x=x; self.y=y; self.z=z
        self.context=context; self.patched=False


class SCMParser:
    WORLD_X=(-4000.0,4000.0); WORLD_Y=(-4000.0,4000.0); WORLD_Z=(-200.0,1000.0)

    def __init__(self):
        self.data=b''; self.hits:List[SCMCoordHit]=[]; self.size=0; self.game='unknown'

    def load(self, path:str)->bool:
        with open(path,'rb') as f: self.data=f.read()
        self.size=len(self.data)
        if self.size>4:
            m=self.data[:4]
            self.game='sa' if m==b'\x03\x00\x00\x00' else 'gta3/vc' if m==b'\x02\x00\x00\x00' else 'unknown'
        return True

    def save(self,path:str):
        with open(path,'wb') as f: f.write(self.data)

    def _valid(self,x,y,z)->bool:
        import math
        return (math.isfinite(x) and math.isfinite(y) and math.isfinite(z) and
                self.WORLD_X[0]<=x<=self.WORLD_X[1] and
                self.WORLD_Y[0]<=y<=self.WORLD_Y[1] and
                self.WORLD_Z[0]<=z<=self.WORLD_Z[1])

    def search_coords(self, x_min=None,x_max=None,y_min=None,y_max=None,
                      tolerance=0.0,target_x=None,target_y=None,target_z=None,
                      progress_cb=None)->List[SCMCoordHit]:
        hits=[]; data=self.data; n=len(data)-11
        for i in range(0,n,1):
            try: x,y,z=struct.unpack_from('<fff',data,i)
            except struct.error: continue
            if not self._valid(x,y,z): continue
            if x_min is not None and x<x_min: continue
            if x_max is not None and x>x_max: continue
            if y_min is not None and y<y_min: continue
            if y_max is not None and y>y_max: continue
            if target_x is not None and abs(x-target_x)>tolerance: continue
            if target_y is not None and abs(y-target_y)>tolerance: continue
            if target_z is not None and abs(z-target_z)>tolerance: continue
            ctx=data[max(0,i-4):i].hex(' ')
            hits.append(SCMCoordHit(i,x,y,z,ctx))
            if progress_cb and len(hits)%200==0: progress_cb(i,n)
        self.hits=hits; return hits

    def patch_all_in_region(self,x1,x2,y1,y2,dx,dy,dz)->int:
        data=bytearray(self.data); count=0
        for h in self.hits:
            if not (x1<=h.x<=x2 and y1<=h.y<=y2): continue
            struct.pack_into('<fff',data,h.offset,h.x+dx,h.y+dy,h.z+dz)
            h.x+=dx; h.y+=dy; h.z+=dz; h.patched=True; count+=1
        self.data=bytes(data); return count

    def hex_dump(self,offset:int,length:int=512)->str:
        start=max(0,offset-8); end=min(len(self.data),start+length)
        lines=[]
        for row in range(start,end,16):
            chunk=self.data[row:row+16]
            hp=' '.join(f'{b:02x}' for b in chunk)
            ap=''.join(chr(b) if 32<=b<127 else '.' for b in chunk)
            m='►' if row<=offset<row+16 else ' '
            lines.append(f"{m} {row:08X}  {hp:<48}  {ap}")
        return '\n'.join(lines)


class SCMSearchThread(QThread):
    progress=pyqtSignal(int,int); finished_s=pyqtSignal(list)
    def __init__(self,parser,kwargs): super().__init__(); self._p=parser; self._k=kwargs
    def run(self):
        hits=self._p.search_coords(progress_cb=lambda i,n:self.progress.emit(i,n),**self._k)
        self.finished_s.emit(hits)


class SCMWorkshop(QWidget):
    App_name="SCM Workshop"

    def __init__(self,parent=None,main_window=None):
        super().__init__(parent)
        self.main_window=main_window; self._parser=SCMParser()
        self._scm_path=''; self._search_thread=None
        self._build_ui()

    def _ic(self):
        try:
            c=self.palette().color(self.palette().ColorRole.WindowText)
            return f"#{c.red():02x}{c.green():02x}{c.blue():02x}"
        except: return '#cccccc'

    def _fspin(self,lo,hi,val,tip):
        s=QDoubleSpinBox(); s.setRange(lo,hi); s.setValue(val)
        s.setDecimals(3); s.setFixedWidth(90); s.setFixedHeight(24); s.setToolTip(tip); return s

    def _build_ui(self):
        root=QVBoxLayout(self); root.setContentsMargins(4,4,4,4); root.setSpacing(4)

        # Toolbar
        bar=QHBoxLayout(); bar.setSpacing(6); ic=self._ic()
        for icon_fn,tip,slot,attr in [
            ('open_icon',  'Open main.scm',  self._open_file,  None),
            ('save_icon',  'Save SCM',        self._save_file,  '_save_btn'),
            ('export_icon','Save As…',        self._save_as,    '_saveas_btn'),
        ]:
            b=QPushButton()
            try: b.setIcon(getattr(SVGIconFactory,icon_fn)(18,ic))
            except: pass
            b.setIconSize(QSize(18,18)); b.setFixedSize(28,28); b.setToolTip(tip)
            b.clicked.connect(slot)
            if attr: setattr(self,attr,b); b.setEnabled(False)
            bar.addWidget(b)
        bar.addSpacing(8)
        self._status_lbl=QLabel("No SCM loaded")
        self._status_lbl.setStyleSheet("color:palette(mid);")
        bar.addWidget(self._status_lbl,1)
        self._game_lbl=QLabel(""); self._game_lbl.setFont(QFont("Arial",9,QFont.Weight.Bold))
        bar.addWidget(self._game_lbl)
        root.addLayout(bar)

        self._progress=QProgressBar(); self._progress.setVisible(False); self._progress.setFixedHeight(10)
        root.addWidget(self._progress)

        self._tabs=QTabWidget(); root.addWidget(self._tabs,1)
        self._tabs.addTab(self._build_search_tab(),"🔍  Coord Search")
        self._tabs.addTab(self._build_patch_tab(), "🔧  Region Patch")
        self._tabs.addTab(self._build_hex_tab(),   "🔢  Hex View")
        self._tabs.addTab(self._build_info_tab(),  "ℹ  Info")

    def _build_search_tab(self):
        w=QFrame(); lay=QVBoxLayout(w); lay.setSpacing(6)
        flt=QGroupBox("Search filter (leave blank = all world coords)")
        fl=QFormLayout(flt); fl.setSpacing(4)
        self._sx_min=self._fspin(-4000,4000,-4000,"Min X"); fl.addRow("X min:",self._sx_min)
        self._sx_max=self._fspin(-4000,4000, 4000,"Max X"); fl.addRow("X max:",self._sx_max)
        self._sy_min=self._fspin(-4000,4000,-4000,"Min Y"); fl.addRow("Y min:",self._sy_min)
        self._sy_max=self._fspin(-4000,4000, 4000,"Max Y"); fl.addRow("Y max:",self._sy_max)
        tr=QHBoxLayout()
        self._tx=self._fspin(-4000,4000,0,"X"); tr.addWidget(QLabel("Target X:")); tr.addWidget(self._tx)
        self._ty=self._fspin(-4000,4000,0,"Y"); tr.addWidget(QLabel("Y:")); tr.addWidget(self._ty)
        self._tz=self._fspin(-200,1000,0,"Z"); tr.addWidget(QLabel("Z:")); tr.addWidget(self._tz)
        self._use_target=QCheckBox("Use target"); tr.addWidget(self._use_target)
        fl.addRow("",tr)
        self._tol=self._fspin(0,500,10,"Tolerance"); fl.addRow("Tolerance ±:",self._tol)
        lay.addWidget(flt)
        br=QHBoxLayout()
        self._search_btn=QPushButton("🔍  Search SCM"); self._search_btn.setFixedHeight(28)
        self._search_btn.setEnabled(False); self._search_btn.clicked.connect(self._run_search)
        br.addWidget(self._search_btn)
        self._result_lbl=QLabel(""); br.addWidget(self._result_lbl,1)
        lay.addLayout(br)
        self._result_table=QTableWidget(0,5)
        self._result_table.setHorizontalHeaderLabels(["Offset","X","Y","Z","Patched"])
        self._result_table.horizontalHeader().setStretchLastSection(True)
        self._result_table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self._result_table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self._result_table.setAlternatingRowColors(True)
        self._result_table.verticalHeader().setDefaultSectionSize(20)
        self._result_table.doubleClicked.connect(self._on_result_dclick)
        lay.addWidget(self._result_table,1)
        return w

    def _build_patch_tab(self):
        w=QFrame(); lay=QVBoxLayout(w); lay.setSpacing(8)
        lay.addWidget(QLabel("Translate all coords in a region by (dX,dY,dZ). Run Coord Search first."))
        rg=QGroupBox("Source region"); rl=QFormLayout(rg); rl.setSpacing(4)
        self._rx1=self._fspin(-4000,4000,-4000,"X min"); rl.addRow("X min:",self._rx1)
        self._rx2=self._fspin(-4000,4000, 4000,"X max"); rl.addRow("X max:",self._rx2)
        self._ry1=self._fspin(-4000,4000,-4000,"Y min"); rl.addRow("Y min:",self._ry1)
        self._ry2=self._fspin(-4000,4000, 4000,"Y max"); rl.addRow("Y max:",self._ry2)
        lay.addWidget(rg)
        dg=QGroupBox("Translation offset"); dl=QHBoxLayout(dg); dl.setSpacing(6)
        self._pdx=self._fspin(-9999,9999,0,"dX"); dl.addWidget(QLabel("dX:")); dl.addWidget(self._pdx)
        self._pdy=self._fspin(-9999,9999,0,"dY"); dl.addWidget(QLabel("dY:")); dl.addWidget(self._pdy)
        self._pdz=self._fspin(-9999,9999,0,"dZ"); dl.addWidget(QLabel("dZ:")); dl.addWidget(self._pdz)
        lay.addWidget(dg)
        pb=QPushButton("⚠  Apply Region Patch"); pb.setFixedHeight(32)
        pb.setStyleSheet("QPushButton{background:#7f1d1d;color:#fca5a5;font-weight:bold}QPushButton:hover{background:#991b1b}")
        pb.clicked.connect(self._apply_patch); lay.addWidget(pb)
        self._patch_log=QTextEdit(); self._patch_log.setReadOnly(True)
        self._patch_log.setFont(QFont("Courier New",9)); self._patch_log.setPlaceholderText("Patch log…")
        lay.addWidget(self._patch_log,1); return w

    def _build_hex_tab(self):
        w=QFrame(); lay=QVBoxLayout(w); lay.setSpacing(4)
        nav=QHBoxLayout(); nav.addWidget(QLabel("Offset (hex):"))
        self._hex_offset=QLineEdit("0"); self._hex_offset.setFixedWidth(100)
        self._hex_offset.returnPressed.connect(self._update_hex_view); nav.addWidget(self._hex_offset)
        gb=QPushButton("Go"); gb.setFixedWidth(48); gb.setFixedHeight(24); gb.clicked.connect(self._update_hex_view)
        nav.addWidget(gb); nav.addStretch(); lay.addLayout(nav)
        self._hex_view=QTextEdit(); self._hex_view.setReadOnly(True)
        self._hex_view.setFont(QFont("Courier New",9))
        self._hex_view.setLineWrapMode(QTextEdit.LineWrapMode.NoWrap)
        self._hex_view.setPlaceholderText("Hex dump appears here after loading SCM…")
        lay.addWidget(self._hex_view); return w

    def _build_info_tab(self):
        w=QFrame(); lay=QVBoxLayout(w); lay.setContentsMargins(8,8,8,8)
        t=QTextEdit(); t.setReadOnly(True); t.setFont(QFont("Arial",10))
        t.setHtml("""<h3>SCM Workshop</h3>
<p>Browse and patch the <b>GTA main.scm</b> compiled bytecode.</p>
<ul>
<li><b>Coord Search</b> — scan for XYZ float triplets within world bounds</li>
<li><b>Region Patch</b> — bulk-translate all coords in a rectangular region</li>
<li><b>Hex View</b> — inspect raw bytes at any offset</li>
<li><b>Save / Save As</b> — write the patched SCM back to disk</li>
</ul>
<p><b>⚠ Warning:</b> SCM is compiled bytecode. Float hits are heuristic — not all
will be world coords. Always keep a backup of the original main.scm.</p>
<h4>TODO (future)</h4>
<ul>
<li>Full opcode disassembly (GTA3 / VC / SA opcode tables)</li>
<li>Mission browser by name and entry offset</li>
<li>Full decompile / recompile round-trip</li>
</ul>""")
        lay.addWidget(t); return w

    def _open_file(self):
        path,_=QFileDialog.getOpenFileName(self,"Open SCM","","SCM Files (*.scm);;All Files (*)")
        if not path: return
        try:
            self._parser=SCMParser(); self._parser.load(path); self._scm_path=path
            name=os.path.basename(path)
            self._status_lbl.setText(f"{name}  |  {self._parser.size:,} bytes")
            self._game_lbl.setText(f"[{self._parser.game}]")
            self._search_btn.setEnabled(True)
            self._save_btn.setEnabled(True); self._saveas_btn.setEnabled(True)
            self._update_hex_view()
            if self.main_window and hasattr(self.main_window,'log_message'):
                self.main_window.log_message(f"SCM Workshop: {name} ({self._parser.size:,} bytes)")
        except Exception as e:
            QMessageBox.critical(self,"Error",f"Failed to load SCM:\n{e}")

    def _save_file(self):
        if not self._scm_path or not self._parser.data: return
        try: self._parser.save(self._scm_path); self._status_lbl.setText(f"Saved: {os.path.basename(self._scm_path)}")
        except Exception as e: QMessageBox.critical(self,"Save Error",str(e))

    def _save_as(self):
        if not self._parser.data: return
        path,_=QFileDialog.getSaveFileName(self,"Save SCM As",self._scm_path,"SCM Files (*.scm);;All Files (*)")
        if path:
            try: self._parser.save(path); self._scm_path=path; self._status_lbl.setText(f"Saved: {os.path.basename(path)}")
            except Exception as e: QMessageBox.critical(self,"Save Error",str(e))

    def _run_search(self):
        if not self._parser.data: return
        self._search_btn.setEnabled(False); self._progress.setVisible(True)
        self._progress.setRange(0,self._parser.size); self._progress.setValue(0)
        self._result_table.setRowCount(0); self._result_lbl.setText("Searching…")
        kw={'x_min':self._sx_min.value(),'x_max':self._sx_max.value(),
            'y_min':self._sy_min.value(),'y_max':self._sy_max.value()}
        if self._use_target.isChecked():
            kw.update({'target_x':self._tx.value(),'target_y':self._ty.value(),
                       'target_z':self._tz.value(),'tolerance':self._tol.value()})
        self._search_thread=SCMSearchThread(self._parser,kw)
        self._search_thread.progress.connect(lambda i,n:self._progress.setValue(i))
        self._search_thread.finished_s.connect(self._on_search_done)
        self._search_thread.start()

    def _on_search_done(self,hits):
        self._progress.setVisible(False); self._search_btn.setEnabled(True)
        self._result_lbl.setText(f"{len(hits):,} hits found")
        tbl=self._result_table; tbl.setRowCount(0); tbl.setSortingEnabled(False)
        for h in hits[:50_000]:
            r=tbl.rowCount(); tbl.insertRow(r)
            tbl.setItem(r,0,QTableWidgetItem(f"0x{h.offset:08X}"))
            tbl.setItem(r,1,QTableWidgetItem(f"{h.x:.4f}"))
            tbl.setItem(r,2,QTableWidgetItem(f"{h.y:.4f}"))
            tbl.setItem(r,3,QTableWidgetItem(f"{h.z:.4f}"))
            tbl.setItem(r,4,QTableWidgetItem("✓" if h.patched else ""))
            tbl.setRowHeight(r,20)
        tbl.setSortingEnabled(True)
        if self.main_window and hasattr(self.main_window,'log_message'):
            self.main_window.log_message(f"SCM search: {len(hits):,} coord hits")

    def _on_result_dclick(self,index):
        row=index.row(); off_str=self._result_table.item(row,0).text()
        self._hex_offset.setText(off_str); self._update_hex_view(); self._tabs.setCurrentIndex(2)

    def _apply_patch(self):
        if not self._parser.data or not self._parser.hits:
            QMessageBox.warning(self,"No Data","Run Coord Search first."); return
        x1=self._rx1.value(); x2=self._rx2.value()
        y1=self._ry1.value(); y2=self._ry2.value()
        dx=self._pdx.value(); dy=self._pdy.value(); dz=self._pdz.value()
        if dx==dy==dz==0: QMessageBox.warning(self,"No Offset","Set a non-zero offset."); return
        reply=QMessageBox.question(self,"Confirm Patch",
            f"Translate coords in X:[{x1:.0f},{x2:.0f}] Y:[{y1:.0f},{y2:.0f}]\n"
            f"by dX={dx:+.1f} dY={dy:+.1f} dZ={dz:+.1f}",
            QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No)
        if reply!=QMessageBox.StandardButton.Yes: return
        count=self._parser.patch_all_in_region(x1,x2,y1,y2,dx,dy,dz)
        msg=f"Patched {count} triplets  dX={dx:+.1f} dY={dy:+.1f} dZ={dz:+.1f}"
        self._patch_log.append(msg)
        if self.main_window and hasattr(self.main_window,'log_message'):
            self.main_window.log_message(f"SCM: {msg}")

    def _update_hex_view(self):
        if not self._parser.data: self._hex_view.setPlainText("No SCM loaded."); return
        try: offset=int(self._hex_offset.text(),16)
        except ValueError: offset=0
        self._hex_view.setPlainText(self._parser.hex_dump(offset,512))


def open_scm_workshop(main_window, file_path=None):
    try:
        mw=main_window
        if mw and hasattr(mw,'main_tab_widget'):
            from PyQt6.QtWidgets import QWidget,QVBoxLayout
            c=QWidget(); l=QVBoxLayout(c); l.setContentsMargins(0,0,0,0)
            w=SCMWorkshop(c,mw); l.addWidget(w)
            tw=mw.main_tab_widget
            try: icon=SVGIconFactory.get_terminal_icon()
            except: icon=None
            idx=tw.addTab(c,icon,"SCM Workshop") if icon else tw.addTab(c,"SCM Workshop")
            tw.setCurrentIndex(idx)
        else:
            w=SCMWorkshop(main_window=mw)
            from PyQt6.QtCore import Qt
            w.setWindowFlags(Qt.WindowType.Window)
            w.setWindowTitle("SCM Workshop — IMG Factory 1.6")
            w.resize(1000,700); w.show()
        if file_path and os.path.isfile(file_path): w._parser.load(file_path); w._scm_path=file_path
        return w
    except Exception as e:
        import traceback; traceback.print_exc()
        if main_window and hasattr(main_window,'log_message'): main_window.log_message(f"SCM Workshop error: {e}")
        return None
