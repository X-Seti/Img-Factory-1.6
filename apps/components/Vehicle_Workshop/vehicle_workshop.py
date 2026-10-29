#!/usr/bin/env python3
#this belongs in apps/components/Vehicle_Workshop/vehicle_workshop.py - Version: 18
# X-Seti - May08 2026 - Img Factory 1.6 - Vehicle Workshop

"""
Vehicle Workshop — tabbed editor for GTA III/VC/SA vehicle data files.
Tabs: Handling (handling.cfg) | Car Colours (carcols.dat) | Car Mods (carmods.dat SA)

Parsers are self-contained. Handling parser imported from Handling_Editor.
"""
import os, json, sys, threading, struct, re, math, shutil
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field

if sys.platform.startswith('linux'):   # xcb only exists on Linux
    os.environ.setdefault('QT_QPA_PLATFORM', 'xcb')
    os.environ.setdefault('QSG_RHI_BACKEND',  'opengl')

#Adding Standalone
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = Path(current_dir).parents[2]
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QGridLayout, QSplitter, QLabel, QLineEdit,
    QListWidget, QListWidgetItem, QScrollArea, QGroupBox, QTabWidget,
    QDoubleSpinBox, QSpinBox, QComboBox, QCheckBox, QPushButton,
    QFileDialog, QMessageBox, QApplication, QFormLayout, QFrame,
    QTableWidget, QTableWidgetItem, QHeaderView, QAbstractItemView,
    QColorDialog, QProgressBar, QMenu, QSizePolicy, QToolButton,
    QDialog, QFontComboBox, QDialogButtonBox, QTextEdit, QButtonGroup
)

from apps.methods.ribbon_system import RibbonMixin
from apps.methods.ribbon_dialog import RibbonIconsMixin
from apps.methods.img_factory_settings import get_user_config_dir
from PyQt6.QtCore import Qt, QPoint, QSize, QThread, pyqtSignal, QTimer
from PyQt6.QtGui import QAction, QBrush, QColor, QFont, QIcon, QImage, QKeySequence, QPainter, QPainterPath, QPen, QPixmap, QPolygon, QShortcut

try:
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    #from PyQt6.QtOpenGL import QOpenGLContext
    from PyQt6.QtGui import QSurfaceFormat
    from OpenGL.GL  import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
    # Request compatibility profile globally — required for fixed-function GL
    # (glMatrixMode, glLightfv, glBegin etc.) on PyQt6 / Python 3.12+
    _fmt = QSurfaceFormat()
    _fmt.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
    _fmt.setVersion(2, 1)
    QSurfaceFormat.setDefaultFormat(_fmt)
except Exception:
    QOpenGLWidget      = QWidget
    OPENGL_AVAILABLE   = False
    print("[Vehicle_Workshop] PyOpenGL not available — install python3-opengl")

from apps.methods.dff_viewport import VehicleViewport, DFFViewport
from apps.methods.dff_parser import load_dff, DFFParser
from apps.methods.txd_parser import parse_txd
from apps.methods.gui_workshop import GUIWorkshop
from apps.methods.statusbar_func import register_statusbar
from apps.components.Vehicle_Workshop.depends.vehicle_layout_func import VehicleLayoutMixin
from apps.components.Vehicle_Workshop.depends.vehicle_logic_func import VehicleLogicMixin
from apps.components.Vehicle_Workshop.depends.vehicle_toolbar_func import VehicleToolbarMixin
from apps.components.Vehicle_Workshop.depends.vehicle_win_func import VehicleWinMixin
from apps.components.Vehicle_Workshop.depends.vehicle_tabs_func import (
    CarColoursTab, CarModsTab, HandlingTab)

# GLViewportMixin — embedded in this file
class GLViewportMixin: pass


# - Detect standalone vs docked
def _is_standalone():  #vers 1
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

##Methods list -
# _is_standalone
# open_vehicle_workshop

##class VehicleWorkshop: -
# __init__
# _build_menus_into_qmenu
# _create_centre_panel
# _create_preview_tab
# _light_setup_dialog
# _open_file
# _open_specific
# _ribbon_config_path
# _save_as
# _save_file
# _show_progress
# _vw_load_dff
# _vw_load_txd
# _vw_pick_dff
# _vw_pick_paint1
# _vw_pick_paint2
# _vw_pick_txd
# _vw_set_mode
# _vw_toggle_assembly
# setup_ui

##Mixins (depends/) -
# GUIWorkshop (window chrome, settings) - methods/gui_workshop.py
# VehicleWinMixin - vehicle_win_func.py
# VehicleToolbarMixin - vehicle_toolbar_func.py
# VehicleLayoutMixin - vehicle_layout_func.py
# VehicleLogicMixin - vehicle_logic_func.py
# HandlingTab, CarColoursTab, CarModsTab - vehicle_tabs_func.py
# Shared imports and flags - vehicle_common_func.py

class VehicleWorkshop(RibbonIconsMixin, RibbonMixin, GLViewportMixin, VehicleWinMixin,
                      VehicleToolbarMixin, VehicleLayoutMixin, VehicleLogicMixin, GUIWorkshop): #vers 7
    _ribbon_name = "vehicle_workshop"
    _OUTER_LAYOUT_VERSION = 1      # Assets + Tools docks
    # Bump when the set of ribbons changes (1 = File ribbon)
    _RIBBON_LAYOUT_VERSION = 1

    def __init__(self, main_window=None, parent=None): #vers 6
        super().__init__(parent, main_window=main_window)   # docked: no corner handles
        self.main_window = main_window
        self.__dict__.setdefault('_dff_model', None)        # set on DFF load
        self.__dict__.setdefault('_current_dff_path', None)
        self._handling_path: Optional[str] = None
        self._carcols_path:  Optional[str] = None
        self._carmods_path:  Optional[str] = None
        # Docked mode: no FramelessWindowHint, widget is embedded in tab
        if parent is None and main_window is None:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        # setup_ui() already called by GUIWorkshop.__init__ via super().__init__()

    def _create_centre_panel(self): #vers 1
        from PyQt6.QtWidgets import QFrame, QVBoxLayout
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        lay = QVBoxLayout(panel); lay.setContentsMargins(0,0,0,0); lay.setSpacing(0)
        self._tabs = QTabWidget()
        self._tab_handling = HandlingTab()
        self._tab_carcols  = CarColoursTab()
        self._tab_carmods  = CarModsTab()
        self._tab_preview = self._create_preview_tab() #TODO this seems bugged, does not show self.viewport
        self._tabs.setCurrentIndex(0)
        self._tabs.addTab(self._tab_preview, "3D Preview")
        self._tabs.addTab(self._tab_handling, "Handling")
        self._tabs.addTab(self._tab_carcols,  "Car Colours")
        self._tabs.addTab(self._tab_carmods,  "Car Mods (SA)")
        lay.addWidget(self._tabs)
        return panel

    def setup_ui(self): #vers 7
        """Titlebar / Assets + Tools docks around ribbons and tabs / status bar."""
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(*self.get_content_margins())
        main_layout.setSpacing(self.setspacing)
        # Viewport must exist before the toolbar (toolbar buttons reference it)
        self.viewport = VehicleViewport()
        self.viewport.app_settings = self.app_settings
        main_layout.addWidget(self._create_toolbar())
        for name in ("open_btn", "save_btn"):           # the File ribbon replaces these
            b = getattr(self, name, None)
            if b:
                b.setVisible(False)
        left, right = self._create_left_panel(), self._create_right_panel()
        main_layout.addWidget(self._dock_layout(self.ribbon_wrap(self._create_centre_panel()), [
            ("Assets", left, 180, Qt.DockWidgetArea.LeftDockWidgetArea),
            ("Tools", right, 155, Qt.DockWidgetArea.RightDockWidgetArea)]), 1)
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        main_layout.addWidget(self._status_widget)
        register_statusbar(self, self._status_widget)
        self.ribbon_restore_state()
        self._inner_mw = self._ribbon_mw      # Ribbon Manager target
        self._apply_ribbon_layout()

    def _build_ribbons(self): #vers 1
        B = self.ribbon_button
        tb = self.ribbon_toolbar("File")
        B(tb, "open_icon",   "Open a DFF, TXD, handling.cfg, carcols.dat or carmods.dat  (Ctrl+O)", self._open_file)
        self.save_btn = B(tb, "save_icon", "Save the file on the current tab  (Ctrl+S) - backs up first", self._save_file)
        B(tb, "saveas_icon", "Save the current tab's file as...", self._save_as)
        B(tb, "export_icon", "Save every changed data file", self._save_all, text="All")

    def _create_preview_tab(self): #vers 1
        """Build the 3D Preview tab with DFFViewport and vehicle controls."""
        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout,
                                     QPushButton, QLabel, QComboBox, QSplitter)
        from PyQt6.QtCore import Qt
        tab = QWidget()
        lay = QVBoxLayout(tab); lay.setContentsMargins(4,4,4,4); lay.setSpacing(4)

        # No toolbar bar in 3D Preview tab — controls are in right panel
        # Open DFF/TXD are in the left panel; Render/Assembly/Paint in right panel

        # GL Viewport — use the shared self.viewport created in setup_ui
        self._vw_viewport = self.viewport
        lay.addWidget(self._vw_viewport, 1)

        # Status
        self._vw_status = QLabel('Open a DFF to preview')
        lay.addWidget(self._vw_status)
        return tab


    # Methods from model_viewer

    def _show_progress(self, visible: bool): #vers 1
        if hasattr(self, '_progress'):
            self._progress.setVisible(visible)
            if visible:
                from PyQt6.QtWidgets import QApplication
                QApplication.processEvents()

    def _light_setup_dialog(self): #vers 2
        """Light direction, intensity and vehicle paint colour dialog."""
        dlg = QDialog(self)
        dlg.setWindowTitle("Light & Paint Setup")
        dlg.resize(360, 380)
        lay = QVBoxLayout(dlg)
        form = QFormLayout()

        vp = self.viewport
        from PyQt6.QtWidgets import QSlider

        # Light direction sliders
        def _slider(lo, hi, val, scale=10):  #vers 1
            s = QSlider(Qt.Orientation.Horizontal)
            s.setRange(int(lo*scale), int(hi*scale))
            s.setValue(int(val*scale))
            return s

        sx = _slider(-10, 10, vp._light_dir[0])
        sy = _slider(-10, 10, vp._light_dir[1])
        sz = _slider(-10, 10, vp._light_dir[2])
        sa = _slider(0, 1, vp._ambient, 100)
        sd = _slider(0, 2, vp._diffuse, 100)

        lx_lbl = QLabel(f"{vp._light_dir[0]:.1f}")
        ly_lbl = QLabel(f"{vp._light_dir[1]:.1f}")
        lz_lbl = QLabel(f"{vp._light_dir[2]:.1f}")
        la_lbl = QLabel(f"{vp._ambient:.2f}")
        ld_lbl = QLabel(f"{vp._diffuse:.2f}")

        def _upd():  #vers 1
            x = sx.value()/10; y = sy.value()/10; z = sz.value()/10
            a = sa.value()/100; d = sd.value()/100
            lx_lbl.setText(f"{x:.1f}"); ly_lbl.setText(f"{y:.1f}"); lz_lbl.setText(f"{z:.1f}")
            la_lbl.setText(f"{a:.2f}"); ld_lbl.setText(f"{d:.2f}")
            vp.set_light_dir(x, y, z); vp.set_ambient(a); vp.set_diffuse(d)

        for s in (sx,sy,sz,sa,sd): s.valueChanged.connect(_upd)

        def _row(lbl, slider, val_lbl):  #vers 1
            rw = QWidget(); rl = QHBoxLayout(rw); rl.setContentsMargins(0,0,0,0)
            rl.addWidget(slider,1); rl.addWidget(val_lbl)
            form.addRow(lbl, rw)

        _row("Light X:", sx, lx_lbl)
        _row("Light Y:", sy, ly_lbl)
        _row("Light Z:", sz, lz_lbl)
        _row("Ambient:", sa, la_lbl)
        _row("Diffuse:", sd, ld_lbl)

        lay.addLayout(form)

        # Paint colours
        from PyQt6.QtWidgets import QGroupBox, QColorDialog
        paint_grp = QGroupBox("Vehicle Paint Preview")
        paint_lay = QHBoxLayout(paint_grp)

        def _colour_btn(label, current_rgb, setter):  #vers 1
            def _to_qcolor(rgb):  #vers 1
                from PyQt6.QtGui import QColor
                return QColor(int(rgb[0]*255), int(rgb[1]*255), int(rgb[2]*255))
            btn = QPushButton(label)
            btn.setStyleSheet(f"background-color: rgb({int(current_rgb[0]*255)},{int(current_rgb[1]*255)},{int(current_rgb[2]*255)})")
            btn.setFixedHeight(28)
            def _pick():  #vers 1
                col = QColorDialog.getColor(_to_qcolor(setter.__self__._paint1 if 'paint1' in label.lower() or 'primary' in label.lower() else setter.__self__._paint2), dlg)
                if col.isValid():
                    rgb = (col.redF(), col.greenF(), col.blueF())
                    setter(rgb)
                    btn.setStyleSheet(f"background-color: {col.name()}")
                    vp.update()
            btn.clicked.connect(_pick)
            return btn

        vp = self.viewport
        p1_btn = QPushButton("Primary Paint")
        p1_btn.setStyleSheet(f"background-color: rgb({int(vp._paint1[0]*255)},{int(vp._paint1[1]*255)},{int(vp._paint1[2]*255)})")
        p1_btn.setFixedHeight(28)
        def _pick1():  #vers 1
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint1[0]*255),int(vp._paint1[1]*255),int(vp._paint1[2]*255)), dlg)
            if col.isValid():
                vp._paint1 = (col.redF(), col.greenF(), col.blueF())
                p1_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p1_btn.clicked.connect(_pick1)

        p2_btn = QPushButton("Secondary Paint")
        p2_btn.setStyleSheet(f"background-color: rgb({int(vp._paint2[0]*255)},{int(vp._paint2[1]*255)},{int(vp._paint2[2]*255)})")
        p2_btn.setFixedHeight(28)
        def _pick2():  #vers 1
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint2[0]*255),int(vp._paint2[1]*255),int(vp._paint2[2]*255)), dlg)
            if col.isValid():
                vp._paint2 = (col.redF(), col.greenF(), col.blueF())
                p2_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p2_btn.clicked.connect(_pick2)

        paint_lay.addWidget(p1_btn); paint_lay.addWidget(p2_btn)
        lay.addWidget(paint_grp)

        reset_btn = QPushButton("Reset Defaults")
        def _reset():  #vers 1
            sx.setValue(10); sy.setValue(20); sz.setValue(15)
            sa.setValue(30); sd.setValue(85)
            vp._paint1 = (0.80, 0.20, 0.20)
            vp._paint2 = (0.20, 0.20, 0.80)
            p1_btn.setStyleSheet("background-color: rgb(204,51,51)")
            p2_btn.setStyleSheet("background-color: rgb(51,51,204)")
            vp.update()
        reset_btn.clicked.connect(_reset)

        btn_row = QHBoxLayout()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dlg.accept)
        btn_row.addWidget(reset_btn)
        btn_row.addStretch()
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)
        dlg.exec()

    def _vw_set_mode(self, mode): #vers 1
        vp = getattr(self,'_vw_viewport',None)
        if vp and hasattr(vp,'set_render_mode'): vp.set_render_mode(mode)

    def _vw_pick_dff(self): #vers 1
        from PyQt6.QtWidgets import QFileDialog
        path,_=QFileDialog.getOpenFileName(self,'Open DFF','','DFF (*.dff);;All (*)')
        if not path: return
        self._vw_load_dff(path)

    def _vw_pick_txd(self): #vers 1
        from PyQt6.QtWidgets import QFileDialog
        path,_=QFileDialog.getOpenFileName(self,'Open TXD','','TXD (*.txd);;All (*)')
        if not path: return
        self._vw_load_txd(path)

    def _vw_load_dff(self, path: str): #vers 2
        try:
            from apps.methods.dff_parser import load_dff
            m = load_dff(path)
            if not m: return
            self._vw_model = m
            self._vw_status.setText(f'{os.path.basename(path)} — {len(m.geometries)} geoms')
            # Also feed the full load_dff pipeline (geom list, frame tree, shared TXDs)
            self.load_dff(path)
            # Auto-load matching TXD
            import os as _os
            for ext in ('.txd','.TXD'):
                txd=_os.path.splitext(path)[0]+ext
                if _os.path.isfile(txd): self._vw_load_txd(txd); break
        except Exception as e:
            self._vw_status.setText(f'Error: {e}')

    def _vw_load_txd(self, path: str): #vers 2
        try:
            from apps.methods.txd_parser import parse_txd
            with open(path,'rb') as f: data=f.read()
            textures=parse_txd(data)
            vp=getattr(self,'_vw_viewport',None)
            if vp and hasattr(vp,'_upload_textures') and textures:
                vp._upload_textures(textures, additive=True)
                vp.update()
            # Populate texture list
            if textures and hasattr(self,'_tex_list'):
                from PyQt6.QtGui import QIcon, QImage, QPixmap
                from PyQt6.QtWidgets import QListWidgetItem
                from PyQt6.QtCore import Qt
                self._tex_list.clear()
                for t in textures:
                    item = QListWidgetItem(f"{t['name']}  {t.get('width',0)}\xd7{t.get('height',0)}")
                    rgba=t.get('rgba_data',b''); w=t.get('width',0); h=t.get('height',0)
                    if rgba and w>0 and h>0:
                        try:
                            img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
                            px=QPixmap.fromImage(img).scaled(32,32,
                                Qt.AspectRatioMode.KeepAspectRatio,
                                Qt.TransformationMode.SmoothTransformation)
                            item.setIcon(QIcon(px))
                        except Exception: pass
                    self._tex_list.addItem(item)
        except Exception as e:
            self._vw_status.setText(f'TXD error: {e}')

    def _vw_toggle_assembly(self, enabled: bool): #vers 1
        m = getattr(self,'_vw_model',None)
        vp = getattr(self,'_vw_viewport',None)
        if not m or not vp or not hasattr(vp,'set_assembly_mode'): return
        vp.set_assembly_mode(enabled)
        if enabled:
            vp.load_all_geometries(m.geometries,[g.materials for g in m.geometries],
                                   m.frames,m.atomics,
                                   damaged=getattr(self,'_damage_mode',False))
        elif m.geometries:
            vp.load_geometry(m.geometries[0],m.geometries[0].materials)

    def _vw_pick_paint1(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp=getattr(self,'_vw_viewport',None)
        if not vp or not hasattr(vp,'_paint1'): return
        p=vp._paint1
        col=QColorDialog.getColor(QColor(int(p[0]*255),int(p[1]*255),int(p[2]*255)),self)
        if col.isValid():
            vp._paint1=(col.redF(),col.greenF(),col.blueF())
            self._vw_paint1.setStyleSheet(f'background:{col.name()}')
            vp.update()

    def _vw_pick_paint2(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp=getattr(self,'_vw_viewport',None)
        if not vp or not hasattr(vp,'_paint2'): return
        p=vp._paint2
        col=QColorDialog.getColor(QColor(int(p[0]*255),int(p[1]*255),int(p[2]*255)),self)
        if col.isValid():
            vp._paint2=(col.redF(),col.greenF(),col.blueF())
            self._vw_paint2.setStyleSheet(f'background:{col.name()}')
            vp.update()

    def _open_file(self, path=None): #vers 4
        """Open button — shows dialog when no path given.
        When path is provided, loads directly without any dialog."""
        # Qt triggered signal passes checked=False/True — treat as no path
        if not isinstance(path, (str, bytes, os.PathLike)):
            path = None
        if path is not None:
            name = os.path.basename(path).lower()
            if name.endswith('.dff'):
                self._vw_load_dff(path)
                self._tabs.setCurrentIndex(0)
            elif name.endswith('.txd'):
                self._vw_load_txd(path)
                self._tabs.setCurrentIndex(0)
            elif 'handling' in name:
                if self._tab_handling.load_file(path):
                    self._handling_path = path
                    self._set_status(f"Handling: {os.path.basename(path)}")
                    try:
                        mw = getattr(self, 'main_window', None)
                        if mw and hasattr(self._tab_handling, '_parser') and self._tab_handling._parser:
                            names = {e.name.lower() for e in self._tab_handling._parser.entries if e.name}
                            mw.vehicle_names = getattr(mw, 'vehicle_names', set()) | names
                    except Exception:
                        pass
            elif 'carcols' in name:
                if self._tab_carcols.load_file(path):
                    self._carcols_path = path
                    self._set_status(f"Car Colours: {os.path.basename(path)}")
            elif 'carmods' in name:
                if self._tab_carmods.load_file(path):
                    self._carmods_path = path
                    self._set_status(f"Car Mods: {os.path.basename(path)}")
            elif ('vehicles' in name or 'default' in name) and name.endswith('.ide'):
                # SA/VC vehicles.ide or GTA3 default.ide — parse wheel scales
                try:
                    self._vehicles_ide_path = path
                    self._parse_sa_vehicles_ide(path)
                    self._set_status(f"Vehicles IDE: {os.path.basename(path)}")
                except Exception as e:
                    self._set_status(f"Vehicles IDE error: {e}")
            elif 'cargrp' in name:
                # cargrp.dat — informational for now
                self._cargrp_path = path
                self._set_status(f"Car Groups: {os.path.basename(path)}")
            return
        # No path — show dialog based on active tab
        active_tab = self._tabs.currentIndex() if hasattr(self, '_tabs') else -1
        if active_tab == 0:
            self._vw_pick_dff()
            return
        path, _ = QFileDialog.getOpenFileName(
            self, "Open Vehicle Data File", "",
            "All supported (*.dff *.cfg *.dat *.txd);;"
            "DFF Model (*.dff);;TXD Textures (*.txd);;"
            "Handling (handling.cfg *.cfg);;Car Colours (carcols.dat *.dat);;"
            "Car Mods (carmods.dat *.dat);;All files (*)")
        if not path: return
        name = os.path.basename(path).lower()
        if name.endswith('.dff'):
            self._vw_load_dff(path)
            self._tabs.setCurrentIndex(0)
        elif name.endswith('.txd'):
            self._vw_load_txd(path)
            self._tabs.setCurrentIndex(0)
        elif "handling" in name:
            if self._tab_handling.load_file(path):
                self._handling_path = path
                self._tabs.setCurrentWidget(self._tab_handling)
                self._set_status(f"Handling: {os.path.basename(path)}")
                # Cache vehicle names on main_window for right-click menu
                try:
                    mw = getattr(self, 'main_window', None)
                    if mw and hasattr(self._tab_handling, '_parser') and self._tab_handling._parser:
                        names = {e.name.lower() for e in self._tab_handling._parser.entries if e.name}
                        existing = getattr(mw, 'vehicle_names', set())
                        mw.vehicle_names = existing | names
                except Exception:
                    pass
        elif "carcols" in name:
            if self._tab_carcols.load_file(path):
                self._carcols_path = path
                self._tabs.setCurrentWidget(self._tab_carcols)
                self._set_status(f"Car Colours: {os.path.basename(path)}")
        elif "carmods" in name:
            if self._tab_carmods.load_file(path):
                self._carmods_path = path
                self._tabs.setCurrentWidget(self._tab_carmods)
                self._set_status(f"Car Mods: {os.path.basename(path)}")
        else:
            QMessageBox.warning(self, "Unknown",
                "Expected a .dff, .txd, handling.cfg, carcols.dat or carmods.dat")

    # -- data files: (tab, path attribute, dialog filter)
    def _data_tabs(self):
        return [(self._tab_handling, "_handling_path", "Handling (handling.cfg *.cfg)"),
                (self._tab_carcols,  "_carcols_path",  "Car Colours (carcols.dat *.dat)"),
                (self._tab_carmods,  "_carmods_path",  "Car Mods (carmods.dat *.dat)")]

    def _current_data_tab(self):
        cur = self._tabs.currentWidget()
        return next((t for t in self._data_tabs() if t[0] is cur), None)

    def has_unsaved(self) -> bool: #vers 1
        return any(getattr(t, "dirty", False) for t, _a, _f in self._data_tabs())

    def _write_data_tab(self, tab, path: str) -> bool: #vers 1
        """Back the existing file up, then let the tab's file model write it atomically."""
        from apps.methods.file_backup import backup_file, note_change
        if os.path.exists(path):
            note_change(f"Save {os.path.basename(path)}")
            if backup_file(path) is None:
                QMessageBox.warning(self, "Save", "Backup failed - file not overwritten.")
                return False
        if not tab.save_file(path):
            QMessageBox.critical(self, "Save", f"Could not write {os.path.basename(path)}")
            return False
        return True

    def _save_file(self): #vers 2
        """Save the file on the current tab (the old version saved by tab NUMBER with
        the numbers off by one, so it wrote the wrong data to the wrong file)."""
        entry = self._current_data_tab()
        if entry is None:
            self._save_all()
            return
        tab, attr, _flt = entry
        path = getattr(self, attr, None)
        if not path:
            self._save_as()
            return
        if self._write_data_tab(tab, path):
            self._set_status(f"Saved {os.path.basename(path)}")

    def _save_all(self): #vers 1
        saved = []
        for tab, attr, _f in self._data_tabs():
            path = getattr(self, attr, None)
            if path and getattr(tab, "dirty", False) and self._write_data_tab(tab, path):
                saved.append(os.path.basename(path))
        self._set_status("Saved " + ", ".join(saved) if saved else "Nothing to save")

    def _save_as(self): #vers 2
        entry = self._current_data_tab()
        if entry is None:
            QMessageBox.information(self, "Save As", "Switch to the Handling, Car Colours or Car Mods tab first.")
            return
        tab, attr, flt = entry
        path, _ = QFileDialog.getSaveFileName(self, "Save As", getattr(self, attr, None) or "", flt)
        if path and self._write_data_tab(tab, path):
            setattr(self, attr, path)
            self._set_status(f"Saved {os.path.basename(path)}")

    def _ribbon_config_path(self): #vers 1
        """Dock and ribbon-layout file."""
        return get_user_config_dir() / "vehicle_workshop_docks.json"

    def closeEvent(self, event): #vers 3
        if self.standalone_mode and self.has_unsaved():
            r = QMessageBox.question(
                self, "Vehicle Workshop", "Save changed data files before closing?",
                QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                event.ignore()
                return
            if r == QMessageBox.StandardButton.Save:
                self._save_all()
                if self.has_unsaved():
                    event.ignore()
                    return
        self.ribbon_save_state()
        self._save_outer_layout()
        super().closeEvent(event)

    def _open_specific(self, kind: str): #vers 1
        f = {"handling":"Handling (handling.cfg *.cfg)",
             "carcols": "Car Colours (carcols.dat *.dat)",
             "carmods":  "Car Mods (carmods.dat *.dat)"}[kind]
        path, _ = QFileDialog.getOpenFileName(self, f"Open {kind}", "", f)
        if path: self._open_file(path)

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open…",              self._open_file)
        fm.addAction("Save",               self._save_file)
        fm.addAction("Save As…",           self._save_as)
        fm.addSeparator()
        fm.addAction("Open handling.cfg",  lambda: self._open_specific("handling"))
        fm.addAction("Open carcols.dat",   lambda: self._open_specific("carcols"))
        fm.addAction("Open carmods.dat",   lambda: self._open_specific("carmods"))
        fm.addSeparator()
        fm.addAction("Close", self.close)


# - Main workshop widget (RadarWorkshop pattern)
def open_vehicle_workshop(main_window=None, path: str = None): #vers 1
    if not QApplication.instance():
        try:
            from PyQt6.QtGui import QSurfaceFormat
            _f = QSurfaceFormat()
            _f.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
            _f.setVersion(2, 1)
            QSurfaceFormat.setDefaultFormat(_f)
        except Exception:
            pass
    app = QApplication.instance() or QApplication(sys.argv)
    w = VehicleWorkshop(main_window)
    w.resize(1200, 720)
    w.show()
    if path: w._open_file(path)
    return w


if __name__ == "__main__": #vers 1
    # Must set compat profile BEFORE QApplication
    try:
        from PyQt6.QtGui import QSurfaceFormat
        _f = QSurfaceFormat()
        _f.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
        _f.setVersion(2, 1)
        QSurfaceFormat.setDefaultFormat(_f)
    except Exception:
        pass
    app = QApplication(sys.argv)
    w = VehicleWorkshop()
    w.resize(1200, 720)
    w.show()
    sys.exit(app.exec())
