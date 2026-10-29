#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_layout_func.py - Version: 2
# X-Seti - October 14 2026 - IMG Factory 1.6 - Vehicle Workshop layout

"""
Panels, sidebar, viewport and list layout.
"""

##class VehicleLayoutMixin: -
# _create_centre_panel
# _create_left_panel
# _create_right_panel
# _create_status_bar
# _filter_img_list
# _light_setup_dialog
# _make_section_header
# _on_frame_visibility_changed
# _on_geom_selected
# _on_img_entry_dclicked
# _populate_frame_tree
# _populate_geom_list
# _populate_sidebar
# _set_status
# get_tab_margins
# setup_ui

import os, re

from PyQt6.QtCore import QSize, QTimer, Qt
from PyQt6.QtGui import QColor
from PyQt6.QtWidgets import (
    QApplication, QButtonGroup, QColorDialog, QDialog, QFormLayout, QFrame, QGroupBox,
    QHBoxLayout, QLabel, QLineEdit, QListWidget, QProgressBar, QPushButton, QSizePolicy,
    QSlider, QSplitter, QTabWidget, QToolButton, QVBoxLayout, QWidget)

from apps.methods.dff_viewport import VehicleViewport
from apps.components.Vehicle_Workshop.depends.vehicle_common_func import (
    ICONS_AVAILABLE, SVGIconFactory)


class VehicleLayoutMixin:
    """Panel creation and layout.
    Mixed into GUIWorkshop — not used standalone.
    Override any _create_* method in your subclass to replace that panel.
    """

    def get_tab_margins(self): #vers 1
        return (self.tabmerginsa, self.tabmerginsb, self.tabmerginsc, self.tabmerginsd)

    def setup_ui(self): #vers 2
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(*self.get_content_margins())
        main_layout.setSpacing(self.setspacing)

        # Viewport must exist before toolbar (toolbar buttons reference it)
        self.viewport = VehicleViewport()
        self.viewport.app_settings = self.app_settings

        main_layout.addWidget(self._create_toolbar())

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(self._create_left_panel())
        splitter.addWidget(self._create_centre_panel())
        splitter.addWidget(self._create_right_panel())
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 5)
        splitter.setStretchFactor(2, 0)
        splitter.setSizes([200, 800, 180])
        main_layout.addWidget(splitter, 1)
        main_layout.addWidget(self._create_status_bar())

    def _create_left_panel(self): #vers 2
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(180); panel.setMaximumWidth(280)
        outer = QVBoxLayout(panel)
        outer.setContentsMargins(*self.get_panel_margins())
        outer.setSpacing(2)


        # Vertical splitter — all three sections resizable
        splitter = QSplitter(Qt.Orientation.Vertical)
        outer.addWidget(splitter, 1)


        # - IMG Entries section
        img_sec = QWidget(); img_lay = QVBoxLayout(img_sec); img_lay.setContentsMargins(0,0,0,0); img_lay.setSpacing(2)
        img_lay.addWidget(self._make_section_header("IMG Entries (DFF)", self._filter_img_list))
        self._img_list = QListWidget()
        self._img_list.setFont(self.panel_font)
        self._img_list.itemDoubleClicked.connect(self._on_img_entry_dclicked)
        img_lay.addWidget(self._img_list)
        splitter.addWidget(img_sec)


        # - Geometries section
        geom_sec = QWidget(); gl = QVBoxLayout(geom_sec); gl.setContentsMargins(0,0,0,0); gl.setSpacing(2)
        gl.addWidget(self._make_section_header("Geometries"))
        self._geom_list = QListWidget()
        self._geom_list.setFont(self.panel_font)
        self._geom_list.currentRowChanged.connect(self._on_geom_selected)
        gl.addWidget(self._geom_list)
        splitter.addWidget(geom_sec)


        # - Textures section with thumbnails
        tex_sec = QWidget(); tl = QVBoxLayout(tex_sec); tl.setContentsMargins(0,0,0,0); tl.setSpacing(2)
        tl.addWidget(self._make_section_header("Textures"))
        self._tex_list = QListWidget()
        self._tex_list.setFont(self.panel_font)
        self._tex_list.setIconSize(QSize(32,32))
        self._tex_list.setViewMode(QListWidget.ViewMode.ListMode)
        tl.addWidget(self._tex_list)
        splitter.addWidget(tex_sec)

        # - Frame Hierarchy section with visibility checkboxes
        frame_sec = QWidget(); fl = QVBoxLayout(frame_sec)
        fl.setContentsMargins(0,0,0,0); fl.setSpacing(2)
        fl.addWidget(self._make_section_header('Frame Hierarchy'))
        from PyQt6.QtWidgets import QTreeWidget, QTreeWidgetItem
        self._frame_tree = QTreeWidget()
        self._frame_tree.setFont(self.panel_font)
        self._frame_tree.setHeaderHidden(True)
        self._frame_tree.setColumnCount(1)
        self._frame_tree.itemChanged.connect(self._on_frame_visibility_changed)
        fl.addWidget(self._frame_tree)
        splitter.addWidget(frame_sec)

        splitter.setSizes([180, 120, 100, 120])

        # Open DFF / Open TXD buttons at bottom of left panel

        btn_row = QHBoxLayout(); btn_row.setSpacing(4)
        from PyQt6.QtWidgets import QToolButton
        ic=self._get_icon_color()
        for label,tip,cb,iname in [
                ('Open DFF','Open DFF model',self._vw_pick_dff,'open'),
                ('Open TXD','Open TXD textures',self._vw_pick_txd,'open')]:
            b=QToolButton(); b.setFixedHeight(28); b.setFont(self.panel_font); b.setToolTip(tip)
            b.setText(label)
            try:
                ico=getattr(SVGIconFactory,f'{iname}_icon',None)
                if ico: b.setIcon(ico(14,ic)); b.setIconSize(QSize(14,14)); b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            except Exception: pass
            b.setSizePolicy(QSizePolicy.Policy.Expanding,QSizePolicy.Policy.Fixed)
            b.clicked.connect(cb); btn_row.addWidget(b)
        self._vw_open_dff_btn=btn_row.itemAt(0).widget()
        self._vw_open_txd_btn=btn_row.itemAt(1).widget()

        outer.addLayout(btn_row)

        return panel

    def _create_centre_panel(self): #Vers 1  #vers 1
        """Centre panel — tab view with placeholder tabs.
        Override to replace with your own canvas/tabs.
        """
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(0)

        self._view_tabs = QTabWidget()
        self._view_tabs.setDocumentMode(True)
        self._view_tabs.currentChanged.connect(self._on_tab_changed)

        for label in ("View A", "View B"):
            tab = QWidget()
            tl  = QVBoxLayout(tab)
            tl.addWidget(QLabel(
                f"[ {label} — override _create_centre_panel() ]",
                alignment=Qt.AlignmentFlag.AlignCenter))
            self._view_tabs.addTab(tab, label)

        cl.addWidget(self._view_tabs)
        return panel

        # - right panel — model info
    def _create_right_panel(self): #vers 4
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(155); panel.setMaximumWidth(200)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(*self.get_panel_margins())
        lay.setSpacing(2)

        ic = self._get_icon_color()
        def _icon(fname, size=14):  #vers 1
            if not ICONS_AVAILABLE: return None
            try:
                fn = getattr(SVGIconFactory, f'get_{fname}_icon', None)
                return fn(size, ic) if fn else None
            except Exception: return None

        from PyQt6.QtWidgets import QToolButton as _QTB, QSlider

        def _tbtn(text, tip, cb, iname=None, checkable=False, checked=False):  #vers 1
            b = _QTB(); b.setFont(self.infobar_font)
            b.setToolTip(tip); b.setMinimumHeight(28)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(13,13))
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
                b.setToolTip(f'{text} — {tip}')
            else:
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
            b.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True); b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(lambda _=False: cb())
            return b

        def _tbtn_txt(text, tip, cb, iname=None, checkable=False, checked=False):  #vers 1
            """Button with icon+short text side by side."""
            b = _QTB(); b.setFont(self.infobar_font)
            b.setToolTip(tip); b.setMinimumHeight(28)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(13,13))
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            else:
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
            b.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True); b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(lambda _=False: cb())
            return b

        def _row(*widgets, spacing=2):  #vers 1
            w = QWidget(); h = QHBoxLayout(w)
            h.setContentsMargins(0,0,0,0); h.setSpacing(spacing)
            for ww in widgets:
                if isinstance(ww, int): h.addSpacing(ww)
                elif ww == 'stretch':  h.addStretch()
                else: h.addWidget(ww)  # no stretch factor — use natural size
            return w

        def _sep():  #vers 1
            from PyQt6.QtWidgets import QFrame as _QF
            s = _QF(); s.setFrameShape(_QF.Shape.HLine)
            s.setFixedHeight(1); s.setStyleSheet("background:#333;")
            return s

        def _lbl(text):  #vers 1
            l = QLabel(text); l.setFont(self.panel_font); return l

        #    Render                                       
        lay.addWidget(_lbl("Render"))
        self._mode_group = QButtonGroup(self); self._mode_group.setExclusive(True)
        mode_row = QWidget(); mode_h = QHBoxLayout(mode_row)
        mode_h.setContentsMargins(0,0,0,0); mode_h.setSpacing(2)
        for label, mode, iname in [("Wire","wireframe","dff_edit"),
                                    ("Solid","solid","properties"),
                                    ("Tex","textured","image")]:
            b = _QTB(); b.setFont(self.infobar_font); b.setMinimumHeight(28)
            b.setToolTip(f"{mode.capitalize()} render mode")
            ico = _icon(iname)
            if ico: b.setIcon(ico); b.setIconSize(QSize(13,13))
            b.setText(label)
            b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            b.setCheckable(True); b.setChecked(mode=='solid')
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            b.toggled.connect(lambda checked, m=mode: self._set_mode(m) if checked else None)
            self._mode_group.addButton(b); mode_h.addWidget(b)
        lay.addWidget(mode_row)

        # Backface + Grid + PreLit — icon-only row
        self._cull_btn   = _tbtn("Cull",   "Backface culling", self.viewport.set_backface_cull, 'col_file', True, False)
        self._grid_btn   = _tbtn("Grid",   "Toggle grid",      self.viewport.set_show_grid,     'fit_grid', True, True)
        self._prelit_btn = _tbtn("PreLit", "Pre-lighting",     self.viewport.set_prelight,      'brighten', True, False)
        lay.addWidget(_row(self._cull_btn, self._grid_btn, self._prelit_btn))
        lay.addWidget(_sep())

        #    Camera + Light                                
        _reset_btn = _tbtn_txt("Reset", "Reset camera", self.viewport.reset_camera, 'refresh')
        _light_btn = _tbtn_txt("Light", "Light setup",  self._light_setup_dialog,   'weather')
        lay.addWidget(_row(_reset_btn, _light_btn))
        lay.addWidget(_sep())

        #    Paint                                         
        lay.addWidget(_lbl("Paint"))
        self._paint1_btn = QPushButton("Pri")
        self._paint2_btn = QPushButton("Sec")
        self._carcol_btn = QPushButton("Cols")
        for b in (self._paint1_btn, self._paint2_btn, self._carcol_btn):
            b.setMinimumHeight(28); b.setFont(self.infobar_font)
        self._paint1_btn.clicked.connect(self._pick_paint1)
        self._paint2_btn.clicked.connect(self._pick_paint2)
        self._carcol_btn.clicked.connect(self._cycle_carcol)
        self._carcol_btn.setToolTip("Cycle through carcols.dat colour pairs")
        lay.addWidget(_row(self._paint1_btn, self._paint2_btn, self._carcol_btn))
        self._update_paint_btns()

        self._carcols_widget = QWidget()
        self._carcols_lay = QVBoxLayout(self._carcols_widget)
        self._carcols_lay.setContentsMargins(0,0,0,2); self._carcols_lay.setSpacing(2)
        lay.addWidget(self._carcols_widget)
        lay.addWidget(_sep())

        #    Assembly                                      
        lay.addWidget(_lbl("Assembly"))
        self._assemble_btn = _tbtn_txt("All",    "Show all parts", self._toggle_assembly_mode, 'select_all', True, False)
        self._damage_btn   = _tbtn_txt("Dmg", "Damaged state",  self._toggle_damage_mode,   'warning',    True, False)
        self._lod_btn      = _tbtn_txt("LOD",    "Show LOD",       self._toggle_lod_mode,      'search',     True, False)
        lay.addWidget(_row(self._assemble_btn, self._damage_btn, self._lod_btn))
        lay.addWidget(_sep())

        #    Animate                                       
        lay.addWidget(_lbl("Animate"))
        self._anim_btn = _tbtn_txt('Play', 'Play door animation',
            self._toggle_animation, 'go', True, False)
        self._anim_btn.setFixedWidth(60)
        spd_slider = QSlider(Qt.Orientation.Horizontal)
        spd_slider.setRange(1, 30); spd_slider.setValue(10)
        spd_slider.setToolTip('Animation speed')
        spd_slider.valueChanged.connect(self._set_anim_speed)
        self._anim_speed_slider = spd_slider
        anim_row = QWidget(); anim_h = QHBoxLayout(anim_row)
        anim_h.setContentsMargins(0,0,0,0); anim_h.setSpacing(3)
        anim_h.addWidget(self._anim_btn)
        anim_h.addWidget(spd_slider, 1)
        lay.addWidget(anim_row)

        # Door toggles — icon-only, 6 buttons
        lay.addWidget(_lbl("Doors"))
        door_row = QWidget(); door_h = QHBoxLayout(door_row)
        door_h.setContentsMargins(0,0,0,0); door_h.setSpacing(2)
        self._door_btns = {}
        for dlabel, dname in [('LF','door_lf'),('RF','door_rf'),('LR','door_lr'),
                               ('RR','door_rr'),('Hd','bonnet'),('Bt','boot')]:
            db = _QTB(); db.setFixedHeight(22); db.setFixedWidth(28)
            db.setFont(self.infobar_font); db.setText(dlabel)
            db.setToolTip(f'Open/close {dname}')
            db.setCheckable(True)
            db.toggled.connect(lambda checked, n=dname: self._toggle_door(n, checked))
            door_h.addWidget(db); self._door_btns[dname] = db
        lay.addWidget(door_row)
        lay.addWidget(_sep())

        #    Wheels                                        
        self._wheels_btn = _tbtn_txt("Wheels", "Show wheels",
                                     self._toggle_show_wheels, 'col_workshop', True, False)
        self._wheels_btn.setFixedWidth(72)
        steer_slider = QSlider(Qt.Orientation.Horizontal)
        steer_slider.setRange(-35, 35); steer_slider.setValue(0)
        steer_slider.setToolTip('Front wheel steer angle')
        steer_slider.valueChanged.connect(self._set_wheel_steer)
        self._wheel_heading_slider = steer_slider
        wheel_row = QWidget(); wheel_h = QHBoxLayout(wheel_row)
        wheel_h.setContentsMargins(0,0,0,0); wheel_h.setSpacing(3)
        wheel_h.addWidget(self._wheels_btn); wheel_h.addWidget(steer_slider, 1)
        lay.addWidget(wheel_row)

        # Wheel scale adjustment (+/- 10%)
        scale_row = QWidget(); scale_h = QHBoxLayout(scale_row)
        scale_h.setContentsMargins(0,0,0,0); scale_h.setSpacing(3)
        scale_lbl = QLabel('Size'); scale_lbl.setFont(self.infobar_font)
        scale_lbl.setFixedWidth(28)
        wheel_scale_slider = QSlider(Qt.Orientation.Horizontal)
        wheel_scale_slider.setRange(-30, 30); wheel_scale_slider.setValue(0)
        wheel_scale_slider.setToolTip('Wheel size offset ±10% (does not save)')
        self._wheel_scale_slider = wheel_scale_slider
        self._wheel_scale_lbl = QLabel('0%'); self._wheel_scale_lbl.setFont(self.infobar_font)
        self._wheel_scale_lbl.setFixedWidth(28)
        def _on_scale_change(v):  #vers 1
            self._wheel_scale_lbl.setText(f'{v:+d}%')
            mult = 1.0 + v / 100.0
            self.viewport._wheel_scale_mult = mult
            if getattr(self.viewport, '_show_wheels', False):
                self._toggle_show_wheels(True)
        wheel_scale_slider.valueChanged.connect(_on_scale_change)
        scale_h.addWidget(scale_lbl); scale_h.addWidget(wheel_scale_slider, 1)
        scale_h.addWidget(self._wheel_scale_lbl)
        lay.addWidget(scale_row)
        lay.addWidget(_sep())

        #    Model Info (collapsible)                       
        info_toggle = _QTB(); info_toggle.setText("▸ Info")
        info_toggle.setFont(self.infobar_font); info_toggle.setFixedHeight(22)
        info_toggle.setCheckable(True)
        info_toggle.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
        info_toggle.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        lay.addWidget(info_toggle)

        self._info_lbl = QLabel('—')
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setVisible(False)
        lay.addWidget(self._info_lbl)
        info_toggle.toggled.connect(lambda c: (
            self._info_lbl.setVisible(c),
            info_toggle.setText(("▾ Info" if c else "▸ Info"))))
        lay.addWidget(_sep())

        #    Editing                                       
        lay.addWidget(_lbl("Editing"))
        # Row 1: CoM, Dummies, Suspension
        # Row 2: Seats, Bounds, Export
        edit_defs = [
            ("CoM",  "Centre of Mass — reads from handling.cfg", self._toggle_com_indicator,  'locate',    True),
            ("Dum",  "Dummy frame positions",                     self._toggle_dummy_overlay,  'tree',      True),
            ("Susp", "Suspension travel range",                   self._toggle_suspension_vis, 'split',     True),
            ("Seat", "Seat positions from ped_frontseat frames",  self._toggle_seat_overlay,   'properties',True),
            ("Box",  "Bounding box",                              self._toggle_bounds,         'col_file',  True),
            ("Exp",  "Export DFF to OBJ/DFF",                     self._export_dff,            'export',    False),
        ]
        self._edit_btns = []
        er1 = QWidget(); eh1 = QHBoxLayout(er1); eh1.setContentsMargins(0,0,0,0); eh1.setSpacing(2)
        er2 = QWidget(); eh2 = QHBoxLayout(er2); eh2.setContentsMargins(0,0,0,0); eh2.setSpacing(2)
        for i, (label, tip, cb, iname, checkable) in enumerate(edit_defs):
            b = _QTB(); b.setMinimumHeight(28)
            b.setFont(self.infobar_font); b.setToolTip(tip)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(13,13))
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
            else:
                b.setText(label)
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True)
                b.toggled.connect(lambda checked, fn=cb, lbl=label: fn() if True else None)
            else:
                b.clicked.connect(lambda _=False, fn=cb: fn())
            self._edit_btns.append(b)
            (eh1 if i < 3 else eh2).addWidget(b)
        lay.addWidget(er1); lay.addWidget(er2)

        lay.addStretch()
        return panel
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(160); panel.setMaximumWidth(210)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(*self.get_panel_margins())
        lay.setSpacing(3)

        ic = self._get_icon_color()
        def _icon(name, size=16):  #vers 1
            if not ICONS_AVAILABLE: return None
            try:
                fn = getattr(SVGIconFactory, name+'_icon', None)
                return fn(size, ic) if fn else None
            except Exception: return None

        from PyQt6.QtWidgets import QToolButton as _QTB, QSlider

        def _tbtn(text, tip, cb, iname=None, checkable=False, checked=False, w=None):  #vers 1
            b = _QTB(); b.setFont(self.infobar_font)
            b.setToolTip(tip); b.setFixedHeight(26)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(14,14))
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            b.setText(text)
            if w: b.setFixedWidth(w)
            else: b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True); b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(lambda _=False: cb())
            return b

        def _row(*widgets, spacing=3):  #vers 1
            w = QWidget(); h = QHBoxLayout(w)
            h.setContentsMargins(0,0,0,0); h.setSpacing(spacing)
            for ww in widgets:
                if isinstance(ww, int): h.addSpacing(ww)
                elif ww == 'stretch':  h.addStretch()
                else: h.addWidget(ww, 1)
            return w

        def _sep():  #vers 1
            from PyQt6.QtWidgets import QFrame as _QF
            s = _QF(); s.setFrameShape(_QF.Shape.HLine)
            s.setFixedHeight(1)
            s.setStyleSheet("background:#333;")
            return s

        def _lbl(text):  #vers 1
            l = QLabel(text); l.setFont(self.panel_font)
            return l

        #    Render                                       
        lay.addWidget(_lbl("Render"))
        self._mode_group = QButtonGroup(self); self._mode_group.setExclusive(True)
        mode_row = QWidget(); mode_h = QHBoxLayout(mode_row)
        mode_h.setContentsMargins(0,0,0,0); mode_h.setSpacing(2)
        for label, mode, iname in [("Wire","wireframe","wireframe"),
                                    ("Solid","solid","solid"),
                                    ("Tex","textured","texture")]:
            b = _QTB(); b.setFont(self.infobar_font); b.setFixedHeight(26)
            b.setToolTip(f"{mode.capitalize()} render mode")
            ico = _icon(iname)
            if ico: b.setIcon(ico); b.setIconSize(QSize(14,14))
            b.setText(label)
            b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            b.setCheckable(True); b.setChecked(mode=='solid')
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            b.toggled.connect(lambda checked, m=mode: self._set_mode(m) if checked else None)
            self._mode_group.addButton(b); mode_h.addWidget(b)
        lay.addWidget(mode_row)

        #    View toggles (Backface + Grid same row)       
        self._cull_btn   = _tbtn("Backface", "Toggle backface culling",  self.viewport.set_backface_cull, 'backface', True, False)
        self._grid_btn   = _tbtn("Grid",     "Toggle grid",              self.viewport.set_show_grid,     'grid',     True, True)
        self._prelit_btn = _tbtn("PreLit",   "Vertex pre-lighting",      self.viewport.set_prelight,      'shading',  True, False)
        lay.addWidget(_row(self._cull_btn, self._grid_btn))
        lay.addWidget(self._prelit_btn)

        lay.addWidget(_sep())

        #    Camera + Light same row                       
        _reset_btn = _tbtn("Reset", "Reset camera", self.viewport.reset_camera, 'reset')
        _light_btn = _tbtn("Light", "Light setup",  self._light_setup_dialog,   'light')
        lay.addWidget(_row(_reset_btn, _light_btn))

        lay.addWidget(_sep())

        #    Paint                                         
        lay.addWidget(_lbl("Paint"))
        self._paint1_btn = QPushButton("Pri")
        self._paint2_btn = QPushButton("Sec")
        for b in (self._paint1_btn, self._paint2_btn):
            b.setFixedHeight(26); b.setFont(self.infobar_font)
        self._paint1_btn.clicked.connect(self._pick_paint1)
        self._paint2_btn.clicked.connect(self._pick_paint2)
        lay.addWidget(_row(self._paint1_btn, self._paint2_btn))
        self._update_paint_btns()

        # Carcols swatches
        self._carcols_widget = QWidget()
        self._carcols_lay = QVBoxLayout(self._carcols_widget)
        self._carcols_lay.setContentsMargins(0,0,0,2); self._carcols_lay.setSpacing(2)
        lay.addWidget(self._carcols_widget)

        lay.addWidget(_sep())

        #    Assembly (3 buttons same row with icons)      
        lay.addWidget(_lbl("Assembly"))
        self._assemble_btn = _tbtn("All",    "Show all parts assembled", self._toggle_assembly_mode, 'mesh',     True, False)
        self._damage_btn   = _tbtn("Damage", "Show damaged state",       self._toggle_damage_mode,   'warning',  True, False)
        self._lod_btn      = _tbtn("LOD",    "Show LOD meshes",          self._toggle_lod_mode,      'zoom_out', True, False)
        lay.addWidget(_row(self._assemble_btn, self._damage_btn, self._lod_btn))

        lay.addWidget(_sep())

        #    Animate: [Play] Speed [---x---] same row      
        lay.addWidget(_lbl("Animate"))
        self._anim_btn = _tbtn('Play', 'Start/stop animation',
            lambda: self.viewport.set_animation(self._anim_btn.isChecked()),
            'play', True, False)
        self._anim_btn.setFixedWidth(54)
        spd_slider = QSlider(Qt.Orientation.Horizontal)
        spd_slider.setRange(1, 30); spd_slider.setValue(10)
        spd_slider.setToolTip('Animation speed')
        spd_slider.valueChanged.connect(lambda v: self.viewport.set_animation_speed(v/10.0))
        self._anim_speed_slider = spd_slider
        anim_row = QWidget(); anim_h = QHBoxLayout(anim_row)
        anim_h.setContentsMargins(0,0,0,0); anim_h.setSpacing(4)
        anim_h.addWidget(self._anim_btn)
        spd_lbl = QLabel('Spd'); spd_lbl.setFont(self.infobar_font)
        anim_h.addWidget(spd_lbl); anim_h.addWidget(spd_slider, 1)
        lay.addWidget(anim_row)

        # Door toggles row
        door_row = QWidget(); door_h = QHBoxLayout(door_row)
        door_h.setContentsMargins(0,0,0,0); door_h.setSpacing(2)
        for dlabel, dname in [('LF','door_lf'),('RF','door_rf'),
                               ('LR','door_lr'),('RR','door_rr'),
                               ('Hood','bonnet'),('Boot','boot')]:
            db = QPushButton(dlabel); db.setFixedHeight(22)
            db.setFont(self.infobar_font); db.setToolTip(f'Toggle {dname}')
            db.clicked.connect(lambda _=False, n=dname: self.viewport.toggle_door(n))
            door_h.addWidget(db)
        lay.addWidget(door_row)

        lay.addWidget(_sep())

        #    Wheels: [Show Wheels] Steer [--x--] same row  
        self._wheels_btn = _tbtn("Wheels", "Show wheels at dummy positions",
                                 self._toggle_show_wheels, 'mesh', True, False)
        self._wheels_btn.setFixedWidth(70)
        steer_slider = QSlider(Qt.Orientation.Horizontal)
        steer_slider.setRange(-35, 35); steer_slider.setValue(0)
        steer_slider.setToolTip('Front wheel steering angle')
        steer_slider.valueChanged.connect(lambda v: self.viewport.set_wheel_heading(v))
        self._wheel_heading_slider = steer_slider
        wheel_row = QWidget(); wheel_h = QHBoxLayout(wheel_row)
        wheel_h.setContentsMargins(0,0,0,0); wheel_h.setSpacing(4)
        wheel_h.addWidget(self._wheels_btn)
        steer_lbl = QLabel('Steer'); steer_lbl.setFont(self.infobar_font)
        wheel_h.addWidget(steer_lbl); wheel_h.addWidget(steer_slider, 1)
        lay.addWidget(wheel_row)

        lay.addWidget(_sep())

        #    Model Info (collapsible)                       
        info_header = QWidget(); info_h = QHBoxLayout(info_header)
        info_h.setContentsMargins(0,0,0,0); info_h.setSpacing(4)
        info_toggle = _QTB(); info_toggle.setText("▸ Model Info")
        info_toggle.setFont(self.infobar_font); info_toggle.setFixedHeight(22)
        info_toggle.setCheckable(True); info_toggle.setChecked(False)
        info_toggle.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
        info_toggle.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        info_h.addWidget(info_toggle)
        lay.addWidget(info_header)

        self._info_lbl = QLabel('—')
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setVisible(False)
        self._info_lbl.setAlignment(Qt.AlignmentFlag.AlignTop)
        lay.addWidget(self._info_lbl)

        def _toggle_info(checked):  #vers 1
            self._info_lbl.setVisible(checked)
            info_toggle.setText(("▾" if checked else "▸") + " Model Info")
        info_toggle.toggled.connect(_toggle_info)

        lay.addWidget(_sep())

        #    Edit Buttons (icon + text, collapse to icon-only when narrow)   
        lay.addWidget(_lbl("Editing"))
        edit_actions = [
            ("CoM",     "Centre of Mass indicator",       self._toggle_com_indicator,  'target',   True),
            ("Dummies", "Show dummy frame positions",     self._toggle_dummy_overlay,  'mesh',     True),
            ("Susp",    "Suspension travel visualiser",  self._toggle_suspension_vis, 'bounds',   True),
            ("Seats",   "Show seat positions",            self._toggle_seat_overlay,   'view',     True),
            ("Bounds",  "Show bounding box",              self._toggle_bounds,         'wireframe',True),
            ("Export",  "Export DFF geometry",            self._export_dff,            'export',   False),
        ]
        edit_row1 = QWidget(); er1 = QHBoxLayout(edit_row1)
        edit_row1.setContentsMargins(0,0,0,0) if hasattr(edit_row1,'setContentsMargins') else None
        er1.setContentsMargins(0,0,0,0); er1.setSpacing(2)
        edit_row2 = QWidget(); er2 = QHBoxLayout(edit_row2)
        er2.setContentsMargins(0,0,0,0); er2.setSpacing(2)
        self._edit_btns = []
        for i, (label, tip, cb, iname, checkable) in enumerate(edit_actions):
            b = _QTB(); b.setFixedHeight(26)
            b.setFont(self.infobar_font); b.setToolTip(tip)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(14,14))
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            b.setText(label)
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            if checkable:
                b.setCheckable(True)
                b.toggled.connect(lambda checked, fn=cb: fn())
            else:
                b.clicked.connect(lambda _=False, fn=cb: fn())
            self._edit_btns.append(b)
            (er1 if i < 3 else er2).addWidget(b)
        lay.addWidget(edit_row1)
        lay.addWidget(edit_row2)

        # Collapse edit buttons to icon-only when panel is narrow
        def _on_panel_resize(event):  #vers 1
            narrow = panel.width() < 170
            style = Qt.ToolButtonStyle.ToolButtonIconOnly if narrow else Qt.ToolButtonStyle.ToolButtonTextBesideIcon
            for b in self._edit_btns:
                if b.icon() and not b.icon().isNull():
                    b.setToolButtonStyle(style)
            QFrame.resizeEvent(panel, event)
        panel.resizeEvent = _on_panel_resize

        lay.addStretch()
        return panel
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(160); panel.setMaximumWidth(200)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(*self.get_panel_margins())
        lay.setSpacing(4)

        ic = self._get_icon_color()
        def _icon(name, size=16):  #vers 1
            if not ICONS_AVAILABLE: return None
            try:
                fn = getattr(SVGIconFactory, name+'_icon', None)
                return fn(size, ic) if fn else None
            except Exception: return None

        from PyQt6.QtWidgets import QToolButton as _QTB

        def _btn(text, tip, cb, iname=None, checkable=False, checked=False): #Vers 1  #vers 1
            b = _QTB(); b.setFont(self.panel_font)
            b.setToolTip(tip); b.setFixedHeight(28)
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(16,16))
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            else:
                b.setText(text)
            if checkable:
                b.setCheckable(True); b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(cb)
            return b

        #  Render mode
        lbl_r = QLabel("Render"); lbl_r.setFont(self.panel_font)
        lay.addWidget(lbl_r)
        self._mode_group = QButtonGroup(self); self._mode_group.setExclusive(True)
        for label, mode, iname in [
            ("Wireframe", "wireframe", "wireframe"),
            ("Solid",     "solid",     "solid"),
            ("Textured",  "textured",  "texture"),
        ]:
            b = _btn(label, f"{mode.capitalize()} render mode",
                     lambda checked=False, m=mode: (self._set_mode(m) if checked else None),
                     iname, checkable=True, checked=(mode=='solid'))
            self._mode_group.addButton(b); lay.addWidget(b)

        lay.addSpacing(6)

        #  View toggles
        lbl_v = QLabel("View"); lbl_v.setFont(self.panel_font)
        lay.addWidget(lbl_v)
        self._cull_btn   = _btn("Backface Cull", "Toggle backface culling",  self.viewport.set_backface_cull, 'backface', True, True)
        self._grid_btn   = _btn("Grid",          "Toggle grid",              self.viewport.set_show_grid,    'grid',     True, True)
        self._prelit_btn = _btn("Pre-Lighting",  "Vertex pre-lighting",      self.viewport.set_prelight,     'shading',  True, False)
        lay.addWidget(self._cull_btn)
        lay.addWidget(self._grid_btn)
        lay.addWidget(self._prelit_btn)

        lay.addSpacing(6)

        #  Camera
        lbl_c = QLabel("Camera"); lbl_c.setFont(self.panel_font)
        lay.addWidget(lbl_c)
        lay.addWidget(_btn("Reset Camera", "Reset camera to default", self.viewport.reset_camera, 'reset'))
        lay.addWidget(_btn("Light Setup",  "Adjust light direction",  self._light_setup_dialog,   'light'))

        lay.addSpacing(6)

        #  Paint Colours
        lbl_p = QLabel("Paint"); lbl_p.setFont(self.panel_font)
        lay.addWidget(lbl_p)

        # Colour swatches row — primary + secondary
        swatch_row = QWidget()
        swatch_lay = QHBoxLayout(swatch_row)
        swatch_lay.setContentsMargins(0,0,0,0); swatch_lay.setSpacing(4)

        self._paint1_btn = QPushButton("Pri")
        self._paint2_btn = QPushButton("Sec")
        for btn in (self._paint1_btn, self._paint2_btn):
            btn.setFixedHeight(28)
            btn.setFont(self.infobar_font)
        self._paint1_btn.clicked.connect(self._pick_paint1)
        self._paint2_btn.clicked.connect(self._pick_paint2)
        swatch_lay.addWidget(self._paint1_btn)
        swatch_lay.addWidget(self._paint2_btn)
        lay.addWidget(swatch_row)
        self._update_paint_btns()

        # Carcols swatches — populated when DFF loads
        self._carcols_widget = QWidget()
        self._carcols_lay = QVBoxLayout(self._carcols_widget)
        self._carcols_lay.setContentsMargins(0,0,0,2); self._carcols_lay.setSpacing(2)
        lay.addWidget(self._carcols_widget)

        lay.addSpacing(4)

        #  Assembly
        lbl_a = QLabel("Assembly"); lbl_a.setFont(self.panel_font)
        lay.addWidget(lbl_a)
        self._assemble_btn = _btn("All Parts", "Show all parts assembled at world positions", self._toggle_assembly_mode, None, True, False)
        self._damage_btn   = _btn("Damage",    "Show damaged state (_dam parts)",             self._toggle_damage_mode,   None, True, False)
        self._lod_btn      = _btn("Show LOD",  "Show LOD meshes (_vlo parts)",                self._toggle_lod_mode,      None, True, False)
        lay.addWidget(self._assemble_btn)
        lay.addWidget(self._damage_btn)
        lay.addWidget(self._lod_btn)

        lay.addSpacing(6)

        #  Animate
        lbl_an = QLabel('Animate'); lbl_an.setFont(self.panel_font)
        lay.addWidget(lbl_an)
        self._anim_btn = _btn('Play', 'Start/stop animation',
            lambda checked: self.viewport.set_animation(checked), None, True, False)
        lay.addWidget(self._anim_btn)

        # Speed
        from PyQt6.QtWidgets import QSlider
        spd_row = QHBoxLayout()
        spd_lbl = QLabel('Speed'); spd_lbl.setFont(self.infobar_font)
        self._anim_speed_slider = QSlider(Qt.Orientation.Horizontal)
        self._anim_speed_slider.setRange(1, 30)
        self._anim_speed_slider.setValue(10)
        self._anim_speed_slider.setToolTip('Animation speed')
        self._anim_speed_slider.valueChanged.connect(
            lambda v: self.viewport.set_animation_speed(v / 10.0))
        spd_row.addWidget(spd_lbl); spd_row.addWidget(self._anim_speed_slider, 1)
        lay.addLayout(spd_row)

        # Door toggles
        door_row = QHBoxLayout(); door_row.setSpacing(2)
        for dlabel, dname in [('LF','door_lf'),('RF','door_rf'),
                               ('LR','door_lr'),('RR','door_rr')]:
            db = QPushButton(dlabel); db.setFixedHeight(22); db.setFixedWidth(28)
            db.setFont(self.infobar_font)
            db.setToolTip(f'Toggle {dname}')
            db.clicked.connect(lambda _=False, n=dname: self.viewport.toggle_door(n))
            door_row.addWidget(db)
        door_row.addStretch()
        # Bonnet / Boot
        for dlabel, dname in [('Hood','bonnet'),('Boot','boot')]:
            db = QPushButton(dlabel); db.setFixedHeight(22)
            db.setFont(self.infobar_font); db.setToolTip(f'Toggle {dname}')
            db.clicked.connect(lambda _=False, n=dname: self.viewport.toggle_door(n))
            door_row.addWidget(db)
        lay.addLayout(door_row)

        lay.addSpacing(4)

        #  Wheels
        lbl_w = QLabel('Wheels'); lbl_w.setFont(self.panel_font)
        lay.addWidget(lbl_w)
        self._wheels_btn = _btn("Show Wheels", "Load and show wheels at dummy positions",
                                self._toggle_show_wheels, None, True, False)
        lay.addWidget(self._wheels_btn)

        from PyQt6.QtWidgets import QSlider
        self._wheel_heading_slider = QSlider(Qt.Orientation.Horizontal)
        self._wheel_heading_slider.setRange(-35, 35)
        self._wheel_heading_slider.setValue(0)
        self._wheel_heading_slider.setToolTip('Front wheel heading angle')
        self._wheel_heading_slider.valueChanged.connect(
            lambda v: self.viewport.set_wheel_heading(v))
        heading_row = QHBoxLayout()
        heading_lbl = QLabel('Steer'); heading_lbl.setFont(self.infobar_font)
        heading_row.addWidget(heading_lbl)
        heading_row.addWidget(self._wheel_heading_slider, 1)
        lay.addLayout(heading_row)

        lay.addSpacing(6)

        #  Model Info
        lbl_i = QLabel('Model Info'); lbl_i.setFont(self.panel_font)
        lay.addWidget(lbl_i)
        self._info_lbl = QLabel('—')
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setAlignment(Qt.AlignmentFlag.AlignTop)
        lay.addWidget(self._info_lbl)

        lay.addStretch()
        return panel



    def _populate_sidebar(self): #Vers 1  #vers 1
        """Build the 2-col icon grid in the right sidebar.
        Override to change or extend the tool set.
        """
        sl = self._sidebar_layout
        ic = self._get_icon_color()
        BTN = 36

        def _nb(icon_fn, tip, slot, checkable=False): #vers 1
            b = QToolButton(); b.setFixedSize(BTN, BTN)
            try: b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
            except Exception: b.setText(tip[:2])
            b.setToolTip(tip); b.setCheckable(checkable)
            b.clicked.connect(slot); return b

        def _row(*btns): #vers 1
            row = QHBoxLayout()
            row.setSpacing(2); row.setContentsMargins(0, 0, 0, 0)
            for b in btns: row.addWidget(b)
            if len(btns) == 1: row.addStretch()
            sl.addLayout(row)

        def _sep(): #vers 1
            s = QFrame(); s.setFrameShape(QFrame.Shape.HLine)
            sl.addSpacing(2); sl.addWidget(s); sl.addSpacing(2)

        def _tool(icon_fn, tip, name): #vers 1
            b = _nb(icon_fn, tip,
                    lambda checked=False, t=name: self._set_active_tool(t),
                    checkable=True)
            self._draw_btns[name] = b; return b

        # Row 1-2: View controls
        _row(_nb("zoom_in_icon",  "Zoom in  (+)",      lambda: self._zoom(1.25)),
             _nb("zoom_out_icon", "Zoom out  (-)",     lambda: self._zoom(0.8)))
        _row(_nb("fit_grid_icon", "Fit  Ctrl+0",       self._fit),
             _nb("locate_icon",   "Jump to selection", self._jump))
        _sep()

        # Rows 3-6: Draw tools (2 per row)
        _row(_tool("paint_icon",     "Pencil (P)",           "pencil"),
             _tool("fill_icon",      "Flood fill (F)",       "fill"))
        _row(_tool("line_icon",      "Line (L)",             "line"),
             _tool("rect_icon",      "Rect outline (R)",     "rect"))
        _row(_tool("rect_fill_icon", "Filled rect (Shift+R)","rect_fill"),
             _tool("dropper_icon",   "Colour picker (K)",    "picker"))
        _row(_tool("scissors_icon",  "Cut (X)",              "cut"),
             _tool("paste_brush_icon","Paste (V)",           "paste"))
        _row(_tool("zoom_in_icon",   "Zoom tool (Z)",        "zoom"),
             _nb("search_icon",      "Open in editor tab",
                 lambda: self._on_toolbar_action("edit")))
        _sep()

        # Rows 7-8: Transform tools
        _row(_nb("rotate_cw_icon",  "Rotate +90°",
                 lambda: self._on_toolbar_action("rotate_cw")),
             _nb("rotate_ccw_icon", "Rotate -90°",
                 lambda: self._on_toolbar_action("rotate_ccw")))
        _row(_nb("flip_horz_icon",  "Flip Horizontal",
                 lambda: self._on_toolbar_action("flip_h")),
             _nb("flip_vert_icon",  "Flip Vertical",
                 lambda: self._on_toolbar_action("flip_v")))

        if "pencil" in self._draw_btns:
            self._draw_btns["pencil"].setChecked(True)
            self._active_tool = "pencil"


    # - status bar
    def _create_status_bar(self): #vers 2
        bar = QFrame()
        bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        bar.setFixedHeight(self.statusheight)
        hl = QHBoxLayout(bar)
        hl.setContentsMargins(*self.get_tab_margins())
        self.status_label = QLabel("No model loaded")
        self.status_label.setFont(self.infobar_font)
        hl.addWidget(self.status_label, 1)
        from PyQt6.QtWidgets import QProgressBar
        self._progress = QProgressBar()
        self._progress.setFixedWidth(120)
        self._progress.setFixedHeight(14)
        self._progress.setRange(0, 0)   # indeterminate / pulsing
        self._progress.setVisible(False)
        self._progress.setTextVisible(False)
        hl.addWidget(self._progress)
        return bar


    # - left panel — geometry + texture lists
    def _make_section_header(self, title, search_cb=None): #vers 1
        """Collapsible section header row with optional search box."""
        row = QWidget(); rl = QHBoxLayout(row); rl.setContentsMargins(0,0,0,0); rl.setSpacing(2)
        lbl = QLabel(title); lbl.setFont(self.panel_font); rl.addWidget(lbl, 1)
        if search_cb:
            from PyQt6.QtWidgets import QLineEdit
            se = QLineEdit(); se.setPlaceholderText("Filter…")
            se.setFixedHeight(20); se.setMaximumWidth(90)
            se.textChanged.connect(search_cb)
            rl.addWidget(se)
        return row

    def _filter_img_list(self, text): #vers 1
        ft = text.lower()
        for i in range(self._img_list.count()):
            item = self._img_list.item(i)
            item.setHidden(bool(ft) and ft not in item.text().lower())

    def _on_img_entry_dclicked(self, item): #vers 2
        """Double-click IMG entry — extract and load DFF + auto TXD with progress."""
        entry = item.data(Qt.ItemDataRole.UserRole)
        img   = getattr(self, '_current_img', None)
        if not entry or not img:
            return
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtCore import Qt as _Qt
        name = entry.name
        stem0, ext0 = os.path.splitext(name)
        display_name = re.sub(r'_[a-z0-9]{6,12}$', '', stem0, flags=re.IGNORECASE) + ext0
        try:
            import tempfile
            # Show busy cursor + progress bar immediately
            QApplication.setOverrideCursor(_Qt.CursorShape.WaitCursor)
            self._show_progress(True)
            self._set_status(f"Loading {display_name}…")
            QApplication.processEvents()

            # Step 1: Extract DFF
            self._set_status(f"Extracting {name}…")
            QApplication.processEvents()
            data = img.read_entry_data(entry)
            if not data:
                return
            tmp_dir  = tempfile.mkdtemp()
            dff_path = os.path.join(tmp_dir, name)
            with open(dff_path,'wb') as f: f.write(data)

            # Step 2: Parse + load DFF
            self._set_status(f"Parsing {name}…")
            QApplication.processEvents()
            self.load_dff(dff_path)
            QApplication.processEvents()

            # Step 3: Find + extract TXD
            stem = os.path.splitext(name)[0].lower()
            txd_entry = next((e for e in img.entries if e.name.lower() == stem + '.txd'), None)
            if txd_entry:
                self._set_status(f"Loading textures for {stem}…")
                QApplication.processEvents()
                txd_data = img.read_entry_data(txd_entry)
                if txd_data:
                    txd_path = os.path.join(tmp_dir, txd_entry.name)
                    with open(txd_path,'wb') as f: f.write(txd_data)
                    self.load_txd(txd_path)
                    QApplication.processEvents()

        except Exception as ex:
            self._set_status(f"Load error: {ex}")
        finally:
            self._show_progress(False)
            QApplication.restoreOverrideCursor()


    def _light_setup_dialog(self): #vers 2
        """Light direction, intensity and vehicle paint colour dialog."""
        dlg = QDialog(self); dlg.setWindowTitle("Light & Paint Setup"); dlg.resize(360, 380)
        lay = QVBoxLayout(dlg)
        form = QFormLayout()

        vp = self.viewport

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
        close_btn = QPushButton("Close"); close_btn.clicked.connect(dlg.accept)
        btn_row.addWidget(reset_btn); btn_row.addStretch(); btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)
        dlg.exec()


    def _populate_frame_tree(self): #vers 1
        tree = getattr(self,'_frame_tree',None)
        if not tree or not self._dff_model: return
        from PyQt6.QtWidgets import QTreeWidgetItem
        from PyQt6.QtCore import Qt
        tree.blockSignals(True)
        tree.clear()
        frames = self._dff_model.frames
        items = {}
        # Build tree from parent indices
        for i,f in enumerate(frames):
            name = f.name or f'frame_{i}'
            item = QTreeWidgetItem([name])
            item.setFlags(Qt.ItemFlag.ItemIsUserCheckable |
                          Qt.ItemFlag.ItemIsEnabled |
                          Qt.ItemFlag.ItemIsSelectable)
            item.setCheckState(0, Qt.CheckState.Checked)
            item.setData(0, Qt.ItemDataRole.UserRole, name.lower())
            items[i] = item
        for i,f in enumerate(frames):
            p = f.parent_index
            if p >= 0 and p != i and p in items:
                items[p].addChild(items[i])
            elif p == -1 or p == i:
                tree.addTopLevelItem(items[i])
        tree.expandAll()
        tree.blockSignals(False)

    def _on_frame_visibility_changed(self, item, col): #vers 2
        from PyQt6.QtCore import Qt
        name = item.data(0, Qt.ItemDataRole.UserRole) or ''
        checked = item.checkState(0) == Qt.CheckState.Checked
        print(f"[FrameTree] {name!r} checked={checked}")
        if not hasattr(self.viewport, '_hidden_frames'):
            self.viewport._hidden_frames = set()
        if item.checkState(0) == Qt.CheckState.Checked:
            self.viewport._hidden_frames.discard(name)
        else:
            self.viewport._hidden_frames.add(name)
        # Always rebuild — hidden_frames filter is applied in load_all_geometries
        m = getattr(self, '_dff_model', None)
        if m and m.frames and m.atomics:
            vp = self.viewport
            # Save camera state — don't reset view on visibility toggle
            cam = (vp._yaw, vp._pitch, vp._dist, vp._pan_x, vp._pan_y)
            vp.load_all_geometries(
                m.geometries, [g.materials for g in m.geometries],
                m.frames, m.atomics,
                damaged=getattr(self, '_damage_mode', False))
            vp._yaw, vp._pitch, vp._dist, vp._pan_x, vp._pan_y = cam
        self.viewport.update()

    def _populate_geom_list(self): #vers 1

        self._geom_list.clear()
        if not self._dff_model: return
        for i, g in enumerate(self._dff_model.geometries):
            name = f"geometry_{i}"
            a = next((a for a in self._dff_model.atomics if a.geometry_index==i), None)
            if a and a.frame_index < len(self._dff_model.frames):
                fn = self._dff_model.frames[a.frame_index].name
                if fn: name = fn
            self._geom_list.addItem(f"[{i}] {name}  {len(g.vertices)}v {len(g.triangles)}t")


    def _on_geom_selected(self, row: int): #vers 5
        if not self._dff_model or row < 0 or row >= len(self._dff_model.geometries): return
        self._current_geom = row
        m = self._dff_model
        g = m.geometries[row]
        damaged = getattr(self, '_damage_mode', False)
        def _do_load():  #vers 1
            if m.frames and m.atomics:
                self.viewport.load_all_geometries(
                    m.geometries, [geom.materials for geom in m.geometries],
                    m.frames, m.atomics, damaged=damaged)
                if not getattr(self.viewport, '_all_geoms', None) and not self.viewport._vertices:
                    self.viewport.load_geometry(g, g.materials)
            else:
                self.viewport.load_geometry(g, g.materials)
            self.viewport.update()
        from PyQt6.QtCore import QTimer
        # Defer so GL context is initialized before first render
        if hasattr(self.viewport, 'isValid') and not self.viewport.isValid():
            QTimer.singleShot(150, _do_load)
        else:
            _do_load()
        has_prelit = bool(g.colors)
        self._prelit_btn.setEnabled(has_prelit)
        self._info_lbl.setText(
            f"Verts:  {len(g.vertices)}\n"
            f"Tris:   {len(g.triangles)}\n"
            f"Mats:   {len(g.materials)}\n"
            f"UVs:    {len(g.uv_layers)}\n"
            f"Norms:  {'yes' if g.normals else 'no'}\n"
            f"PreLit: {'yes' if has_prelit else 'no'}")

    def _set_status(self, msg: str): #vers 1
        self.status_label.setText(msg)
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"[ModelViewer] {msg}")
