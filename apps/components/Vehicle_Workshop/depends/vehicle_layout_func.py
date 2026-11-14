#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_layout_func.py - Version: 9
# X-Seti - November 12 2026 - IMG Factory 1.6 - Vehicle Workshop layout

"""
Vehicle Model dock: IMG Entries, Geometries, Textures, Frame Visibility tabs.
"""

##class VehicleLayoutMixin: -
# _create_vehicle_model_dock
# _create_vehicle_tools_dock
# _filter_img_list
# _make_section_header
# _on_frame_visibility_changed
# _on_geom_selected
# _populate_frame_visibility
# _populate_geom_list

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtWidgets import (
    QButtonGroup, QDockWidget, QFrame, QHBoxLayout, QLabel, QLineEdit, QListWidget,
    QPushButton, QScrollArea, QSizePolicy, QSlider, QToolButton, QTreeWidget,
    QVBoxLayout, QWidget)

from apps.methods.dff_viewport import VehicleViewport
from apps.components.Vehicle_Workshop.depends.vehicle_common_func import (
    ICONS_AVAILABLE, SVGIconFactory)


class VehicleLayoutMixin:
    """Vehicle Model dock. Mixed into VehicleWorkshop."""

    def _create_vehicle_model_dock(self): #vers 1
        """IMG Entries / Geometries / Textures / Frame Visibility - one dock, tabbed."""
        from apps.components.Vehicle_Workshop.vehicle_workshop import CurrentOnlyTabWidget
        from apps.methods.fold_widgets import ButtonRow
        tabs = CurrentOnlyTabWidget()
        ic = self._get_icon_color()

        # IMG Entries
        img_tab = QWidget(); il = QVBoxLayout(img_tab); il.setContentsMargins(4, 4, 4, 4)
        il.addWidget(self._make_section_header("", self._filter_img_list))
        self._img_list = QListWidget()
        self._img_list.setFont(self.panel_font); self._img_list.setAlternatingRowColors(True)
        self._img_list.itemClicked.connect(self._on_img_entry_selected)
        self._img_list.itemActivated.connect(self._on_img_entry_selected)
        il.addWidget(self._img_list, 1)
        btns = []
        for label, tip, cb in (('Open DFF', 'Open DFF model', self._vw_pick_dff),
                               ('Open TXD', 'Open TXD textures', self._vw_pick_txd)):
            b = QToolButton(); b.setFixedHeight(28); b.setFont(self.panel_font); b.setToolTip(tip)
            b.setText(label)
            b.setIcon(SVGIconFactory.open_icon(14, ic)); b.setIconSize(QSize(14, 14))
            b.clicked.connect(cb)
            btns.append(b)
        self._vw_open_dff_btn, self._vw_open_txd_btn = btns
        il.addWidget(ButtonRow(btns))
        tabs.addTab(img_tab, "IMG Entries")

        # Geometries
        geom_tab = QWidget(); gl = QVBoxLayout(geom_tab); gl.setContentsMargins(4, 4, 4, 4)
        self._geom_list = QListWidget()
        self._geom_list.setFont(self.panel_font); self._geom_list.setAlternatingRowColors(True)
        self._geom_list.currentRowChanged.connect(self._on_geom_selected)
        gl.addWidget(self._geom_list)
        tabs.addTab(geom_tab, "Geometries")

        # Textures
        tex_tab = QWidget(); tl = QVBoxLayout(tex_tab); tl.setContentsMargins(4, 4, 4, 4)
        self._tex_list = QListWidget()
        self._tex_list.setFont(self.panel_font); self._tex_list.setAlternatingRowColors(True)
        self._tex_list.setIconSize(QSize(32, 32))
        self._tex_list.setViewMode(QListWidget.ViewMode.ListMode)
        tl.addWidget(self._tex_list)
        tl.addWidget(self._skin_bar())
        tabs.addTab(tex_tab, "Textures")

        # Frame Visibility (own tree - ModelWorkshop's own _frame_tree is a
        # different, 3-column selection tree, not this checkbox one)
        fv_tab = QWidget(); fl = QVBoxLayout(fv_tab); fl.setContentsMargins(4, 4, 4, 4)
        self._vw_frame_tree = QTreeWidget()
        self._vw_frame_tree.setFont(self.panel_font); self._vw_frame_tree.setAlternatingRowColors(True)
        self._vw_frame_tree.setHeaderHidden(True)
        self._vw_frame_tree.setColumnCount(1)
        self._vw_frame_tree.itemChanged.connect(self._on_frame_visibility_changed)
        fl.addWidget(self._vw_frame_tree)
        tabs.addTab(fv_tab, "Frame Visibility")

        dock = QDockWidget("Vehicle Model", self)
        dock.setObjectName("VehicleModelDock")
        dock.setWidget(tabs)
        self._vehicle_model_dock = dock
        return dock

    def _create_vehicle_tools_dock(self): #vers 1
        """Render, camera, paint, assembly, animate, doors, wheels, info, editing - scrollable dock."""
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(60)                        # dock separator sets the width
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(4, 4, 4, 4)
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
            s.setFixedHeight(1); s.setStyleSheet("background:palette(mid);")
            return s

        def _lbl(text):  #vers 1
            l = QLabel(text); l.setFont(self.panel_font); return l

        # Pre-lighting only; render mode, cull and grid live in the ribbon and viewport chips
        self._prelit_btn = _tbtn("PreLit", "Pre-lighting", self.viewport.set_prelight, 'brighten', True, False)
        lay.addWidget(_row(self._prelit_btn, 'stretch'))
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
                                     self._toggle_show_wheels, 'col_workshop', True, True)
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
        scroll = QScrollArea(); scroll.setWidgetResizable(True)       # shrinks with the dock
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setWidget(panel)
        dock = QDockWidget("Vehicle Tools", self)
        dock.setObjectName("VehicleToolsDock")
        dock.setWidget(scroll)
        self._vehicle_tools_dock = dock
        return dock

    def _make_section_header(self, title, search_cb=None): #vers 2
        """Collapsible section header row with optional search box."""
        row = QWidget(); rl = QHBoxLayout(row); rl.setContentsMargins(0, 0, 0, 0); rl.setSpacing(2)
        if title:
            lbl = QLabel(title); lbl.setFont(self.panel_font); rl.addWidget(lbl, 1)
        if search_cb:
            se = QLineEdit(); se.setPlaceholderText("Filter…")
            se.setFixedHeight(20)
            if title:
                se.setMaximumWidth(90)
            se.textChanged.connect(search_cb)
            rl.addWidget(se)
        return row

    def _filter_img_list(self, text): #vers 1
        ft = text.lower()
        for i in range(self._img_list.count()):
            item = self._img_list.item(i)
            item.setHidden(bool(ft) and ft not in item.text().lower())

    def _populate_frame_visibility(self): #vers 3
        tree = getattr(self, '_vw_frame_tree', None)
        if not tree or not self._dff_model:
            return
        from PyQt6.QtWidgets import QTreeWidgetItem
        tree.blockSignals(True)
        tree.clear()
        frames = self._dff_model.frames
        for i, f in enumerate(frames):
            name = f.name or f'frame_{i}'
            item = QTreeWidgetItem([name])
            item.setFlags(Qt.ItemFlag.ItemIsUserCheckable |
                          Qt.ItemFlag.ItemIsEnabled |
                          Qt.ItemFlag.ItemIsSelectable)
            item.setCheckState(0, Qt.CheckState.Checked)
            item.setData(0, Qt.ItemDataRole.UserRole, name)
            tree.addTopLevelItem(item)
        tree.blockSignals(False)

    def _on_frame_visibility_changed(self, item, col): #vers 2
        name = item.data(0, Qt.ItemDataRole.UserRole) or ''
        if not hasattr(self.viewport, '_hidden_frames'):
            self.viewport._hidden_frames = set()
        if item.checkState(0) == Qt.CheckState.Checked:
            self.viewport._hidden_frames.discard(name)
        else:
            self.viewport._hidden_frames.add(name)
        m = getattr(self, '_dff_model', None)
        if m and m.frames and m.atomics:
            vp = self.viewport
            vp.load_all_geometries(m.geometries, [g.materials for g in m.geometries],
                                   m.frames, m.atomics, damaged=getattr(self, '_damage_mode', False))
            vp.update()

    def _populate_geom_list(self): #vers 1
        self._geom_list.clear()
        if not self._dff_model:
            return
        for i, g in enumerate(self._dff_model.geometries):
            name = f"geometry_{i}"
            a = next((a for a in self._dff_model.atomics if a.geometry_index == i), None)
            if a and a.frame_index < len(self._dff_model.frames):
                fn = self._dff_model.frames[a.frame_index].name
                if fn:
                    name = fn
            self._geom_list.addItem(f"[{i}] {name}  {len(g.vertices)}v {len(g.triangles)}t")

    def _on_geom_selected(self, row: int): #vers 5
        if not self._dff_model or row < 0 or row >= len(self._dff_model.geometries):
            return
        self._current_geom = row
        m = self._dff_model
        g = m.geometries[row]
        damaged = getattr(self, '_damage_mode', False)
        if m.frames and m.atomics:
            self.viewport.load_all_geometries(
                m.geometries, [geom.materials for geom in m.geometries],
                m.frames, m.atomics, damaged=damaged)
            if not getattr(self.viewport, '_all_geoms', None) and not self.viewport._vertices:
                self.viewport.load_geometry(g, g.materials)
        else:
            self.viewport.load_geometry(g, g.materials)
        self.viewport.update()
