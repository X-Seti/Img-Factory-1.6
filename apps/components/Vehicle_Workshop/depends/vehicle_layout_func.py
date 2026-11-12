#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_layout_func.py - Version: 6
# X-Seti - October 14 2026 - IMG Factory 1.6 - Vehicle Workshop layout

"""
Panels, sidebar, viewport and list layout.
"""

##class VehicleLayoutMixin: -
# _create_left_panel
# _create_right_panel
# _create_status_bar
# _filter_img_list
# _make_section_header
# _on_frame_visibility_changed
# _on_geom_selected
# _populate_frame_tree
# _populate_geom_list
# _populate_sidebar
# _set_status
# get_tab_margins

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


    def _create_left_panel(self): #vers 6
        """Asset sections as separate docks: [(title, widget)]."""
        from PyQt6.QtWidgets import QTreeWidget, QToolButton
        from apps.methods.fold_widgets import ButtonRow

        def _sec():
            w = QFrame(); w.setFrameStyle(QFrame.Shape.StyledPanel)
            lay = QVBoxLayout(w); lay.setContentsMargins(*self.get_panel_margins()); lay.setSpacing(2)
            return w, lay

        # - IMG Entries: filter, list, Open DFF / Open TXD
        img_sec, img_lay = _sec()
        img_lay.addWidget(self._make_section_header("", self._filter_img_list))
        self._img_list = QListWidget()
        self._img_list.setFont(self.panel_font)
        self._img_list.itemClicked.connect(self._on_img_entry_selected)     # click or Enter loads
        self._img_list.itemActivated.connect(self._on_img_entry_selected)
        img_lay.addWidget(self._img_list, 1)
        ic = self._get_icon_color()
        btns = []
        for label, tip, cb in (('Open DFF', 'Open DFF model', self._vw_pick_dff),
                               ('Open TXD', 'Open TXD textures', self._vw_pick_txd)):
            b = QToolButton(); b.setFixedHeight(28); b.setFont(self.panel_font); b.setToolTip(tip)
            b.setText(label)
            b.setIcon(SVGIconFactory.open_icon(14, ic)); b.setIconSize(QSize(14, 14))
            b.clicked.connect(cb)
            btns.append(b)
        self._vw_open_dff_btn, self._vw_open_txd_btn = btns
        img_lay.addWidget(ButtonRow(btns))

        # - Geometries
        geom_sec, gl = _sec()
        self._geom_list = QListWidget()
        self._geom_list.setFont(self.panel_font)
        self._geom_list.currentRowChanged.connect(self._on_geom_selected)
        gl.addWidget(self._geom_list)

        # - Textures with thumbnails
        tex_sec, tl = _sec()
        self._tex_list = QListWidget()
        self._tex_list.setFont(self.panel_font)
        self._tex_list.setIconSize(QSize(32,32))
        self._tex_list.setViewMode(QListWidget.ViewMode.ListMode)
        tl.addWidget(self._tex_list)
        tl.addWidget(self._skin_bar())                       # DP5, replace, add, export, save

        # - Frame Hierarchy with visibility checkboxes
        frame_sec, fl = _sec()
        self._frame_tree = QTreeWidget()
        self._frame_tree.setFont(self.panel_font)
        self._frame_tree.setHeaderHidden(True)
        self._frame_tree.setColumnCount(1)
        self._frame_tree.itemChanged.connect(self._on_frame_visibility_changed)
        fl.addWidget(self._frame_tree)

        return [("IMG Entries", img_sec), ("Geometries", geom_sec),
                ("Textures", tex_sec), ("Frame Hierarchy", frame_sec)]

    def _create_right_panel(self): #vers 7
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(140)                       # dock separator sets the width
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
    def _make_section_header(self, title, search_cb=None): #vers 2
        """Collapsible section header row with optional search box."""
        row = QWidget(); rl = QHBoxLayout(row); rl.setContentsMargins(0,0,0,0); rl.setSpacing(2)
        if title:
            lbl = QLabel(title); lbl.setFont(self.panel_font); rl.addWidget(lbl, 1)
        if search_cb:
            from PyQt6.QtWidgets import QLineEdit
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
