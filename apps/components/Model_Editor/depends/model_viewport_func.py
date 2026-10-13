#this belongs in apps/components/Model_Editor/depends/model_viewport_func.py - Version: 6
# X-Seti - October 12 2026 - IMG Factory 1.6 - Model Workshop viewport

"""
Viewport shading, lights, render and selection modes, 4-pane layout.
"""

##class ModelViewportMixin: -
# _assign_quad_pane_view
# _create_quad_viewport
# _cycle_view_render_style
# eventFilter
# _live_viewports
# _load_quad_layout
# _load_viewport_light_settings
# _open_gl_viewer
# _open_light_setup_dialog
# _open_render_settings_dialog
# _save_quad_layout
# _set_select_mode
# _show_quad_pane_menu
# _show_shaders_dialog
# _sync_quad_from_main
# _sync_selection_to_other_viewports
# _toggle_backface_cull
# _toggle_front_only_paint
# _toggle_pane_maximize
# _toggle_quad_view
# _toggle_upscale_native
# _toggle_viewport_shading
# _update_select_mode_availability

import os
from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QFont
from PyQt6.QtWidgets import QComboBox, QDialog, QFrame, QHBoxLayout, QLabel, QMenu, QPushButton, QSplitter, QVBoxLayout
from apps.methods.dff_viewport import DFFViewport
from apps.methods.img_factory_settings import get_user_config_dir

class ModelViewportMixin: #vers 1
    """Viewport shading, lights, render and selection modes, 4-pane layout."""




    def _cycle_view_render_style(self): #vers 3
        """Cycle viewport render: wireframe -> solid -> textured.
        Applied to every visible viewport (single or all 4 quad panes)."""
        pw = getattr(self, 'preview_widget', None)
        if not pw: return
        modes = ['wireframe', 'solid', 'textured']
        cur = getattr(pw, '_mode', 'solid')
        nxt = modes[(modes.index(cur) + 1) % len(modes)] if cur in modes else 'solid'
        for vp in self._live_viewports():
            if hasattr(vp, 'set_render_mode'):
                vp.set_render_mode(nxt)
            else:
                vp._mode = nxt
                vp.update()






    def _open_render_settings_dialog(self): #vers 2
        """Render & background settings dialog."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel,
                                     QComboBox, QPushButton, QColorDialog,
                                     QGroupBox, QDialogButtonBox)
        from PyQt6.QtGui import QColor

        pw = getattr(self, 'preview_widget', None)
        if not pw: return

        dlg = QDialog(self)
        dlg.setWindowTitle("Render Settings")
        dlg.setMinimumWidth(360)
        lay = QVBoxLayout(dlg)

        # Object rendering style
        style_grp = QGroupBox("Object Rendering")
        sg = QHBoxLayout(style_grp)
        sg.addWidget(QLabel("Style:"))
        style_combo = QComboBox()
        style_combo.addItems(["Wireframe", "Solid", "Textured"])
        mapping = {"wireframe": "Wireframe", "solid": "Solid", "textured": "Textured"}
        cur_mode = getattr(pw, '_mode', 'solid')
        style_combo.setCurrentText(mapping.get(cur_mode, "Solid"))
        sg.addWidget(style_combo)
        lay.addWidget(style_grp)

        # Background
        bg_grp = QGroupBox("Background")
        bg = QHBoxLayout(bg_grp)
        override = getattr(pw, '_bg_color_override', None)
        r, g, b = override if override is not None else (
            pw._get_ui_color('bg_panel').red(),
            pw._get_ui_color('bg_panel').green(),
            pw._get_ui_color('bg_panel').blue())
        bg_preview = QPushButton("  ")
        bg_preview.setFixedSize(60, 28)
        bg_preview.setStyleSheet(f"background-color: rgb({r},{g},{b});")
        def _pick_bg():  #vers 1
            c = QColorDialog.getColor(QColor(r, g, b), dlg, "Background Colour")
            if c.isValid():
                bg_preview.setStyleSheet(f"background-color: {c.name()};")
                bg_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        bg_preview.clicked.connect(_pick_bg)
        bg.addWidget(QLabel("Colour:"))
        bg.addWidget(bg_preview)
        reset_btn = QPushButton("Use Theme")
        reset_btn.clicked.connect(lambda: bg_preview.setProperty("reset_to_theme", True))
        bg.addWidget(reset_btn)
        lay.addWidget(bg_grp)

        # Buttons
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                QDialogButtonBox.StandardButton.Cancel)
        btns.rejected.connect(dlg.reject)
        def _apply():  #vers 1
            s = style_combo.currentText()
            rev = {"Wireframe": "wireframe", "Solid": "solid", "Textured": "textured"}
            pw.set_render_mode(rev.get(s, "solid"))
            if bg_preview.property("reset_to_theme"):
                pw.set_checkerboard_background()
            else:
                chosen = bg_preview.property("chosen")
                if chosen:
                    pw.set_background_color(chosen)
            dlg.accept()
        btns.accepted.connect(_apply)
        lay.addWidget(btns)
        dlg.exec()

    #Left side vertical panel



    # - DFF mode toolbar


    def _toggle_viewport_shading(self, enabled: bool): #vers 1
        """Toggle Lambertian shading on/off in the viewport."""
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp._shading_enabled = enabled
            vp.update()
        btn = getattr(self, '_shading_btn', None)
        if btn:
            try:
                from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
                ic = self._get_icon_color()
                btn.setIcon(
                    _SVG.backface_icon(size=16, color=ic)
                    if enabled else
                    _SVG.shading_off_icon(size=16, color=ic))
            except Exception:
                btn.setText("S" if enabled else "F")
        self._set_status(
            f"Shading: {'ON (Lambertian)' if enabled else 'OFF (flat)'}")

    def _open_light_setup_dialog(self): #vers 3
        """Viewport light setup — visual position picker + sliders."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
            QLabel, QSlider, QPushButton, QDialogButtonBox,
            QGroupBox, QCheckBox, QFrame)
        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QRadialGradient
        from PyQt6.QtCore import Qt as _Qt
        import json, math, os

        vp = getattr(self, 'preview_widget', None)
        _dir   = getattr(self, '_vp_light_dir',      (0.5, 0.5, 0.8, 0.0))
        _amb   = getattr(self, '_vp_light_ambient',  0.30)
        _int   = getattr(self, '_vp_light_intensity', 1.0)
        _sb    = getattr(self, '_shading_btn', None)
        _shade_on = _sb.isChecked() if _sb else True

        # Internal state (az=azimuth 0-360, el=elevation 0-90)
        _az = [math.degrees(math.atan2(_dir[0], _dir[1])) % 360]
        _el = [max(0, min(90, math.degrees(math.asin(max(-1, min(1, _dir[2]))))))]

        dlg = QDialog(self)
        dlg.setWindowTitle("Light Setup")
        dlg.setFixedWidth(360)
        root = QVBoxLayout(dlg); root.setSpacing(6)

        # - Shading toggle
        shade_cb = QCheckBox("Enable shading")
        shade_cb.setChecked(_shade_on); root.addWidget(shade_cb)
        sep = QFrame(); sep.setFrameShape(QFrame.Shape.HLine)
        sep.setStyleSheet("color:palette(mid);"); root.addWidget(sep)

        # - Visual position picker
        # Top-down circle = azimuth, height on circle = elevation
        pos_grp = QGroupBox("Light Position")
        pos_vlay = QVBoxLayout(pos_grp)

        class _PosPicker(QFrame):
            def __init__(self):  #vers 1
                super().__init__()
                self.setFixedSize(200, 200)
                self.setCursor(_Qt.CursorShape.CrossCursor)
                self.az = _az[0]; self.el = _el[0]
                self.on_change = None
            def _az_el_to_xy(self, az, el):  #vers 1
                r  = 88 * (1.0 - el/90.0)
                ar = math.radians(az)
                return (100 + r*math.sin(ar), 100 - r*math.cos(ar))
            def _xy_to_az_el(self, x, y):  #vers 1
                dx, dy = x-100, y-100
                r = math.sqrt(dx*dx+dy*dy)
                az = (math.degrees(math.atan2(dx,-dy))) % 360
                el = max(0, min(90, 90*(1-r/88)))
                return az, el
            def paintEvent(self, ev):  #vers 1
                p = QPainter(self)
                p.setRenderHint(QPainter.RenderHint.Antialiasing)
                # Background circle = top-down hemisphere
                grad = QRadialGradient(100,100,90)
                grad.setColorAt(0, QColor(60,80,120))
                grad.setColorAt(1, QColor(20,25,40))
                p.setBrush(QBrush(grad)); p.setPen(_Qt.PenStyle.NoPen)
                p.drawEllipse(10,10,180,180)
                # Rings
                p.setPen(QPen(QColor(80,90,130,120), 0.8))
                for frac in (0.33,0.66,1.0):
                    r = int(88*frac)
                    p.drawEllipse(100-r, 100-r, 2*r, 2*r)
                # Compass ticks N/E/S/W
                p.setPen(QPen(QColor(140,150,200), 1))
                for az_t, lbl in [(0,"N"),(90,"E"),(180,"S"),(270,"W")]:
                    ar = math.radians(az_t)
                    x1,y1 = 100+80*math.sin(ar), 100-80*math.cos(ar)
                    x2,y2 = 100+88*math.sin(ar), 100-88*math.cos(ar)
                    p.drawLine(int(x1),int(y1),int(x2),int(y2))
                    p.drawText(int(100+96*math.sin(ar))-6,
                               int(100-96*math.cos(ar))+4, lbl)
                # Light dot
                lx,ly = self._az_el_to_xy(self.az, self.el)
                # Ray from centre to dot
                p.setPen(QPen(QColor(255,220,80,160), 1, _Qt.PenStyle.DashLine))
                p.drawLine(100,100,int(lx),int(ly))
                # Glow
                glow = QRadialGradient(lx,ly,14)
                glow.setColorAt(0,QColor(255,220,80,200))
                glow.setColorAt(1,QColor(255,220,80,0))
                p.setBrush(QBrush(glow)); p.setPen(_Qt.PenStyle.NoPen)
                p.drawEllipse(int(lx)-14,int(ly)-14,28,28)
                # Solid dot
                p.setBrush(QBrush(QColor(255,220,80)))
                p.setPen(QPen(QColor(20,25,40), 1.5))
                p.drawEllipse(int(lx)-5,int(ly)-5,10,10)
                # Elevation label
                p.setPen(QPen(QColor(140,150,200)))
                from PyQt6.QtGui import QFont
                p.setFont(QFont("Arial",8))
                p.drawText(4,196,f"El:{self.el:.0f}°  Az:{self.az:.0f}°")
            def _drag(self, x, y):  #vers 1
                az, el = self._xy_to_az_el(x,y)
                self.az = az; self.el = el; _az[0]=az; _el[0]=el
                self.update()
                if self.on_change: self.on_change()
            def mousePressEvent(self,ev): self._drag(ev.position().x(),ev.position().y())  #vers 1
            def mouseMoveEvent(self,ev):  #vers 1
                if ev.buttons() & _Qt.MouseButton.LeftButton:
                    self._drag(ev.position().x(),ev.position().y())

        picker = _PosPicker()
        picker.az = _az[0]; picker.el = _el[0]
        pos_vlay.addWidget(picker, 0, _Qt.AlignmentFlag.AlignHCenter)

        # Preset quick-picks
        preset_row = QHBoxLayout()
        _preset_icon_map = {
            "Top":    "light_preset_top_icon",
            "GTA":    "light_preset_gta_icon",
            "Side":   "light_preset_side_icon",
            "Sunset": "light_preset_sunset_icon",
        }
        for label, az2, el2 in [
            ("Top",    0,   90),
            ("GTA",    45,  50),
            ("Side",   90,  15),
            ("Sunset", 225, 10),
        ]:
            pb = QPushButton(label); pb.setFixedHeight(26)
            try:
                from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVGL
                ico_fn = _preset_icon_map.get(label)
                if ico_fn:
                    pb.setIcon(getattr(_SVGL, ico_fn)(size=16))
                    pb.setIconSize(QSize(16, 16))
            except Exception:
                pass
            def _set_preset(checked=False, a=az2, e=el2):  #vers 1
                picker.az=a; picker.el=e; _az[0]=a; _el[0]=e
                picker.update(); _apply_live()
            pb.clicked.connect(_set_preset)
            preset_row.addWidget(pb)
        pos_vlay.addLayout(preset_row)
        root.addWidget(pos_grp)

        # - Brightness sliders
        bri_grp = QGroupBox("Brightness")
        bri_lay = QVBoxLayout(bri_grp)

        def _slider_row(label, lo, hi, val, scale=100):  #vers 1
            row = QHBoxLayout()
            lbl = QLabel(label); lbl.setFixedWidth(72)
            sl  = QSlider(_Qt.Orientation.Horizontal)
            sl.setRange(int(lo*scale), int(hi*scale))
            sl.setValue(int(val*scale))
            val_lbl = QLabel(f"{val:.2f}"); val_lbl.setFixedWidth(36)
            sl.valueChanged.connect(lambda v: val_lbl.setText(f"{v/scale:.2f}"))
            sl.valueChanged.connect(lambda _: _apply_live())
            row.addWidget(lbl); row.addWidget(sl,1); row.addWidget(val_lbl)
            bri_lay.addLayout(row)
            return sl

        int_sl  = _slider_row("Intensity", 0, 2, _int, 100)
        amb_sl  = _slider_row("Ambient",   0, 1, _amb, 100)
        root.addWidget(bri_grp)

        # - Live preview
        def _apply_live():  #vers 1
            az_r = math.radians(_az[0])
            el_r = math.radians(_el[0])
            lx = math.sin(az_r)*math.cos(el_r)
            ly = math.cos(az_r)*math.cos(el_r)
            lz = math.sin(el_r)
            nd = (lx,ly,lz,0.0)
            self._vp_light_dir      = nd
            self._vp_light_ambient  = amb_sl.value()/100
            self._vp_light_intensity = int_sl.value()/100
            if vp:
                vp._light_dir       = nd
                vp._light_ambient   = self._vp_light_ambient
                vp._shading_enabled = shade_cb.isChecked()
                vp.update()

        picker.on_change = _apply_live

        def _sync_shade(on):  #vers 1
            sb2 = getattr(self,'_shading_btn',None)
            if sb2: sb2.blockSignals(True); sb2.setChecked(on); sb2.blockSignals(False)
            self._toggle_viewport_shading(on)
        shade_cb.toggled.connect(_sync_shade)
        shade_cb.toggled.connect(lambda _: _apply_live())

        # Reset
        reset_btn = QPushButton("Reset to defaults"); reset_btn.setFixedHeight(26)
        def _reset():  #vers 1
            picker.az=45; picker.el=50; _az[0]=45; _el[0]=50; picker.update()
            int_sl.setValue(100); amb_sl.setValue(30)
            shade_cb.setChecked(True); _apply_live()
        reset_btn.clicked.connect(_reset)
        root.addWidget(reset_btn)

        # OK / Cancel
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                              QDialogButtonBox.StandardButton.Cancel)

        def _save():  #vers 2
            _apply_live()
            cfg_path = os.path.join(get_user_config_dir(), 'model_workshop.json')
            try:
                try: cfg = json.load(open(cfg_path))
                except Exception: cfg = {}
                nd = self._vp_light_dir
                cfg['viewport_light'] = {
                    'dir_x':nd[0],'dir_y':nd[1],'dir_z':nd[2],
                    'intensity':self._vp_light_intensity,
                    'ambient':self._vp_light_ambient,
                    'shading':shade_cb.isChecked(),
                    'az':_az[0], 'el':_el[0],
                }
                os.makedirs(os.path.dirname(cfg_path),exist_ok=True)
                json.dump(cfg,open(cfg_path,'w'),indent=2)
            except Exception: pass
            dlg.accept()

        def _cancel():  #vers 1
            self._vp_light_dir=_dir; self._vp_light_ambient=_amb
            self._vp_light_intensity=_int
            if vp: vp._light_dir=_dir; vp._light_ambient=_amb
            vp._shading_enabled=_shade_on; vp.update()
            _sync_shade(_shade_on); dlg.reject()

        bb.accepted.connect(_save); bb.rejected.connect(_cancel)
        root.addWidget(bb)
        _apply_live(); dlg.exec()

    def _load_viewport_light_settings(self): #vers 3
        """Load saved viewport light settings from model_workshop.json."""
        import json, os
        cfg_path = os.path.join(get_user_config_dir(), 'model_workshop.json')
        try:
            cfg = json.load(open(cfg_path))
            vl = cfg.get('viewport_light', {})
            if vl:
                lx = vl.get('dir_x', 0.5)
                ly = vl.get('dir_y', 0.5)
                lz = vl.get('dir_z', 0.8)
                import math
                ll = math.sqrt(lx*lx+ly*ly+lz*lz) or 1.0
                self._vp_light_dir      = (lx/ll, ly/ll, lz/ll, 0.0)
                self._vp_light_ambient  = vl.get('ambient',   0.30)
                self._vp_light_intensity = vl.get('intensity', 1.0)
                shade_on = vl.get('shading', True)
                vp = getattr(self, 'preview_widget', None)
                if vp:
                    vp._light_dir       = self._vp_light_dir
                    vp._light_ambient   = self._vp_light_ambient
                    vp._shading_enabled = shade_on
                sb = getattr(self, '_shading_btn', None)
                if sb:
                    sb.blockSignals(True)
                    sb.setChecked(shade_on)
                    sb.blockSignals(False)
        except Exception:
            pass   # no saved settings — use defaults

    def _set_select_mode(self, mode: str): #vers 9
        """Set viewport selection mode: vertex / edge / face / poly / object.
        Applied to every visible viewport (single or all 4 quad panes).
        Switching modes clears the other modes' selection sets, so there is
        no stale hidden selection left over when the user switches back.
        Sub-object modes show the list's geometry alone so it can be picked."""
        was = getattr(getattr(self, 'preview_widget', None), '_select_mode', 'object')
        names = {'vertex': 'Vertex', 'edge': 'Edge', 'face': 'Face', 'poly': 'Polygon'}
        for vp in self._live_viewports():
            vp._select_mode = mode
            vp.set_edit_mode_banner(f"Edit Mode - {names[mode]}" if mode in names else "")
            vp._mesh_context_callback = self._mesh_menu
            vp.set_mesh_gizmo_callback(self._mesh_gizmo_moved)
            vp.set_mesh_xform_callback(self._mesh_gizmo_xform)
            vp.set_mesh_face_mode_callback(self._mesh_set_face_mode)
            vp.set_mesh_face_mode(getattr(self, '_mesh_face_mode', 'connected'), notify=False)
            if mode != 'vertex':
                vp._selected_verts.clear()
            if mode != 'edge':
                vp._selected_edges.clear()
            if mode not in ('face', 'poly'):
                vp._selected_faces.clear()
            vp.update()
        if (mode == 'object') != (was == 'object') and getattr(self, '_current_dff_model', None):
            self._show_dff_geometry(max(0, self._dff_get_selected_geom_idx()))
        act = getattr(self, {'object': '_sel_obj_act', 'vertex': '_sel_vert_act', 'edge': '_sel_edge_act',
                             'face': '_sel_face_act', 'poly': '_sel_poly_act'}.get(mode, ''), None)
        if act is not None and not act.isChecked():
            act.setChecked(True)                    # ribbon shows the real mode
        self._set_status(f"Select mode: {mode}")
        mw = self.main_window
        if mw and hasattr(mw, 'log_message'):
            mw.log_message(f"Model Workshop: select mode → {mode}")

    def _update_select_mode_availability(self): #vers 3
        """Enable vertex/edge/face/poly select buttons only when there is
        an actual model (DFF geometry or COL surfaces) to select from.
        Called after load, after creating a primitive from scratch, and
        whenever the active model is cleared."""
        has_dff = bool(getattr(self, '_current_dff_model', None) and
                       getattr(self._current_dff_model, 'geometries', None))
        has_col = bool(getattr(self, 'current_col_file', None) and
                       getattr(self.current_col_file, 'models', None))
        has_model = has_dff or has_col
        for attr in ('_sel_vert_act', '_sel_edge_act',
                     '_sel_face_act', '_sel_poly_act', '_edit_model_act'):
            b = getattr(self, attr, None)
            if b:
                b.setEnabled(has_model)
        if not has_model:
            for vp in self._live_viewports():
                vp._select_mode = 'object'
                vp._selected_verts.clear()
                vp._selected_edges.clear()
                vp._selected_faces.clear()
                vp.update()
            grp = getattr(self, '_select_mode_group', None)
            if grp:
                grp.setExclusive(False)
                for attr in ('_sel_vert_act', '_sel_edge_act',
                             '_sel_face_act', '_sel_poly_act'):
                    b = getattr(self, attr, None)
                    if b: b.setChecked(False)
                grp.setExclusive(True)
            if getattr(self, '_sel_obj_act', None):
                self._sel_obj_act.setChecked(True)

    def _toggle_backface_cull(self): #vers 1
        """Toggle backface culling — when ON only the front face is visible/selectable."""
        vp = getattr(self, 'preview_widget', None)
        if not vp:
            return
        current = getattr(vp, '_backface', False)
        vp.set_backface(not current)
        state = "off (front+back)" if current else "on (front only)"
        self._set_status(f"Backface cull: {state}")

    def _toggle_front_only_paint(self): #vers 1
        """Toggle front-face-only paint — prevents painting geometry behind the current view."""
        vp = getattr(self, 'preview_widget', None)
        if not vp:
            return
        current = getattr(vp, '_front_only_paint', False)
        vp._front_only_paint = not current
        btn = getattr(self, '_front_paint_btn', None)
        if btn:
            btn.setChecked(not current)
        self._set_status(
            f"Front-only paint: {'ON — only visible faces painted' if not current else 'OFF'}")


    # - 3ds Max style 4-pane viewport (Top/Front/Side/Perspective)

    def _create_quad_viewport(self): #vers 1
        """Build the 2x2 quad viewport, resizable via splitters, each pane's
        view is user-assignable (right-click). Perspective pane stays free-
        rotate; Top/Front/Side panes are locked to ortho projection. Added
        as a second page of self._viewport_stack."""
        presets = [
            ("Top",         0,   0, 'ortho'),
            ("Front",       0,  90, 'ortho'),
            ("Side",       90,   0, 'ortho'),
            ("Perspective",30,  20, 'perspective'),
        ]
        self._quad_panes = []
        for label, yaw, pitch, proj in presets:
            pane = DFFViewport()
            pane.app_settings = getattr(self.preview_widget, 'app_settings', None)
            pane._workshop_ref = self
            pane.set_view_lock(proj == 'ortho', label, yaw=yaw, pitch=pitch, projection=proj)
            pane._quad_preset_name = label
            pane.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)
            pane._label_widget.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            pane._label_widget.customContextMenuRequested.connect(
                lambda pos, p=pane: self._show_quad_pane_menu(p, p._label_widget.mapToGlobal(pos)))
            # Double-click to maximize/restore - via an event filter rather
            # than overriding DFFViewport's own mouseDoubleClickEvent, since
            # that class is shared by Model Viewer/Vehicle Workshop/etc with
            # no quad-specific behaviour needed there.
            pane.installEventFilter(self)
            self._quad_panes.append(pane)

        top_row = QSplitter(Qt.Orientation.Horizontal)
        top_row.addWidget(self._quad_panes[0])
        top_row.addWidget(self._quad_panes[1])

        bottom_row = QSplitter(Qt.Orientation.Horizontal)
        bottom_row.addWidget(self._quad_panes[2])
        bottom_row.addWidget(self._quad_panes[3])

        quad = QSplitter(Qt.Orientation.Vertical)
        quad.addWidget(top_row)
        quad.addWidget(bottom_row)
        self._quad_container  = quad
        self._quad_top_row    = top_row
        self._quad_bottom_row = bottom_row
        self._maximized_pane  = None
        self._viewport_stack.addWidget(quad)

        self._load_quad_layout()
        return quad

    def eventFilter(self, obj, event): #vers 1
        """Double-click on a quad pane maximizes/restores it - previously
        there was no way to view just one of the 4 panes full-size; the
        '4-Pane View' toggle only ever swapped between the whole quad
        grid and the single original preview_widget (never any specific
        quad pane), reported as feeling 'locked to one full pane'."""
        from PyQt6.QtCore import QEvent
        if event.type() == QEvent.Type.MouseButtonDblClick:
            panes = getattr(self, '_quad_panes', None)
            if panes and obj in panes:
                self._toggle_pane_maximize(obj)
                return True
        return super().eventFilter(obj, event)

    def _toggle_pane_maximize(self, pane): #vers 2
        """Maximize the given quad pane to fill the whole quad area
        (hiding the other 3), or restore all 4 if this pane is already
        maximized. Hiding a widget inside a QSplitter lets its siblings
        take the freed space automatically - hiding one pane per row
        plus the whole opposite row covers both nesting levels.

        The maximized pane's label switches to 'Full View' - that label
        is also the pane's exclusive right-click-menu trigger (see
        _create_quad_viewport), so without it there'd be no labelled
        target to right-click while maximized, and right-click-drag
        rotation elsewhere on the pane would have nothing to contrast
        against. Restores the pane's original view-name label
        (Top/Front/Side/Perspective) when un-maximized."""
        panes = getattr(self, '_quad_panes', None)
        top_row = getattr(self, '_quad_top_row', None)
        bottom_row = getattr(self, '_quad_bottom_row', None)
        if not panes or top_row is None or bottom_row is None:
            return

        if getattr(self, '_maximized_pane', None) is pane:
            # Restore all 4
            for p in panes:
                p.setVisible(True)
            top_row.setVisible(True)
            bottom_row.setVisible(True)
            self._maximized_pane = None
            restore_label = getattr(pane, '_pre_maximize_label', None) or pane._view_label
            pane._label_widget.setText(restore_label)
            pane._label_widget.adjustSize()
            return

        idx = panes.index(pane)
        for i, p in enumerate(panes):
            p.setVisible(i == idx)
        if idx in (0, 1):
            top_row.setVisible(True)
            bottom_row.setVisible(False)
        else:
            top_row.setVisible(False)
            bottom_row.setVisible(True)
        self._maximized_pane = pane
        pane._pre_maximize_label = pane._view_label
        pane._label_widget.setText("Full View")
        pane._label_widget.adjustSize()

    def _show_quad_pane_menu(self, pane, global_pos): #vers 3
        """Right-click a pane's LABEL to reassign its view, user-defined,
        same idea as 3ds Max viewport labels - plus Maximize/Restore,
        for discoverability alongside the double-click shortcut.
        Right-clicking anywhere else on the pane does the normal
        rotate-drag instead (see the label-only context menu wiring in
        _create_quad_viewport) - global_pos is already in screen
        coordinates, mapped by the caller from the label widget it was
        actually clicked on, not the pane itself."""
        options = [
            ("Top",         0,   0, 'ortho'),
            ("Front",       0,  90, 'ortho'),
            ("Side",       90,   0, 'ortho'),
            ("Back",      180,  90, 'ortho'),
            ("Bottom",       0, 180, 'ortho'),
            ("Perspective",30,  20, 'perspective'),
        ]
        menu = QMenu(pane)
        for label, yaw, pitch, proj in options:
            act = menu.addAction(label)
            act.triggered.connect(
                lambda checked=False, l=label, y=yaw, p=pitch, pr=proj:
                    self._assign_quad_pane_view(pane, l, y, p, pr))
        menu.addSeparator()
        is_max = getattr(self, '_maximized_pane', None) is pane
        max_act = menu.addAction("Restore 4-Pane View" if is_max else "Maximize This Pane")
        max_act.triggered.connect(lambda checked=False, p=pane: self._toggle_pane_maximize(p))
        menu.exec(global_pos)

    def _assign_quad_pane_view(self, pane, label, yaw, pitch, projection): #vers 2
        """Apply a user-chosen preset to one quad pane and persist it.
        If this pane is currently maximized, re-apply the 'Full View'
        label afterward - set_view_lock always writes the new view name
        into the label, which would otherwise silently drop the full-
        view indication while still actually maximized."""
        pane.set_view_lock(projection == 'ortho', label, yaw=yaw, pitch=pitch,
                            projection=projection)
        pane._quad_preset_name = label
        if getattr(self, '_maximized_pane', None) is pane:
            pane._pre_maximize_label = label
            pane._label_widget.setText("Full View")
            pane._label_widget.adjustSize()
        self._save_quad_layout()

    def _toggle_quad_view(self, checked): #vers 2
        """Swap the central-widget stack between the single viewport and the
        4-pane view."""
        if checked:
            if not hasattr(self, '_quad_container'):
                self._create_quad_viewport()
                self.preview_widget._on_geometry_loaded = self._sync_quad_from_main
            self._viewport_stack.setCurrentWidget(self._quad_container)
            self._sync_quad_from_main()
        else:
            if hasattr(self, '_quad_container'):
                self._save_quad_layout()
                if getattr(self, '_maximized_pane', None) is not None:
                    self._toggle_pane_maximize(self._maximized_pane)  # restore all 4
            self._viewport_stack.setCurrentWidget(self.preview_widget)

    def _sync_quad_from_main(self): #vers 3
        """Mirror the main viewport's loaded geometry, selection state and
        render mode into the 4 quad panes (so opening 4-Pane View shows
        whatever was already selected/set in the single view)."""
        panes = getattr(self, '_quad_panes', None)
        if not panes:
            return
        src = self.preview_widget
        for pane in panes:
            pane._vertices  = list(getattr(src, '_vertices', []))
            pane._normals   = list(getattr(src, '_normals', []))
            pane._uvs       = list(getattr(src, '_uvs', []))
            pane._triangles = list(getattr(src, '_triangles', []))
            pane._materials = getattr(src, '_materials', [])
            pane._prelit    = list(getattr(src, '_prelit', []))
            pane._all_geoms = list(getattr(src, '_all_geoms', []))
            pane._current_geom_flags = getattr(src, '_current_geom_flags', 0)
            pane._select_mode    = getattr(src, '_select_mode', 'object')
            pane._selected_verts = set(getattr(src, '_selected_verts', set()))
            pane._selected_edges = set(getattr(src, '_selected_edges', set()))
            pane._selected_faces = set(getattr(src, '_selected_faces', set()))
            pane.set_mesh_face_mode_callback(self._mesh_set_face_mode)
            pane.set_mesh_face_mode(getattr(self, '_mesh_face_mode', 'connected'), notify=False)
            if hasattr(pane, 'set_render_mode') and hasattr(src, '_mode'):
                pane.set_render_mode(src._mode)
            pane._auto_fit()
            pane.update()

    def _save_quad_layout(self): #vers 2
        """Persist per-pane view assignment + splitter sizes."""
        import json, os
        panes = getattr(self, '_quad_panes', None)
        if not panes:
            return
        try:
            cfg_dir = str(get_user_config_dir())
            os.makedirs(cfg_dir, exist_ok=True)
            cfg_path = os.path.join(cfg_dir, 'model_workshop.json')
            data = {}
            if os.path.exists(cfg_path):
                try:
                    with open(cfg_path) as f:
                        data = json.load(f)
                except Exception:
                    data = {}
            data['quad_view'] = {
                'panes': [
                    {'label': p._view_label, 'yaw': p._yaw, 'pitch': p._pitch,
                     'projection': p._projection}
                    for p in panes
                ],
                'top_row_sizes':    self._quad_top_row.sizes(),
                'bottom_row_sizes': self._quad_bottom_row.sizes(),
                'outer_sizes':      self._quad_container.sizes(),
            }
            with open(cfg_path, 'w') as f:
                json.dump(data, f, indent=2)
        except Exception:
            pass

    def _load_quad_layout(self): #vers 2
        """Restore per-pane view assignment + splitter sizes, if saved."""
        import json, os
        panes = getattr(self, '_quad_panes', None)
        if not panes:
            return
        try:
            cfg_path = os.path.join(get_user_config_dir(), 'model_workshop.json')
            if not os.path.exists(cfg_path):
                return
            with open(cfg_path) as f:
                data = json.load(f)
            qv = data.get('quad_view')
            if not qv:
                return
            saved_panes = qv.get('panes', [])
            for pane, saved in zip(panes, saved_panes):
                pane.set_view_lock(saved.get('projection') == 'ortho',
                                    saved.get('label', ''),
                                    yaw=saved.get('yaw'), pitch=saved.get('pitch'),
                                    projection=saved.get('projection', 'perspective'))
                pane._quad_preset_name = saved.get('label', '')
            if qv.get('top_row_sizes'):
                self._quad_top_row.setSizes(qv['top_row_sizes'])
            if qv.get('bottom_row_sizes'):
                self._quad_bottom_row.setSizes(qv['bottom_row_sizes'])
            if qv.get('outer_sizes'):
                self._quad_container.setSizes(qv['outer_sizes'])
        except Exception:
            pass





    def _live_viewports(self): #vers 1
        """All currently active DFF viewport instances - just preview_widget
        in single-view mode, or preview_widget + the 4 quad panes when the
        4-Pane View is showing. Toolbar actions (flip/rotate/render style/
        select mode) and selection state are applied across every entry
        here, so every visible pane behaves like the main viewport did."""
        vps = []
        pw = getattr(self, 'preview_widget', None)
        if pw:
            vps.append(pw)
        stack = getattr(self, '_viewport_stack', None)
        quad_visible = bool(stack and getattr(self, '_quad_container', None)
                             and stack.currentWidget() is self._quad_container)
        if quad_visible:
            vps.extend(getattr(self, '_quad_panes', None) or [])
        return vps

    def _sync_selection_to_other_viewports(self, source): #vers 1
        """Called via source._workshop_ref after a selection click. Copies
        the sub-object selection mode + sets from source into every other
        live viewport so a click in one pane highlights the same
        vertices/edges/faces everywhere (3ds Max behaviour). Camera
        (yaw/pitch/pan/zoom) is untouched - only selection state."""
        for vp in self._live_viewports():
            if vp is source:
                continue
            vp._select_mode     = source._select_mode
            vp._selected_verts  = set(source._selected_verts)
            vp._selected_edges  = set(source._selected_edges)
            vp._selected_faces  = set(source._selected_faces)
            vp.update()

    def _toggle_upscale_native(self): #vers 1
        """Toggle upscale native resolution"""
        # Placeholder for upscale native functionality
        print("Upscale Native toggled")

    def _show_shaders_dialog(self): #vers 2
        """Viewport render style presets."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QLabel,
                                      QRadioButton, QButtonGroup, QPushButton)
        dlg = QDialog(self)
        dlg.setWindowTitle("Viewport Render Style")
        dlg.setFixedSize(260, 200)
        lay = QVBoxLayout(dlg)
        lay.addWidget(QLabel("<b>Render style (viewport display only)</b>"))
        presets = [("Wireframe", "wireframe"), ("Solid", "solid"),
                   ("Painted (material colours)", "painted")]
        grp = QButtonGroup(dlg)
        current = getattr(self, '_render_mode', 'wireframe')
        for label, key in presets:
            rb = QRadioButton(label)
            rb.setChecked(key == current)
            def _set(checked, k=key):  #vers 1
                if checked:
                    self._render_mode = k
                    pw = getattr(self, 'preview_widget', None)
                    if pw and hasattr(pw, '_refresh'):
                        pw._refresh()
            rb.toggled.connect(_set)
            grp.addButton(rb)
            lay.addWidget(rb)
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dlg.accept)
        lay.addWidget(close_btn)
        dlg.exec()


    def _open_gl_viewer(self): #vers 1
        """Launch the OpenGL model viewer with current DFF/TXD if loaded."""
        try:
            from apps.components.Model_Viewer.model_viewer import open_model_viewer
            mw = getattr(self, 'main_window', None)
            dff_path = getattr(self, '_current_dff_path', None)
            txd_path = getattr(self, '_current_txd_path', None)
            win, viewer = open_model_viewer(mw, dff_path, txd_path)
            # Keep reference so window isn't GC'd
            self._gl_viewer_win = win
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f"GL Viewer error: {e}")
