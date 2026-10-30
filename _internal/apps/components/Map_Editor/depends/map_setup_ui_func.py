#this belongs in apps/components/Map_Editor/depends/map_setup_ui_func.py - Version: 13
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop UI setup

"""
Toolbars, ribbons, panels, docks, quad viewport, menu style, icons for ModelWorkshop (Map).
"""

##class MapSetupUIMixin: -
# _apply_icon_scale
# _apply_menu_bar_style
# _assign_quad_pane_view
# _build_model_ide_toolbar
# _build_model_name_toolbar
# _build_toolbars
# _create_centre_panel
# _create_frame_hierarchy_panel
# _create_left_panel
# _create_models_table_panel
# _create_paint_bar
# _create_quad_viewport
# _create_right_panel
# eventFilter
# _get_or_create_hidden_toolbar
# _get_tool_menu_style
# _info_ribbon_menu
# _live_viewports
# _load_quad_layout
# _load_viewport_light_settings
# _make_section_collapsible
# _menu_btn_popup
# _move_info_ribbon
# _open_light_setup_dialog
# _paint_variant_shape
# _rebuild_toolbars
# _render_variant_icon
# _restore_outer_layout
# _restore_toolbar_state
# _ribbon_config_path
# _save_quad_layout
# _save_toolbar_state
# set_menu_orientation
# _show_quad_pane_menu
# _switch_icon_set
# _sync_quad_from_main
# _sync_selection_to_other_viewports
# _titlebar_actions
# _titlebar_left_extras
# _titlebar_title
# _toggle_pane_maximize
# _toggle_quad_view
# _toggle_viewport_shading
# _toolbar_context_menu
# _wrap_middle_panel_with_own_dock_areas

import math

from PyQt6.QtCore import QPoint, QPointF, QSize, Qt
from PyQt6.QtGui import QBrush, QColor, QFont, QIcon, QPainter, QPen, QPixmap
from PyQt6.QtWidgets import (
    QAbstractItemView, QApplication, QComboBox, QFrame, QHBoxLayout, QLabel, QLineEdit, QListWidget,
    QMenu, QPushButton, QSplitter, QStatusBar, QTableWidget, QToolButton, QVBoxLayout,
    QWidget)

from apps.methods.dock_panels import DockPanelsMixin
from apps.methods.dff_viewport import DFFViewport
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.titlebar_func import TitlebarMixin
from apps.components.Map_Editor.depends.map_widgets_func import (
    App_name, _MapWaterDepth, _ModelListDelegate, _ViewportGauges,
    _model_workshop_config_dir)


class MapSetupUIMixin(TitlebarMixin): #vers 2
    """Toolbars, ribbons, panels, docks, quad viewport, menu style, icons."""


# - Panel Setup

    def _menu_btn_popup(self): #vers 1
        """Menu button calls _on_menu_btn_clicked."""
        return False

    def _titlebar_actions(self, layout): #vers 1
        """Map titlebar: IMG pickers, Open, Save, Import, Export, Undo."""
        icon_color = self._get_icon_color()
        _ICO_SZ = self._titlebar_icon_size()
        # Only show "Open IMG" button if NOT standalone
        if not self.standalone_mode:
            self.open_img_btn = QPushButton("OpenIMG")
            self.open_img_btn.setFont(self.button_font)
            self.open_img_btn.setIcon(self.icon_factory.folder_icon(color=icon_color))
            self.open_img_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
            self.open_img_btn.clicked.connect(self.open_img_archive)
            self.open_img_btn.setToolTip("Open an IMG archive and browse its DFF/COL entries")
            layout.addWidget(self.open_img_btn)

            # "From IMG" — pick a COL entry from the currently loaded IMG in IMG Factory
            self.from_img_btn = QPushButton("From IMG")
            self.from_img_btn.setFont(self.button_font)
            self.from_img_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
            self.from_img_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
            self.from_img_btn.clicked.connect(self._pick_col_from_current_img)
            self.from_img_btn.setToolTip("Pick a DFF/COL entry from the currently loaded IMG")
            layout.addWidget(self.from_img_btn)

        # Open button
        self.open_btn = QPushButton()
        self.open_btn.setFont(self.button_font)
        self.open_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_btn.setText("Open")
        self.open_btn.setIconSize(QSize(20, 20))
        self.open_btn.setShortcut("Ctrl+O")
        if self.button_display_mode == 'icons':
            self.open_btn.setFixedSize(40, 40)
        self.open_btn.setToolTip("Open DFF or COL model file (Ctrl+O)")
        self.open_btn.clicked.connect(self._map_open_menu)
        layout.addWidget(self.open_btn)

        # Open DFF (standalone — skips COL filter)
        self.open_dff_btn = QPushButton()
        self.open_dff_btn.setFont(self.button_font)
        self.open_dff_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_dff_btn.setText("DFF/ TXD")
        self.open_dff_btn.setIconSize(QSize(20, 20))
        self.open_dff_btn.setToolTip("Open a DFF model file directly")
        self.open_dff_btn.clicked.connect(self._open_dff_standalone)

        self.load_txd_btn = QPushButton("TXD")
        self.load_txd_btn.setFont(self.button_font)
        self.load_txd_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.load_txd_btn.setIconSize(QSize(16, 16))
        self.load_txd_btn.setFixedHeight(26)
        self.load_txd_btn.setMinimumWidth(80)
        self.load_txd_btn.setToolTip("Open TXD — uses IDE link if available, else browse")
        self.load_txd_btn.clicked.connect(self._open_txd_smart)

        # GL Model Viewer button
        self.gl_viewer_btn = QPushButton()
        self.gl_viewer_btn.setFont(self.button_font)
        self.gl_viewer_btn.setText("3D View")
        self.gl_viewer_btn.setToolTip("Open GL Model Viewer (hardware 3D)")
        self.gl_viewer_btn.clicked.connect(self._open_gl_viewer)

        # Open TXD button — loads TXD for the current DFF, or browses for one
        self.open_txd_btn = QPushButton()
        self.open_txd_btn.setFont(self.button_font)
        self.open_txd_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_txd_btn.setText("TXD")
        self.open_txd_btn.setIconSize(QSize(20, 20))
        self.open_txd_btn.setToolTip(
            "Open TXD for current DFF model (Ctrl+T)\n"
            "If IDE link is known, auto-finds TXD from open IMGs.\n"
            "Hold Shift to always browse for a file.")
        self.open_txd_btn.setShortcut("Ctrl+T")
        self.open_txd_btn.clicked.connect(self._open_txd_combined)

        # Save button
        self.save_btn = QPushButton()
        self.save_btn.setFont(self.button_font)
        self.save_btn.setIcon(self.icon_factory.save_icon(color=icon_color))
        self.save_btn.setText("Save")
        self.save_btn.setIconSize(QSize(20, 20))
        if self.button_display_mode == 'icons':
            self.save_btn.setFixedSize(40, 40)
        self.save_btn.setEnabled(True)
        self.save_btn.setToolTip("Save changed IPLs, or the open model (Ctrl+S)")
        self.save_btn.clicked.connect(self._save_map_or_model)
        layout.addWidget(self.save_btn)

        # Save button
        self.saveall_btn = QPushButton()
        self.saveall_btn.setFont(self.button_font)
        self.saveall_btn.setIcon(self.icon_factory.saveas_icon(color=icon_color))
        self.saveall_btn.setText("Save All")
        self.saveall_btn.setIconSize(QSize(20, 20))
        if self.button_display_mode == 'icons':
            self.saveall_btn.setFixedSize(40, 40)
        self.saveall_btn.setEnabled(True)
        self.saveall_btn.setToolTip("Export/save model (Ctrl+S)")
        self.saveall_btn.clicked.connect(self._saveall_file)
        #layout.addWidget(self.saveall_btn)

        # Import button (title bar) — same _import_model callback the
        # docked middle-panel mini toolbar already uses successfully.
        self.tb_import_btn = QPushButton()
        self.tb_import_btn.setFont(self.button_font)
        self.tb_import_btn.setIcon(
            self.icon_factory.import_icon(color=icon_color)
            if hasattr(self.icon_factory, 'import_icon')
            else self.icon_factory.open_icon(color=icon_color))
        self.tb_import_btn.setText("Import")
        self.tb_import_btn.setIconSize(QSize(20, 20))
        if self.button_display_mode == 'icons':
            self.tb_import_btn.setFixedSize(40, 40)
        self.tb_import_btn.setToolTip("Import IPL / IDE into the loaded world, or a model")
        self.tb_import_btn.clicked.connect(self._map_import_menu)
        layout.addWidget(self.tb_import_btn)

        # Export button (title bar) — same _export_model_menu callback the
        # docked middle-panel mini toolbar already uses successfully.
        # (Previously wired to export_all(), which is COL-specific and the
        # wrong callback for a general model export button - fixed.)
        self.export_ojs_btn = QPushButton("Export")
        self.export_ojs_btn.setFont(self.button_font)
        self.export_ojs_btn.setIcon(self.icon_factory.multi_export_icon(color=icon_color)
            if hasattr(self.icon_factory, 'multi_export_icon')
            else self.icon_factory.package_icon(color=icon_color))
        self.export_ojs_btn.setIconSize(QSize(20, 20))
        if self.button_display_mode == 'icons':
            self.export_ojs_btn.setFixedSize(40, 40)
        self.export_ojs_btn.setToolTip("Export IPLs (selected / changed), or the model")
        self.export_ojs_btn.clicked.connect(self._map_export_menu)
        self.export_ojs_btn.setEnabled(True)
        layout.addWidget(self.export_ojs_btn)

        self.export_tex_btn = QPushButton("Extract Tex")
        self.export_tex_btn.setFont(self.button_font)
        self.export_tex_btn.setIcon(self.icon_factory.package_icon(color=icon_color))
        self.export_tex_btn.setIconSize(QSize(20, 20))
        self.export_tex_btn.setToolTip("Export textures as png, tga, dss or other formats")
        self.export_tex_btn.clicked.connect(self.export_all)
        self.export_tex_btn.setEnabled(True)



        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undo_icon(color=icon_color))
        self.undo_btn.setText("")
        self.undo_btn.setIconSize(QSize(20, 20))
        self.undo_btn.clicked.connect(self._undo_last_action)
        self.undo_btn.setEnabled(True)
        self.undo_btn.setToolTip("Undo last change")
        layout.addWidget(self.undo_btn)

        # Register title bar buttons for adaptive icon-only display when
        # the window/toolbar is narrow (same apply_compact_buttons helper
        # the middle panel's mini toolbar and texture panel already use).
        self._titlebar_compact_btns = [
            (self.open_btn,       "Open"),
            (self.save_btn,       "Save"),
            (self.tb_import_btn,  "Import"),
            (self.export_ojs_btn, "Export"),
            (self.undo_btn,       ""),
        ]

    def _titlebar_left_extras(self, layout): #vers 1
        """Map has no extras after Settings."""
        pass

    def _titlebar_title(self): #vers 1
        """Centre title."""
        return App_name

    #Left side vertical panel



    # - DFF mode toolbar

    def _toggle_viewport_shading(self, enabled: bool): #vers 1
        """Toggle Lambertian shading on/off in the viewport."""
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp._shading_enabled = enabled
            vp.update()
        btn = getattr(self, '_shading_act', None)
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


    def _open_light_setup_dialog(self): #vers 2
        """Viewport light setup — visual position picker + sliders."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
            QLabel, QSlider, QPushButton, QDialogButtonBox,
            QGroupBox, QCheckBox, QFrame)
        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QRadialGradient
        from PyQt6.QtCore import Qt as _Qt
        import json, math, os

        vp = getattr(self, 'preview_widget', None)
        # 4-tuple fallback default (Aug 1 2026, same IndexError fix as
        # _apply_live below - _setup_lighting always expects (x,y,z,w))
        _dir   = getattr(self, '_vp_light_dir',      (0.5, 0.5, 0.8, 0.0))
        _amb   = getattr(self, '_vp_light_ambient',  0.30)
        _int   = getattr(self, '_vp_light_intensity', 1.0)
        _sb    = getattr(self, '_shading_act', None)
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
                    pb.setIconSize(QSize(16,16))
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
            # 4-tuple (Aug 1 2026)
            nd = (lx, ly, lz, 0.0)
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
            sb2 = getattr(self,'_shading_act',None)
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

        def _save():  #vers 1
            _apply_live()
            cfg_path = str(_model_workshop_config_dir() / 'model_workshop.json')
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

    def _load_viewport_light_settings(self): #vers 1
        """Load saved viewport light settings from model_workshop.json."""
        import json
        cfg_path = str(_model_workshop_config_dir() / 'model_workshop.json')
        try:
            cfg = json.load(open(cfg_path))
            vl = cfg.get('viewport_light', {})
            if vl:
                lx = vl.get('dir_x', 0.5)
                ly = vl.get('dir_y', 0.5)
                lz = vl.get('dir_z', 0.8)
                import math
                ll = math.sqrt(lx*lx+ly*ly+lz*lz) or 1.0
                # 4-tuple (Aug 1 2026, same IndexError fix as
                # _apply_live - the saved config only ever stores
                # dir_x/y/z, never a w component, so this needs to add
                # it back explicitly rather than just normalizing xyz)
                self._vp_light_dir      = (lx/ll, ly/ll, lz/ll, 0.0)
                self._vp_light_ambient  = vl.get('ambient',   0.30)
                self._vp_light_intensity = vl.get('intensity', 1.0)
                shade_on = vl.get('shading', True)
                vp = getattr(self, 'preview_widget', None)
                if vp:
                    vp._light_dir       = self._vp_light_dir
                    vp._light_ambient   = self._vp_light_ambient
                    vp._shading_enabled = shade_on
                sb = getattr(self, '_shading_act', None)
                if sb:
                    sb.blockSignals(True)
                    sb.setChecked(shade_on)
                    sb.blockSignals(False)
        except Exception:
            pass   # no saved settings — use defaults

    # - Primitive creation





    def _create_left_panel(self): #vers 8
        """Create left panel - COL file list (only in IMG Factory mode)"""
        # In standalone mode, don't create this panel
        if self.standalone_mode:
            self.col_list_widget = None  # Explicitly set to None
            return None

        if not self.main_window:
            # Standalone mode - return None to hide this panel
            return None

        # Only create panel in IMG Factory mode
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        panel.setMaximumWidth(300)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)

        # Header label + search button: shown in the Files dock title bar
        self._left_panel_header = QLabel("No files loaded")
        self._left_panel_header.setFont(self.panel_font)

        self._model_search_btn = QPushButton()
        self._model_search_btn.setFixedSize(20, 18)
        self._model_search_btn.setToolTip("Search model files")
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
            self._model_search_btn.setIcon(_SVG.search_icon(16))
            self._model_search_btn.setIconSize(QSize(16, 16))
        except Exception:
            self._model_search_btn.setText("?")
        self._model_search_btn.clicked.connect(self._show_model_search)

        # Search box (hidden by default)
        self._model_search_box = QLineEdit()
        self._model_search_box.setPlaceholderText("Search model files...")
        self._model_search_box.setVisible(False)
        self._model_search_box.textChanged.connect(self._filter_model_list)
        layout.addWidget(self._model_search_box)

        self.col_list_widget = QListWidget()
        self.col_list_widget.setAlternatingRowColors(True)
        self.col_list_widget.itemClicked.connect(self._on_col_selected)
        layout.addWidget(self.col_list_widget)
        return panel


    def _create_models_table_panel(self): #vers 3
        """Create the Models table panel - mini toolbar + view toggle +
        model table only. Frame Hierarchy and Textures are now separate
        top-level dock widgets (see _create_frame_hierarchy_panel,
        _create_texture_panel), not stacked into this same panel -
        every section can be independently dragged/floated/docked now,
        same as Files already could."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(250)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)
        self._middle_panel_layout = layout

        # - Mini toolbar row: Open / Save / Import / Export / Undo / 3D View
        # (docked mode only) + the view-toggle button (always visible).
        # Previously the toggle had its own dedicated row above this one,
        # wasting a full row of height on just one small button - merged
        # into a single row instead.
        icon_color = self._get_icon_color()
        self._middle_btn_row = QFrame()
        btn_layout = QHBoxLayout(self._middle_btn_row)
        btn_layout.setContentsMargins(0, 0, 0, 0)
        btn_layout.setSpacing(3)

        # Collapse/expand toggle - hides the action icons below to reclaim
        # space, independent of the icon-only/icon+text compacting.
        self._mid_btn_row_collapsed = False
        self.mid_btn_collapse_btn = QPushButton()
        self.mid_btn_collapse_btn.setIcon(SVGIconFactory.arrow_up_icon(16, icon_color))
        self.mid_btn_collapse_btn.setFixedSize(26, 28)
        self.mid_btn_collapse_btn.setToolTip("Collapse/expand this toolbar")
        self.mid_btn_collapse_btn.clicked.connect(self._toggle_mid_btn_row_collapsed)
        btn_layout.addWidget(self.mid_btn_collapse_btn)

        def _icon_btn(icon, tooltip, slot, enabled=True):  #vers 2
            b = QPushButton()
            b.setIcon(icon)
            b.setIconSize(QSize(20, 20))
            b.setFixedHeight(28)
            b.setMinimumWidth(28)
            b.setToolTip(tooltip)
            b.clicked.connect(slot)
            b.setEnabled(enabled)
            return b

        # Load (DFF + TXD + COL multi-select)
        self.open_col_btn = _icon_btn(
            self.icon_factory.open_icon(color=icon_color),
            "Load — DFF model, TXD textures, COL collision (multi-select)",
            self._open_dff_standalone)
        btn_layout.addWidget(self.open_col_btn)

        # Save
        self.save_col_btn = _icon_btn(
            self.icon_factory.save_icon(color=icon_color),
            "Save changed IPLs, or the open model (Ctrl+S)",
            self._save_map_or_model)
        btn_layout.addWidget(self.save_col_btn)

        # Import (other formats)
        self.import_btn = _icon_btn(
            self.icon_factory.import_icon(color=icon_color)
                if hasattr(self.icon_factory, 'import_icon')
                else self.icon_factory.open_icon(color=icon_color),
            "Import — MDL, OBJ, FBX and other formats",
            self._import_model)
        btn_layout.addWidget(self.import_btn)

        # Export (multiple formats — geometry + COL)
        self.export_col_btn = _icon_btn(
            self.icon_factory.multi_export_icon(color=icon_color),
            "Export — OBJ, COL, 3DS, FBX and other formats",
            self._export_model_menu)
        btn_layout.addWidget(self.export_col_btn)

        # Undo
        self.undo_col_btn = _icon_btn(
            self.icon_factory.undo_icon(color=icon_color),
            "Undo last change",
            self._undo_last_action)
        btn_layout.addWidget(self.undo_col_btn)

        # 3D View
        self._mini_3d_btn = _icon_btn(
            self.icon_factory.viewport_icon(color=icon_color),
            "Open GL Model Viewer",
            self._open_gl_viewer)
        btn_layout.addWidget(self._mini_3d_btn)

        btn_layout.addStretch()

        # View-toggle button - always visible regardless of dock state,
        # unlike the action icons above it.
        self._col_view_mode = 'detail'   # start in compact thumbnail view
        self.col_view_toggle_btn = QPushButton("[=]")
        self.col_view_toggle_btn.setFont(self.button_font)
        self.col_view_toggle_btn.setFixedWidth(32)
        self.col_view_toggle_btn.setFixedHeight(20)            # sits in the Models title bar
        self.col_view_toggle_btn.setToolTip(
            "Toggle view: compact list ↔ full details table")
        self.col_view_toggle_btn.clicked.connect(self._toggle_col_view)
        btn_layout.addWidget(self.col_view_toggle_btn)

        layout.addWidget(self._middle_btn_row)
        self._middle_btn_row.setVisible(True)   # row itself always shown -
        # the toggle button lives here now, only the action icons below
        # are conditionally hidden when not docked.
        self._sync_middle_btn_row_visibility()
        # Register for adaptive compact display
        self._mid_compact_btns = [
            (getattr(self, 'open_col_btn',  None), "Open"),
            (getattr(self, 'save_col_btn',  None), "Save"),
            (getattr(self, 'import_btn',    None), "Import"),
            (getattr(self, 'export_col_btn',None), "Export"),
            (getattr(self, 'undo_col_btn',  None), "Undo"),
        ]

        # Hook the row's own resize event directly - self.resizeEvent()
        # only fires when the whole ModelWorkshop widget's outer bounding
        # box changes, but dragging the Models/viewport dock divider only
        # changes this row's own width (the outer tab doesn't resize at
        # all), so relying solely on self.resizeEvent() left the compact
        # check stuck at whatever it computed on first layout - possibly
        # wrong, and never re-evaluated afterwards.
        _orig_mid_resize = self._middle_btn_row.resizeEvent
        def _mid_btn_row_resize(event, _orig=_orig_mid_resize):  #vers 1
            _orig(event)
            try:
                from apps.methods.imgfactory_ui_settings import apply_compact_buttons
                apply_compact_buttons(self._mid_compact_btns,
                                      self._middle_btn_row.width(),
                                      compact_threshold=320)
            except Exception:
                pass
        self._middle_btn_row.resizeEvent = _mid_btn_row_resize

        # - Model table (detail view)
        self.collision_list = QTableWidget()

        class _GuiLayout:
            def __init__(self, table):  #vers 1
                self.table = table
        self.gui_layout = _GuiLayout(self.collision_list)

        self.collision_list.setColumnCount(8)
        self.collision_list.setHorizontalHeaderLabels([
            "Name", "Format", "RW Version", "Size",
            "Verts", "Tris", "Materials", "UV Layers"])
        self.collision_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.collision_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.collision_list.setAlternatingRowColors(True)
        self.collision_list.itemSelectionChanged.connect(self._on_collision_selected)
        self.collision_list.horizontalHeader().setStretchLastSection(True)
        self.collision_list.horizontalHeader().setFixedHeight(26)
        self.collision_list.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.collision_list.customContextMenuRequested.connect(
            self._show_collision_context_menu)
        self.collision_list.setVisible(False)  # hidden at startup — compact view is default
        layout.addWidget(self.collision_list)

        # - Compact list (thumbnail + name/version/counts, single row)
        self.mod_compact_list = QTableWidget()
        self.mod_compact_list.setColumnCount(2)
        self.mod_compact_list.setHorizontalHeaderLabels(["Preview", "Details"])
        self.mod_compact_list.horizontalHeader().setStretchLastSection(True)
        self.mod_compact_list.horizontalHeader().setFixedHeight(26)
        self.mod_compact_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.mod_compact_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.mod_compact_list.setAlternatingRowColors(True)
        self.mod_compact_list.setIconSize(QSize(64, 64))
        self.mod_compact_list.itemSelectionChanged.connect(
            self._on_compact_col_selected)
        self.mod_compact_list.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.mod_compact_list.customContextMenuRequested.connect(
            self._show_collision_context_menu)
        self.mod_compact_list.setVisible(True)    # start in compact view
        self.mod_compact_list.setRowCount(0)      # populated on first file load
        self.mod_compact_list.setWordWrap(True)
        self.mod_compact_list.setItemDelegate(_ModelListDelegate(self.mod_compact_list))
        layout.addWidget(self.mod_compact_list)

        # Model Name / IDE-TXD ribbons - built here but docked into the
        # middle panel's own nested QMainWindow below, not added to this
        # layout directly. Split into two so they can be moved
        # independently of each other.
        self._info_ribbons = {
            'name': self._build_model_name_toolbar(),
            'ide':  self._build_model_ide_toolbar(icon_color),
        }
        self._info_ribbon_location = {'name': 'middle', 'ide': 'middle'}

        return self._wrap_middle_panel_with_own_dock_areas(panel)

    def _create_frame_hierarchy_panel(self): #vers 2
        """Create the Frame Hierarchy panel on its own - previously stacked
        inside the same panel as the model table/textures, now a separate
        top-level dock widget so it can be independently dragged/floated.
        No internal header row - the dock's own title bar already shows
        'Frame Hierarchy', and collapsing is now done by double-clicking
        that title bar (see _make_dock_collapsible) instead of a per-panel
        toggle button."""
        from PyQt6.QtWidgets import QTreeWidget
        self._frame_tree_panel = QFrame()
        self._frame_tree_panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ft_lay = QVBoxLayout(self._frame_tree_panel)
        ft_lay.setContentsMargins(2, 2, 2, 2)
        ft_lay.setSpacing(2)

        self._frame_tree = QTreeWidget()
        self._frame_tree.setHeaderLabels(["Frame", "Parent", "Position"])
        self._frame_tree.setAlternatingRowColors(True)
        self._frame_tree.setFont(self.panel_font)
        self._frame_tree.itemClicked.connect(self._on_frame_tree_clicked)
        ft_lay.addWidget(self._frame_tree)

        return self._frame_tree_panel


    def _make_section_collapsible(self, label, content_widget): #vers 1
        """Double-click a section's bold title label to collapse/restore
        the content widget below it - same double-click-to-collapse
        interaction as dock title bars (_make_dock_collapsible), just
        applied to a sub-section within a panel instead of a whole
        dock. Used for the IMG/IDE/IPL/DAT tabs inside Object Browser."""
        def _dbl_click(event, w=content_widget): #vers 1
            w.setVisible(not w.isVisible())
        label.mouseDoubleClickEvent = _dbl_click
        existing_tip = label.toolTip()
        label.setToolTip((existing_tip + "\n" if existing_tip else "") +
                         "Double-click to collapse/expand")

    def _wrap_middle_panel_with_own_dock_areas(self, content_panel): #vers 4
        """Give the middle panel its own nested QMainWindow (same pattern
        the right panel already uses for the viewport), so the Name and
        IDE/TXD ribbons have real Left/Right toolbar dock areas specifically
        flanking the middle panel's own content - not just the option of
        living inside it as a plain widget, or all the way over on the
        far-right ribbon stack. Both default to the bottom (closest to
        their old combined position), but can be dragged to either side
        independently within this mini QMainWindow, or moved to the main
        right panel via the existing right-click action."""
        from PyQt6.QtWidgets import QMainWindow
        middle_mw = QMainWindow()
        middle_mw.setWindowFlags(Qt.WindowType.Widget)
        middle_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        # Same explicit separator styling as outer_mw - prevents the
        # docked-mode theme-cascade issue that zeroed out separator
        # visibility from affecting this nested QMainWindow too.
        middle_mw.setStyleSheet(
            "QMainWindow::separator { "
            "background: palette(mid); width: 5px; height: 5px; } "
            "QMainWindow::separator:hover { background: palette(highlight); }")
        middle_mw.setCentralWidget(content_panel)
        middle_mw.addToolBar(Qt.ToolBarArea.BottomToolBarArea, self._info_ribbons['name'])
        # Explicit break so IDE always sits on its own row below Name,
        # instead of sharing one row whenever there's enough width.
        middle_mw.addToolBarBreak(Qt.ToolBarArea.BottomToolBarArea)
        middle_mw.addToolBar(Qt.ToolBarArea.BottomToolBarArea, self._info_ribbons['ide'])
        self._middle_mw = middle_mw
        return middle_mw

    def _build_model_name_toolbar(self): #vers 2
        """Build the 'Model Name: Click to edit...' ribbon on its own,
        split out from the combined Model Info ribbon so it can be
        independently docked/moved from the IDE/TXD one."""
        from PyQt6.QtWidgets import QToolBar
        tb = QToolBar("Model Name")
        tb.setObjectName("Model Name")
        tb.setIconSize(QSize(18, 18))
        tb.setMovable(True)
        tb.setFloatable(True)
        tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tb.customContextMenuRequested.connect(
            lambda pos, t=tb, key='name': self._info_ribbon_menu(t, pos, key))

        name_label = QLabel("Model Name:")
        name_label.setFont(self.panel_font)
        tb.addWidget(name_label)

        self.info_name = QLineEdit()
        self.info_name.setText("Click to edit...")
        self.info_name.setFont(self.panel_font)
        self.info_name.setReadOnly(True)
        self.info_name.setMinimumWidth(120)
        self.info_name.setStyleSheet("padding: 2px; border: 1px solid palette(mid);")
        self.info_name.mousePressEvent = lambda e: self._enable_name_edit(e, False)
        tb.addWidget(self.info_name)

        return tb

    def _build_model_ide_toolbar(self, icon_color): #vers 2
        """Build the 'IDE: -- ID: -- TXD: --' ribbon on its own, split out
        from the combined Model Info ribbon so it can be independently
        docked/moved from the Name one."""
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction
        tb = QToolBar("Model IDE / TXD")
        tb.setObjectName("Model IDE / TXD")
        tb.setIconSize(QSize(18, 18))
        tb.setMovable(True)
        tb.setFloatable(True)
        tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tb.customContextMenuRequested.connect(
            lambda pos, t=tb, key='ide': self._info_ribbon_menu(t, pos, key))

        ide_lbl = QLabel("IDE:")
        ide_lbl.setFont(self.panel_font)
        tb.addWidget(ide_lbl)
        self.info_ide_section = QLabel("—")
        self.info_ide_section.setFont(self.panel_font)
        tb.addWidget(self.info_ide_section)
        self.info_model_id = QLabel("ID: —")
        self.info_model_id.setFont(self.panel_font)
        tb.addWidget(self.info_model_id)
        txd_lbl = QLabel("TXD:")
        txd_lbl.setFont(self.panel_font)
        tb.addWidget(txd_lbl)
        self.info_txd_name = QLabel("—")
        tb.addWidget(self.info_txd_name)

        self.find_in_ide_btn = QAction(
            self.icon_factory.database_icon(color=icon_color), "Find in IDE", tb)
        self.find_in_ide_btn.setToolTip("Look up model in DAT Browser IDE entries")
        self.find_in_ide_btn.triggered.connect(self._find_in_ide)
        tb.addAction(self.find_in_ide_btn)

        return tb

    def _info_ribbon_menu(self, tb, pos, key): #vers 1
        """Right-click menu on the Model Name / IDE-TXD ribbons - lets the
        user move either one independently between the middle panel
        (default) and the right panel's ribbon stack. A plain QToolBar
        can't natively drag-and-dock between two separate QMainWindows,
        so this is a deliberate action instead."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        if self._info_ribbon_location.get(key, 'middle') == 'middle':
            menu.addAction("Move to Right Panel Ribbons",
                            lambda: self._move_info_ribbon(key, 'right'))
        else:
            menu.addAction("Move to Middle Panel",
                            lambda: self._move_info_ribbon(key, 'middle'))
        menu.exec(tb.mapToGlobal(pos))

    def _move_info_ribbon(self, key: str, location: str): #vers 2
        """Reparent one of the Name/IDE-TXD toolbars (identified by key,
        'name' or 'ide') between the middle panel's own nested QMainWindow
        and the right panel's QMainWindow - dockable/floatable in both
        places. Persists the choice per-ribbon to model_workshop.json."""
        tb = self._info_ribbons.get(key)
        if tb is None or location == self._info_ribbon_location.get(key):
            return
        if location == 'right':
            mid_mw = getattr(self, '_middle_mw', None)
            if mid_mw:
                mid_mw.removeToolBar(tb)
            tb.setParent(None)
            mw = getattr(self, '_inner_mw', None)
            if mw:
                mw.addToolBar(Qt.ToolBarArea.RightToolBarArea, tb)
                tb.setVisible(True)
        else:
            mw = getattr(self, '_inner_mw', None)
            if mw:
                mw.removeToolBar(tb)
            tb.setParent(None)
            mid_mw = getattr(self, '_middle_mw', None)
            if mid_mw:
                mid_mw.addToolBar(Qt.ToolBarArea.BottomToolBarArea, tb)
                tb.setVisible(True)
        self._info_ribbon_location[key] = location
        try:
            import json
            path = self._ribbon_config_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            locations = data.get('info_ribbon_locations', {})
            locations[key] = location
            data['info_ribbon_locations'] = locations
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass
        self._set_status(f"Model Info ribbon moved to {location} panel")

    def _create_right_panel(self): #vers 17
        """Right panel using QMainWindow + QToolBar for native docking.
        QMainWindow handles toolbar placement, row stacking, floating, and
        save/restore natively — same system Gwenview/KDE apps use."""
        icon_color = self._get_icon_color()

        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        self._right_panel_ref = panel
        outer_layout = QVBoxLayout(panel)
        outer_layout.setContentsMargins(4, 4, 4, 4)
        outer_layout.setSpacing(3)

        # Inner QMainWindow — owns the viewport as central widget and all
        # QToolBars. Embedded as a plain widget (no window chrome).
        from PyQt6.QtWidgets import QMainWindow
        inner_mw = QMainWindow()
        inner_mw.setWindowFlags(Qt.WindowType.Widget)
        inner_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        # Same explicit separator styling as outer_mw/middle_mw - guards
        # against the same docked-mode theme-cascade issue, in case any
        # dock widgets get added here in future.
        inner_mw.setStyleSheet(
            "QMainWindow::separator { "
            "background: palette(mid); width: 5px; height: 5px; } "
            "QMainWindow::separator:hover { background: palette(highlight); }")
        self._inner_mw = inner_mw

        # Central widget — the 3D viewport, wrapped in a stack so the
        # 4-Pane view (Top/Front/Side/Perspective) can be swapped in without
        # disturbing QMainWindow's central-widget ownership.
        from PyQt6.QtWidgets import QStackedWidget
        self.preview_widget = DFFViewport()
        self.preview_widget._workshop_ref = self
        if hasattr(self.preview_widget, 'set_texture_downscale_settings'):
            self.preview_widget.set_texture_downscale_settings(
                self.map_settings.get('texture_downscale_enabled'),
                self.map_settings.get('texture_downscale_threshold'),
                self.map_settings.get('texture_downscale_target'))
        # Restore any saved camera keybinding overrides (Aug 16 2026)
        if hasattr(self.preview_widget, 'set_key_bindings'):
            saved_key_overrides = self.map_settings.get('viewport_key_bindings')
            if saved_key_overrides:
                self.preview_widget.set_key_bindings(saved_key_overrides)
        # Restore the saved zone render style (Aug 16 2026)
        if hasattr(self.preview_widget, 'set_zone_render_style'):
            self.preview_widget.set_zone_render_style(
                self.map_settings.get('zone_render_style'))
        # Restore saved grid settings
        if hasattr(self.preview_widget, 'set_grid_type'):
            self.preview_widget.set_grid_type(self.map_settings.get('grid_type') or 'lines')
        if hasattr(self.preview_widget, 'set_grid_colors'):
            self.preview_widget.set_grid_colors(
                tuple(self.map_settings.get('grid_bg_color') or (51, 128, 230)),
                tuple(self.map_settings.get('grid_line_color') or (76, 76, 102)))
        if hasattr(self.preview_widget, 'set_grid_line_size'):
            self.preview_widget.set_grid_line_size(int(self.map_settings.get('grid_line_size') or 4))
        if hasattr(self.preview_widget, 'set_grid_spacing'):
            self.preview_widget.set_grid_spacing(int(self.map_settings.get('grid_spacing') or 5))
        if hasattr(self.preview_widget, 'set_grid_fixed_step'):
            self.preview_widget.set_grid_fixed_step(int(self.map_settings.get('grid_fixed_step') or 200))
        if hasattr(self.preview_widget, 'set_grid_cell_count'):
            self.preview_widget.set_grid_cell_count(int(self.map_settings.get('grid_cell_count') or 24))
        # Radar tex layer needs a loaded world/ModelCache to read real
        # radarNN.txd textures from - nothing to load yet at
        # construction time, the world-loaded hook (_on_world_loaded,
        # search "Same for the radar tex layer") handles it once a
        # real game's world actually loads.
        if hasattr(self.preview_widget, 'set_grid_scale_mode'):
            self.preview_widget.set_grid_scale_mode(self.map_settings.get('grid_scale_mode') or 'zoom_relative')
        if hasattr(self.preview_widget, 'set_squares_fill'):
            self.preview_widget.set_squares_fill(
                self.map_settings.get('squares_fill_mode') or 'color',
                tuple(self.map_settings.get('grid_bg_color') or (51, 128, 230)),
                self.map_settings.get('squares_texture_path') or '',
                int(self.map_settings.get('squares_tile_size') or 256))
        if hasattr(self.preview_widget, 'set_squares_texture_alpha'):
            self.preview_widget.set_squares_texture_alpha(float(self.map_settings.get('squares_texture_alpha') or 0.5))
        if hasattr(self.preview_widget, 'set_grid_show_lines'):
            self.preview_widget.set_grid_show_lines(self.map_settings.get('grid_show_lines', True))
        if hasattr(self.preview_widget, 'set_grid_hide_over_radar_tiles'):
            self.preview_widget.set_grid_hide_over_radar_tiles(bool(self.map_settings.get('grid_hide_over_radar_tiles')))
        if hasattr(self.preview_widget, 'set_skybox_path'):
            saved_skybox = self.map_settings.get('skybox_path')
            if saved_skybox:
                self.preview_widget.set_skybox_path(saved_skybox)
        #  fix (Aug 20 2026,  "the timecyc.dat also has a
        # fixed path, but the radar loads fine across the game
        # versions") - removed the startup restore of a previously-
        # saved timecyc_path here. Auto-detection already re-runs on
        # every real world load (search "re-checked on every world
        # load" below) and correctly overwrites this same setting with
        # whichever game is actually current - this startup restore
        # was the one remaining place a stale, different game's own
        # path could still be used, in the brief window before a new
        # world is actually loaded. radar never has this problem at
        # all because it never persists a path into settings in the
        # first place - it always reads fresh, live textures straight
        # out of whichever IMG archive is currently indexed.
        if hasattr(self.preview_widget, 'set_sky_gradient_flipped'):
            self.preview_widget.set_sky_gradient_flipped(bool(self.map_settings.get('sky_gradient_flipped')))
        if hasattr(self.preview_widget, 'set_water_display_style'):
            self.preview_widget.set_water_display_style(self.map_settings.get('water_display_style') or 'fill')
        if hasattr(self.preview_widget, 'set_water_texture'):
            self.preview_widget.set_water_texture(
                self.map_settings.get('water_texture_path') or '',
                int(self.map_settings.get('water_tile_size') or 256))
        if hasattr(self.preview_widget, 'set_water_hide_outside_map'):
            self.preview_widget.set_water_hide_outside_map(bool(self.map_settings.get('water_hide_outside_map')))
        if hasattr(self.preview_widget, 'set_water2_use_texture'):
            self.preview_widget.set_water2_use_texture(bool(self.map_settings.get('water2_use_texture')))
        if hasattr(self.preview_widget, 'set_water2_texture_path'):
            saved_water2_tex = self.map_settings.get('water2_custom_texture_path')
            if saved_water2_tex:
                self.preview_widget.set_water2_texture_path(saved_water2_tex)
        if hasattr(self.preview_widget, 'set_water2_height_offset'):
            self.preview_widget.set_water2_height_offset(float(self.map_settings.get('water2_height_offset')))
        if hasattr(self.preview_widget, 'set_water2_alpha'):
            self.preview_widget.set_water2_alpha(float(self.map_settings.get('water2_alpha')))
        if hasattr(self.preview_widget, 'set_water2_x_offset'):
            self.preview_widget.set_water2_x_offset(float(self.map_settings.get('water2_x_offset')))
        if hasattr(self.preview_widget, 'set_water2_y_offset'):
            self.preview_widget.set_water2_y_offset(float(self.map_settings.get('water2_y_offset')))
        if hasattr(self.preview_widget, 'set_water2_offset_mode'):
            self.preview_widget.set_water2_offset_mode(self._water2_offset_mode())
        if hasattr(self.preview_widget, 'set_camera_state'):
            self.preview_widget.set_camera_state(
                dist=self.map_settings.get('viewport_dist'),
                pan_x=self.map_settings.get('viewport_pan_x'),
                pan_y=self.map_settings.get('viewport_pan_y'),
                yaw=self.map_settings.get('viewport_yaw'),
                pitch=self.map_settings.get('viewport_pitch'))
        # Wire the path node drag callback once, here at construction
        # (Aug 17 2026)
        if hasattr(self.preview_widget, 'set_path_node_drag_callback'):
            self.preview_widget.set_path_node_drag_callback(self._on_path_node_dragged)
        # Wire the box-corner resize callback too (Aug 19 2026, for
        # No-Clip box editing's own prerequisite - box resizing
        # itself), same one-time-wiring pattern.
        if hasattr(self.preview_widget, 'set_box_resize_callback'):
            self.preview_widget.set_box_resize_callback(self._on_box_resized)
        # Wire the whole-IPL drag callback once too, here at
        # construction (Aug 18 2026)
        if hasattr(self.preview_widget, 'set_ipl_drag_callback'):
            self.preview_widget.set_ipl_drag_callback(self._on_ipl_dragged)
        # Single-object move gizmo (Sep 23 2026)
        if hasattr(self.preview_widget, 'set_gizmo_move_callback'):
            self.preview_widget.set_gizmo_move_callback(self._on_gizmo_moved)
            self.preview_widget.set_gizmo_rotate_callback(self._on_gizmo_rotated)
            self.preview_widget.set_selection_callback(self._on_viewport_selection)
            self.preview_widget.set_model_drop_callback(self._on_model_dropped)
        # Wire the Move/Rotate click callback too (Aug 19 2026)
        if hasattr(self.preview_widget, 'set_ipl_click_callback'):
            self.preview_widget.set_ipl_click_callback(self._on_ipl_click_for_move_or_rotate)
        # Wire the multi-IPL selection-change callback too (Aug 19 2026)
        if hasattr(self.preview_widget, 'set_ipl_selection_callback'):
            self.preview_widget.set_ipl_selection_callback(self._on_ipl_selection_changed)
        # Restore the saved zoom-to-cursor setting (Aug 18 2026)
        if hasattr(self.preview_widget, 'set_zoom_to_cursor'):
            self.preview_widget.set_zoom_to_cursor(bool(self.map_settings.get('zoom_to_cursor')))
        # Restore the saved auto-highlight-on-hover setting too (Aug 19 2026)
        if hasattr(self.preview_widget, 'set_hover_highlight_enabled'):
            self.preview_widget.set_hover_highlight_enabled(
                bool(self.map_settings.get('auto_highlight_hover')))
        if hasattr(self.preview_widget, 'set_hover_context_callback'):
            self.preview_widget.set_hover_context_callback(self._on_hover_context_menu)
        # Middle-click cycle (Aug 21 2026,  "middle click
        # can cycle?") - same real callback the Cycle Zones button's
        # own left-click already uses.
        if hasattr(self.preview_widget, 'set_middle_click_cycle_callback'):
            self.preview_widget.set_middle_click_cycle_callback(self._cycle_selected_box)
        # Restore the saved no-clip-box-resizing setting too (Aug 19 2026)
        if hasattr(self.preview_widget, 'set_no_clip_boxes'):
            self.preview_widget.set_no_clip_boxes(bool(self.map_settings.get('no_clip_boxes')))
        self._viewport_stack = QStackedWidget()
        self._viewport_stack.addWidget(self.preview_widget)   # index 0: single view
        inner_mw.setCentralWidget(self._viewport_stack)
        self._gl_viewport  = self.preview_widget
        self._qp_viewport  = self.preview_widget
        self._gl_mode      = True

        self._create_paint_bar()

        # Build all toolbars and add to the inner QMainWindow
        self._build_toolbars(inner_mw, icon_color)
        self._apply_custom_icons()

        outer_layout.addWidget(inner_mw, stretch=1)

        # Restore saved toolbar state (positions, rows, floating)
        from PyQt6.QtCore import QTimer as _QT
        _QT.singleShot(400, self._restore_toolbar_state)

        # Save on close, on hide (tab switch / docked close) and on app quit
        self.window_closed.connect(self._save_toolbar_state)
        self.window_closed.connect(self._save_quad_layout)
        QApplication.instance().aboutToQuit.connect(self._save_toolbar_state)

        return panel

    def _build_toolbars(self, mw: 'QMainWindow', icon_color: str): #vers 18
        """Build all QToolBar instances using QAction.
        Icon set resolved once — 'default' uses SVGIconFactory with currentColor,
        '3dsmax' uses MaxIconSet with hardcoded Max palette."""
        from PyQt6.QtWidgets import QToolBar
        icon_size = QSize(20, 20)
        pw = self.preview_widget
        self._ribbon_actions = []

        # Resolve icon set once for all buttons
        _icon_set = self._get_icon_set()
        if _icon_set == '3dsmax':
            from apps.methods.max_icon_set import MaxIconSet as _IS
        else:
            _IS = None

        def _icon(default_fn, max_name: str = None):  #vers 1
            """Return correct icon function for active icon set."""
            if _IS and max_name and hasattr(_IS, max_name):
                return getattr(_IS, max_name)
            return default_fn

        def _tb(name, area=Qt.ToolBarArea.TopToolBarArea):  #vers 1
            tb = QToolBar(name, mw)
            tb.setObjectName(name)
            tb.setIconSize(icon_size)
            tb.setMovable(True)
            tb.setFloatable(True)
            tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            tb.customContextMenuRequested.connect(
                lambda pos, t=tb: self._toolbar_context_menu(t, pos))
            mw.addToolBar(area, tb)
            return tb

        def _act(tb, name, icon_fn, callback=None, checkable=False,
                 checked=False, attr=None):  #vers 1
            """Create a QAction, add to toolbar, register in _ribbon_actions."""
            try:
                icon = icon_fn(color=icon_color)
            except Exception:
                icon = self.icon_factory.settings_icon(color=icon_color)
            act = QAction(icon, name, mw)
            act.setToolTip(name)
            act.setCheckable(checkable)
            if checkable:
                act.setChecked(checked)
            if callback:
                if checkable:
                    act.toggled.connect(callback)
                else:
                    act.triggered.connect(callback)
            tb.addAction(act)
            self._ribbon_actions.append({
                'action':    act,
                'toolbar':   tb,
                'name':      name,
                'icon_fn':   icon_fn,
                'checkable': checkable,
            })
            if attr:
                setattr(self, attr, act)
            return act

        # - Ribbon 1: Selection
        tb_sel = _tb("Selection")
        from PyQt6.QtGui import QAction, QActionGroup
        from PyQt6.QtWidgets import QLabel as _QL

        # V/E/F/P as exclusive QActions in an action group
        sel_group = QActionGroup(mw)
        sel_group.setExclusive(True)

        def _sel_act(attr, name, mode, icon_fn_name, max_name=None):  #vers 1
            act = QAction(name, mw)
            try:
                fn = _icon(getattr(self.icon_factory, icon_fn_name), max_name or icon_fn_name)
                act.setIcon(fn(color=icon_color))
            except Exception:
                pass
            act.setToolTip(name)
            act.setCheckable(True)
            act.setEnabled(False)
            act.triggered.connect(lambda _=False, m=mode: self._set_select_mode(m))
            sel_group.addAction(act)
            tb_sel.addAction(act)
            self._ribbon_actions.append({
                'action': act, 'toolbar': tb_sel, 'name': name,
                'icon_fn': getattr(self.icon_factory, icon_fn_name, None),
                'checkable': True,
            })
            setattr(self, attr, act)
            return act

        _sel_act('_sel_vert_act', 'Vertex Select', 'vertex', 'vertex_select_icon', 'vertex_select_icon')
        _sel_act('_sel_edge_act', 'Edge Select',   'edge',   'edge_select_icon',   'edge_select_icon')
        _sel_act('_sel_face_act', 'Face Select',   'face',   'face_select_icon',   'face_select_icon')
        _sel_act('_sel_poly_act', 'Polygon Select','poly',   'poly_select_icon',   'poly_select_icon')
        self._sel_face_act.setChecked(True)
        self._select_mode_group = sel_group

        # Hidden - Model Workshop icon,  (Aug 20 2026)
        for a in (self._sel_vert_act, self._sel_edge_act,
                  self._sel_face_act, self._sel_poly_act):
            a.setVisible(False)

        # Selection count label — QToolBar widget (labels can't be QAction)
        self._sel_count_label = _QL("")
        self._sel_count_label.setToolTip("Selection count")
        self._sel_count_label.setStyleSheet(
            "color: palette(mid); font-size: 11px; padding: 0 4px;")
        _cnt_act = tb_sel.addWidget(self._sel_count_label)
        from apps.methods.imgfactory_svg_icons import get_select_all_icon
        _cnt_act.setIcon(get_select_all_icon(20, icon_color))   # Ribbon Manager row icon
        _cnt_act.setText("Selection count")

        tb_sel.addSeparator()

        _act(tb_sel, "Backface Culling",
             _icon(self.icon_factory.backface_icon, 'backface_cull_icon'),
             lambda v: self._toggle_backface_cull(),
             checkable=True, attr='_backface_cull_act')

        _act(tb_sel, "Front-only Paint",
             self.icon_factory.view_icon,
             lambda v: self._toggle_front_only_paint(),
             checkable=True, attr='_front_paint_act')
        # Hidden - Model Workshop icon,  (Aug 20 2026)
        self._front_paint_act.setVisible(False)

        # - Ribbon 2: Snap Targets

        from apps.methods.max_svg_icons import MaxSVGIcons
        tb_snap = _tb("Snap Targets")

        def _snap_act(attr, target, name, icon_fn_name, max_name=None):  #vers 1
            # Resolve icon — Max set overrides MaxSVGIcons snap icons
            if _IS and max_name and hasattr(_IS, max_name):
                try:
                    icon = getattr(_IS, max_name)(size=20)
                except Exception as _e:
                    print(f"[max snap icon] {max_name}: {_e}")
                    icon = self.icon_factory.settings_icon(color=icon_color)
            else:
                try:
                    icon = getattr(MaxSVGIcons, icon_fn_name)(size=20, color=icon_color)
                except Exception as _e:
                    print(f"[snap icon] {icon_fn_name}: {_e}")
                    icon = self.icon_factory.settings_icon(color=icon_color)
            act = QAction(icon, name, mw)
            act.setToolTip(name)
            act.setCheckable(True)
            def _on(checked, t=target, a=act):  #vers 1
                vp = getattr(self, 'preview_widget', None)
                if vp and hasattr(vp, '_snap_targets'):
                    vp.toggle_snap_target(t)
                    a.setChecked(vp._snap_targets.get(t, False))
            act.toggled.connect(_on)
            tb_snap.addAction(act)
            _fn = getattr(MaxSVGIcons, icon_fn_name, None)
            self._ribbon_actions.append({
                'action': act, 'toolbar': tb_snap, 'name': name,
                'icon_fn': (lambda sz=20, c=icon_color, fn=_fn:
                            fn(size=sz, color=c)) if _fn else None,
                'checkable': True,
            })
            setattr(self, attr, act)
            return act

        _snap_act('_snap_edge_act', 'edge', 'Snap: Edge of Model', 'snap_edge_icon', 'snap_edge_icon')
        _snap_act('_snap_centre_act', 'centre', 'Snap: Centre of Model', 'snap_pivot_icon', 'snap_pivot_icon')
        # Edge of Model disabled with an honest explanation (Aug 19
        # 2026) - see _snap_targets' own docstring in dff_viewport.py
        # for the full reasoning: no loaded-model bounding box data
        # exists anywhere in this viewport yet to snap an edge to, and
        # faking it with an arbitrary offset would be worse than
        # admitting it isn't built.
        self._snap_edge_act.setToolTip(
            "Snap moved objects side-to-side, on top of, or centre-to-centre\n"
            "with nearby objects (model bounding boxes). Snapped neighbours\n"
            "light up green; the controller rumbles on contact.")
        self._snap_centre_act.setToolTip(
            "While dragging a whole IPL, snaps the clicked instance's\n"
            "position to align exactly with the nearest other\n"
            "instance's own position, once within a small distance.")
        tb_snap.addSeparator()
        _act(tb_snap, "Axis Constraints",
             _icon(lambda color=icon_color: MaxSVGIcons.snap_axis_constraint_icon(
                 size=20, color=color), 'snap_axis_icon'),
             checkable=True, attr='_snap_axis_act')
        _act(tb_snap, "Angle Snap",
             _icon(lambda color=icon_color: MaxSVGIcons.snap_angle_icon(
                 size=20, color=color), 'snap_angle_icon'),
             checkable=True, attr='_snap_angle_act')
        _act(tb_snap, "Percent Snap",
             _icon(lambda color=icon_color: MaxSVGIcons.snap_percent_icon(
                 size=20, color=color), 'snap_percent_icon'),
             checkable=True, attr='_snap_percent_act')
        # Hidden - Model Workshop icons,  (Aug 20 2026)
        for a in (self._snap_axis_act, self._snap_angle_act, self._snap_percent_act):
            a.setVisible(False)

        # - Ribbon 3: Edit Geometry
        tb_geo = _tb("Edit Geometry")
        _act(tb_geo, "Mirror",
             _icon(lambda color=icon_color: MaxSVGIcons.mirror_icon(
                 size=20, color=color), 'mirror_icon'),
             callback=self._mirror_dialog, attr='_mirror_act')
        # Hidden - Model Workshop icon,  (Aug 20 2026)
        self._mirror_act.setVisible(False)
        _act(tb_geo, "Align",
             _icon(lambda color=icon_color: MaxSVGIcons.align_icon(
                 size=20, color=color), 'align_icon'),
             callback=self._align_dialog, attr='_align_act')
        _act(tb_geo, "Snap Vertices",
             _icon(self.icon_factory.snap_vertex_icon, 'snap_vertex_icon'),
             callback=self._on_snap_vertices_clicked, attr='_snap_vertices_act')
        self._snap_vertices_act.setToolTip(
            "Closes gaps by moving nearby vertices to their shared\n"
            "centroid. Acts on the current selection, or whole model.")

        # - Ribbon 4: Navigation
        tb_nav = _tb("Navigation", Qt.ToolBarArea.RightToolBarArea)
        _act(tb_nav, "Zoom In",       _icon(self.icon_factory.zoom_in_icon,   'zoom_in_icon'),   pw.zoom_in)
        _act(tb_nav, "Zoom Out",      _icon(self.icon_factory.zoom_out_icon,  'zoom_out_icon'),  pw.zoom_out)
        _act(tb_nav, "Reset View",    _icon(self.icon_factory.reset_view_icon,'reset_view_icon'),pw.reset_view)
        _act(tb_nav, "Fit to Window", _icon(self.icon_factory.fit_icon,       'fit_view_icon'),  pw.fit_to_window)
        _act(tb_nav, "Snap to Centre", _icon(self.icon_factory.converge_to_center_icon, 'snap_centre_icon'), pw.snap_to_center)
        tb_nav.addSeparator()
        for label, yaw, pitch, default_fn, max_name in [
            ("View XY",  0,   0,  self.icon_factory.view_xy_icon,  'view_xy_icon'),
            ("View XZ",  0,  90,  self.icon_factory.view_xz_icon,  'view_xz_icon'),
            ("View YZ",  90,  0,  self.icon_factory.view_yz_icon,  'view_yz_icon'),
            ("View Iso", 30, 20,  self.icon_factory.view_iso_icon, 'view_iso_icon'),
        ]:
            def _set_v(checked=False, y=yaw, p=pitch):  #vers 1
                pw._yaw = y; pw._pitch = p; pw.update()
            _act(tb_nav, label, _icon(default_fn, max_name), _set_v)
        tb_nav.addSeparator()
        _act(tb_nav, "4-Pane View",
             _icon(self.icon_factory.quad_view_icon, 'quad_view_icon'),
             self._toggle_quad_view, checkable=True, attr='_quad_view_act')
        # Hidden - Model Workshop icon,  (Aug 20 2026)
        self._quad_view_act.setEnabled(False)
        self._quad_view_act.setVisible(False)
        self._quad_view_act.setToolTip(
            "4-Pane View is currently disabled - it only ever mirrors\n"
            "single-model geometry, not the loaded world/map, so it\n"
            "showed blank panes rather than anything useful here.")

        # - Ribbon 5: Render
        tb_rend = _tb("Render", Qt.ToolBarArea.RightToolBarArea)
        # New "Overlays" ribbon (Aug 20 2026,  "The New Icons
        # on the IPL Control pane, can be moved to the ribbon" - the
        # arrow in his own screenshot pointed from IPL Controls' own
        # overlay toggle buttons up to this same top toolbar area).
        # Created empty here - the buttons themselves are still built
        # in _create_ipl_controls_dock (that method runs before this
        # one, so the ribbon has to exist first for them to move into)
        # and moved in here afterwards, deferred, by _move_overlay_
        # buttons_to_ribbon.
        tb_overlays = _tb("Overlays", Qt.ToolBarArea.RightToolBarArea)
        # Water depth gang (26px): same blue scale as the water cells
        from apps.components.Water_Editor.water_workshop import WaterZGauge
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons as _OverlayIconsForDepth
        tb_depth = _tb("Water Depth", Qt.ToolBarArea.RightToolBarArea)
        tb_depth.setStyleSheet("QToolBar { padding: 0px; spacing: 0px; }")
        self._water_gauge = WaterZGauge(_MapWaterDepth(self))
        _depth_act = tb_depth.addWidget(self._water_gauge)
        _depth_act.setIcon(_OverlayIconsForDepth.water_icon(20))   # Ribbon Manager row icon
        _depth_act.setText("Water depth")
        from apps.methods.ribbon_system import RibbonMixin as _RM
        tb_depth._side = (self._water_gauge, 26, 0)       # turns with the ribbon
        tb_depth.orientationChanged.connect(lambda _o, t=tb_depth: _RM.ribbon_fit_side(t))
        _RM.ribbon_fit_side(tb_depth)
        self._vp_gauges = _ViewportGauges(self, pw)       # middle left/right of the view
        # Cycle Zones/Cull (Aug 21 2026,  "on zons we could
        # also cycle through the entries list, and show the zon box
        # highlighted, with right click options, this would be a
        # failback, other then clicking on the zon box" - moved here,
        # onto the ribbon directly, per his own follow-up: "the cycle
        # button should be on the ribbons") - left-click steps to the
        # next cull/zone box (wrapping around), right-click opens a
        # menu of every one loaded, to jump directly to any specific
        # one by name rather than only stepping through sequentially.
        # A plain QToolButton (not a _MapOverlayToggleButton like the
        # rest of this ribbon) added directly here rather than via
        # _move_overlay_buttons_to_ribbon - it's a real action button
        # with its own custom right-click menu, not a show/hide
        # overlay toggle, so it doesn't fit that shared mechanism.
        cycle_zones_btn = QToolButton()
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons as _OverlayIconsForCycle
        cycle_zones_btn.setIcon(_OverlayIconsForCycle.cycle_icon(24))
        cycle_zones_btn.setToolTip(
            "Left-click: step to the next cull/zone box.\n"
            "Right-click: pick one directly from a list.\n\n"
            "To resize a box: click and drag one of its corner\n"
            "spheres directly - no separate mode needed.")
        cycle_zones_btn.clicked.connect(self._cycle_selected_box)
        cycle_zones_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        cycle_zones_btn.customContextMenuRequested.connect(
            lambda pos, b=cycle_zones_btn: self._show_box_picker_menu(b))
        tb_overlays.addWidget(cycle_zones_btn)

        # Add/Delete/Save for Zon/Cull/Occlusion/Garage (Aug 21 2026,
        #  "we need to finish the add, del, save functions
        # for zon" / "lets build add, del for zon, cull, occu and ipl
        # changes" / "both if you can") moved off the ribbon entirely
        # (Aug 21 2026  own follow-up: "the new buttons,
        # +zon, -zon, and save zon... add these to the svg icon zon
        # button with a middle-click, and move those functions over.
        # Same with cull, occl, auzo, grge") - each is now on its own
        # overlay toggle button's own middle-click menu instead of a
        # separate ribbon button (see show_cull_btn/show_zone_btn/
        # show_occl_btn/show_grge_btn's own middle_clicked wiring,
        # and _show_overlay_middle_click_menu).

        # Undo (Aug 21 2026,  "we need an undo button
        # /ribbon icon") - the real, map-undo-aware handler (_on_
        # undo_clicked, wired to Ctrl+Z/Ctrl+Y earlier this session)
        # was previously only reachable from inside the Item Editor
        # Dialog's own [Apply][Undo][Close][Save] row, which only
        # opens by double-clicking an instance - genuinely not
        # reachable at all otherwise. Same left-click=undo, Shift+
        # click=redo behavior as that button already has.
        undo_ribbon_btn = QToolButton()
        undo_ribbon_btn.setIcon(_icon(self.icon_factory.undo_icon, 'undo_icon')(color=icon_color))
        undo_ribbon_btn.setToolTip("Undo (Shift+click to Redo)")
        undo_ribbon_btn.clicked.connect(self._on_undo_ribbon_clicked)
        tb_overlays.addWidget(undo_ribbon_btn)

        # Repair zero-scale instances (Aug 21 2026  own
        # real, worked VC/SA/SOL example lines: "so we need a function
        # to find and change 1, 1, 1, to 0, 0, 0 ... but I can see
        # there is an error in how I converted the files") - fixes any
        # instance whose scale is (0,0,0) back to (1,1,1) (see repair_
        # zero_scale_inst_fields's own docstring for the full, real
        # "why 0,0,0 and not the other direction" reasoning - a real
        # zero scale is never correct, unlike my own hand-typed
        # example which had the direction backwards).
        #
        # Scope note: a real, correct SA<->VC line converter (convert_
        # inst_fields, gta_dat_parser.py) is already built and tested
        # against my own real example lines, but wiring it up to
        # actually convert and write back every real instance in a
        # loaded world is a separate, much larger piece (needs its own
        # real INST-section write-back infrastructure, unlike zone/
        # cull/occl/grge's own much smaller sections) - not attempted
        # this turn; this button covers the repair half only.
        repair_scale_btn = QToolButton()
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons as _OverlayIconsForRepair
        repair_scale_btn.setIcon(_OverlayIconsForRepair.repair_scale_icon(24))
        repair_scale_btn.setToolTip(
            "Left-click: fix any instance whose scale is (0,0,0) back to (1,1,1).\n"
            "Right-click: choose a direction (also covers 1,1,1 -> 0,0,0).")
        repair_scale_btn.clicked.connect(self._repair_zero_scale_instances)
        repair_scale_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        repair_scale_btn.customContextMenuRequested.connect(
            lambda pos, b=repair_scale_btn: self._show_repair_scale_menu(b))
        tb_overlays.addWidget(repair_scale_btn)

        # Convert VC<->SA/SOL INST format (Aug 21 2026, .per:
        # "having VC -> SA or SA -> VC is something I need, under a
        # convertion SVG icon, right clicked for options, and selected
        # ipl files to convert, anywhere on the harddrive or loaded
        # ipl browser list") - both left and right-click open the same
        # options menu (see _show_convert_ipl_menu) - there's no
        # separate "primary" action for this button the way Cycle's
        # own left-click-to-advance has, so gating the menu behind
        # right-click only would make it invisible to anyone who only
        # ever left-clicks.
        convert_btn = QToolButton()
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons as _OverlayIconsForConvert
        convert_btn.setIcon(_OverlayIconsForConvert.convert_icon(24))
        convert_btn.setToolTip(
            "Convert IPL INST sections between VC and SA/SOL format.\n"
            "Click for options: pick files from disk, or from the\n"
            "currently loaded world.")
        convert_btn.clicked.connect(lambda: self._show_convert_ipl_menu(convert_btn))
        tb_overlays.addWidget(convert_btn)

        # Optimize Load Order (Aug 21 2026,  "if the model
        # names, col names, and ide/ipl entries loaded in the same
        # order the game spends less work matching them up, result is
        # the game loads and renders faster. the SOL files have been
        # organized this way, with 3 times the data, it loads faster
        # then standard VC") - confirmed, not guessed: gta_vc.dat's
        # own real header comments say this in as many words ("Load
        # IDEs first, then the models and after that the IPLs...
        # everything is loaded on a per directory basis and in
        # alphabetical order to improve the speed of loading"). See
        # optimize_dat_load_order's own docstring for the full, real
        # algorithm and its own honest scope limit (reorders the real
        # .dat file's own directive lines only, not real model/
        # instance order within an IDE/IPL file's own real contents).
        optimize_btn = QToolButton()
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons as _OverlayIconsForOptimize
        optimize_btn.setIcon(_OverlayIconsForOptimize.optimize_icon(24))
        optimize_btn.setToolTip(
            "Reorder this world's own loaded .dat file's IDE/IPL/COLFILE/IMG\n"
            "lines to match Rockstar's own documented convention: grouped by\n"
            "directory, alphabetical within each - the same real pattern SOL's\n"
            "own files already use.")
        optimize_btn.clicked.connect(self._optimize_dat_load_order_clicked)
        tb_overlays.addWidget(optimize_btn)

        # Search loaded instances by model name (Sep 5 2026, per
        #  : "a search [O'] function on the ribbon bar to find a
        # model name so I can see the IPL line") - directly reuses
        # the same real fix just made to the wrong-instance lookup
        # bug (matches by name, disambiguates results the same way
        # the Object Browser's own model-click already does).
        search_btn = QToolButton()
        search_btn.setIcon(_OverlayIconsForOptimize.search_model_icon(24))
        search_btn.setToolTip("Search loaded instances by model name")
        search_btn.clicked.connect(self._show_model_search_dialog)
        tb_overlays.addWidget(search_btn)

        # Force Prelighting (Sep 5 2026,  "showing dark
        # models. I think some models might not be loading the
        # prelighting, so we need a prelighting on/off SVG button") -
        # wires DFFViewport.set_prelight, an override that already
        # existed and is already used by Vehicle Workshop/Model Viewer
        # (self._use_prelight), just never wired into this workshop's
        # own ribbon. Forces baked vertex-colour lighting on
        # regardless of a model's own geom_flags detection, which only
        # ever uses its first geometry's flags for the whole merged
        # model (same known simplification as DFFViewport.load_all_
        # geometries) - multi-geometry objects where a later geometry
        # actually needs prelighting but the first one doesn't can end
        # up rendered dark via real-time lighting instead. This is a
        # manual escape hatch for exactly that case, not an attempt at
        # fixing the underlying per-geometry detection.
        prelight_btn = QToolButton()
        prelight_btn.setCheckable(True)
        prelight_btn.setIcon(_OverlayIconsForOptimize.prelight_icon(24))
        prelight_btn.setToolTip(
            "Force baked vertex-colour (pre-lit) lighting on for every\n"
            "model, instead of each model's own auto-detected lighting -\n"
            "use this if a model is rendering unexpectedly dark.")
        prelight_btn.toggled.connect(self._on_toggle_force_prelight)
        prelight_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        prelight_btn.customContextMenuRequested.connect(
            lambda _p, b=prelight_btn: self._prelight_menu(b.mapToGlobal(_p)))
        prelight_btn.setToolTip(prelight_btn.toolTip() + "\nRight-click: generate world prelighting with shadows.")
        tb_overlays.addWidget(prelight_btn)
        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons as _OverlayIconsTerrain
        _act(tb_overlays, "Terrain tools", lambda color=None: _OverlayIconsTerrain.terrain_icon(20),
             self._toggle_terrain,
             checkable=True, attr='_terrain_act')

        _act(tb_rend, "Render Settings",
             _icon(self.icon_factory.render_settings_icon, 'render_settings_icon'),
             lambda checked=False: self._show_workshop_settings('Render'))
        _act(tb_rend, "Toggle Mesh",
             _icon(self.icon_factory.mesh_icon, 'toggle_mesh_icon'),
             self._on_toggle_mesh_col,
             checkable=True, checked=True, attr='_view_mesh_act')
        self._view_mesh_act.setToolTip(
            "Checked: show model mesh. Unchecked: show collision instead.")
        _act(tb_rend, "Toggle Backface",
             _icon(self.icon_factory.toggle_backface_icon, 'toggle_backface_icon'),
             lambda v: pw.set_backface(v),
             checkable=True, attr='_backface_act')
        _act(tb_rend, "Cycle Render Style",
             _icon(self.icon_factory.render_style_icon, 'render_style_icon'),
             self._cycle_view_render_style)
        tb_rend.addSeparator()
        _act(tb_rend, "Toggle Shading",
             _icon(self.icon_factory.shading_sphere_icon, 'shading_icon'),
             lambda v: self._toggle_viewport_shading(v),
             checkable=True, checked=True, attr='_shading_act')
        _act(tb_rend, "Light Setup",
             _icon(self.icon_factory.light_icon, 'light_setup_icon'),
             self._open_light_setup_dialog, attr='_light_setup_act')
        tb_rend.addSeparator()
        _act(tb_rend, "Open GL Model Viewer (hardware 3D)",
             _icon(self.icon_factory.viewport_icon, 'gl_viewer_icon'),
             self._open_gl_viewer, attr='_gl_viewer_act')
        tb_rend.addSeparator()
        # LOD Test (Aug 1 2026)
        _lod_test_act = _act(tb_rend, "LOD Test",
             _icon(self.icon_factory.lod_test_icon, 'lod_test_icon'),
             self._on_lod_test_toggled,
             checkable=True, attr='_lod_test_act')
        _lod_test_act.setToolTip(
            "Live LOD switching test - draws a circle around the mouse\n"
            "cursor (radius set in Settings > Map Assets); models\n"
            "inside switch to one detail level, models outside switch\n"
            "to the other, updated in real time as the mouse moves.")

        # Store toolbar refs
        self._tb_selection = tb_sel
        self._tb_snap      = tb_snap
        self._tb_geometry  = tb_geo
        self._tb_nav       = tb_nav
        self._tb_render    = tb_rend
        self._tb_overlays  = tb_overlays
        _names = {cycle_zones_btn: "Cycle boxes", undo_ribbon_btn: "Undo / Redo",
                  repair_scale_btn: "Repair scale", convert_btn: "Convert IPL",
                  optimize_btn: "Optimize DAT", search_btn: "Search instances",
                  prelight_btn: "Prelight"}
        for _a in tb_overlays.actions():                     # Ribbon Manager rows: icon + name
            _wd = tb_overlays.widgetForAction(_a)
            if _wd in _names:
                _a.setIcon(_wd.icon())
                _a.setText(_names[_wd])

        # DFF-only actions — disabled until a model is loaded
        self._dff_only_actions = [
            self._sel_vert_act, self._sel_edge_act,
            self._sel_face_act, self._sel_poly_act,
            self._backface_cull_act, self._front_paint_act,
            self._shading_act, self._light_setup_act,
        ]
        # Legacy compat for code that checks _dff_only_toolbar_btns
        self._dff_only_toolbar_btns = []

        # Compatibility attrs for code using old QPushButton names
        self._sel_vert_btn   = None
        self._sel_edge_btn   = None
        self._sel_face_btn   = None
        self._sel_poly_btn   = None

    def _toolbar_context_menu(self, toolbar, pos): #vers 5
        """Right-click on any toolbar: shared menu (methods/ribbon_system.py)."""
        from PyQt6.QtWidgets import QToolBar
        from apps.methods.toolbar_menu import standard_toolbar_menu
        try:
            import json
            scale = json.loads((self._ribbon_config_path()).read_text()).get('icon_scale', 20)
        except Exception:
            scale = 20
        menu = standard_toolbar_menu(self, self._inner_mw.findChildren(QToolBar), scale)
        menu.exec(toolbar.mapToGlobal(pos))

    def _switch_icon_set(self, key: str): #vers 2
        """Save icon set choice and rebuild toolbars to apply it."""
        try:
            import json
            path = self._ribbon_config_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['icon_set'] = key
            path.write_text(json.dumps(data, indent=2))
        except Exception as _e:
            print(f"[icon_set] save failed: {_e}")
        # Rebuild toolbars live
        self._rebuild_toolbars()

    def _rebuild_toolbars(self): #vers 2
        """Remove all existing toolbars and rebuild with the current
        icon set.

         fix (Aug 20 2026,  "The New Icons on the IPL
        Control pane, can be moved to the ribbon") - the overlay
        toggle buttons (Water/Radar/Tcyc/etc.) now live as real child
        widgets of the "Overlays" toolbar (_move_overlay_buttons_to_
        ribbon), not QActions rebuilt fresh by _build_toolbars itself
        - deleteLater() on that toolbar would have deleted those real
        buttons right along with it, permanently, the next time
          changed icon sets. Re-parents them to self first (kept
        alive, off-screen) before any toolbar is destroyed, then
        re-adds them to the freshly rebuilt "Overlays" toolbar
        afterwards."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        for btn in getattr(self, '_overlay_toggle_buttons', []):
            btn.setParent(self)
        from PyQt6.QtWidgets import QToolBar
        for tb in list(mw.findChildren(QToolBar)):
            mw.removeToolBar(tb)
            tb.deleteLater()
        self._ribbon_actions = []
        icon_color = self._get_icon_color()
        self._build_toolbars(mw, icon_color)
        self._apply_custom_icons()
        self._move_overlay_buttons_to_ribbon()
        self._apply_ipl_controls_display_style()
        self._set_status(f"Icon set applied")

    def _apply_icon_scale(self, px: int): #vers 2
        """Apply icon size to all toolbars live and persist it."""
        mw = getattr(self, '_inner_mw', None)
        if mw:
            from PyQt6.QtWidgets import QToolBar
            from PyQt6.QtCore import QSize
            for tb in mw.findChildren(QToolBar):
                tb.setIconSize(QSize(px, px))
        try:
            import json
            path = self._ribbon_config_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['icon_scale'] = px
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass

    def _ribbon_config_path(self): #vers 1
        """Ribbon layout, presets and custom icons file."""
        return _model_workshop_config_dir() / 'model_workshop.json'

    def _get_or_create_hidden_toolbar(self): #vers 1
        """Return the special "Hidden" toolbar, creating it, if it
        doesn't exist yet."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return None
        existing = getattr(self, '_hidden_toolbar', None)
        if existing is not None:
            return existing
        from PyQt6.QtWidgets import QToolBar
        tb = QToolBar("Hidden", mw)
        tb.setObjectName("Hidden")
        tb.setMovable(False)
        tb.setFloatable(False)
        mw.addToolBar(Qt.ToolBarArea.TopToolBarArea, tb)
        tb.setVisible(False)
        tb.visibilityChanged.connect(
            lambda visible, t=tb: t.setVisible(False) if visible else None)
        self._hidden_toolbar = tb
        return tb

    def _save_toolbar_state(self): #vers 4
        """Save QMainWindow toolbar state to model_workshop.json (after restore only)."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None or not getattr(self, '_toolbar_restored', False):
            return                                  # never overwrite before the saved layout loads
        try:
            import json
            path = self._ribbon_config_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['toolbar_state'] = mw.saveState(self._RIBBON_LAYOUT_VERSION).toHex().data().decode()
            data['toolbar_state_version'] = self._RIBBON_LAYOUT_VERSION
            path.write_text(json.dumps(data, indent=2))
            self._set_status("Ribbon config saved")
            main_wnd = getattr(self, 'main_window', None)
            if main_wnd and hasattr(main_wnd, 'log_message'):
                main_wnd.log_message("Model Workshop: Ribbon config saved")
        except Exception as _e:
            print(f"[ModelWorkshop] _save_toolbar_state error: {_e}")

    def _restore_toolbar_state(self): #vers 9
        """Restore QMainWindow toolbar state from model_workshop.json.
        Uses an explicit layout version - bumped whenever ribbons are
        added/removed/renamed - so a stale save from an older ribbon
        layout is cleanly rejected instead of silently failing to
        restore (Qt's own toolbar-name hashing does this invisibly and
        without any way to detect success/failure).

         fix (Aug 20 2026,  "I've noticed moving icons
        to hidden, and save, these movements dont get saved") - the
        real "Hidden" toolbar is only ever created lazily, the first
        time the Ribbon Manager dialog itself opens (_get_or_create_
        hidden_toolbar) - it genuinely didn't exist yet at either of
        this method's own real call times (both fire from startup
        timers, well before   would have opened that dialog even
        once this session). Qt's own restoreState() can only
        reassociate a saved button with a toolbar object that already
        exists at the moment it's called - with no real "Hidden"
        toolbar object to hand icons back to, any icon   had
        moved there and saved was silently dropped back to whichever
        toolbar it started in, every single time. Creating it here
        first, unconditionally, before restoreState() runs, ensures
        it always exists to receive them correctly."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        self._get_or_create_hidden_toolbar()
        try:
            import json
            from PyQt6.QtCore import QByteArray
            path = self._ribbon_config_path()
            if not path.exists():
                return
            data = json.loads(path.read_text())
            if 'icon_scale' in data:                      # saved ribbon icon size
                from PyQt6.QtCore import QSize
                from PyQt6.QtWidgets import QToolBar
                for tb in mw.findChildren(QToolBar):
                    tb.setIconSize(QSize(data['icon_scale'], data['icon_scale']))
            state_hex = data.get('toolbar_state')
            saved_version = data.get('toolbar_state_version')
            if state_hex and saved_version == self._RIBBON_LAYOUT_VERSION:
                ok = mw.restoreState(QByteArray.fromHex(state_hex.encode()),
                                      self._RIBBON_LAYOUT_VERSION)
                if ok:
                    self._set_status("Ribbon config loaded")
                    main_wnd = getattr(self, 'main_window', None)
                    if main_wnd and hasattr(main_wnd, 'log_message'):
                        main_wnd.log_message("Model Workshop: Ribbon config loaded")
                else:
                    print("[ModelWorkshop] _restore_toolbar_state: restoreState() returned False")
            elif state_hex:
                print(f"[ModelWorkshop] Saved ribbon layout is from an older version "
                      f"({saved_version} != {self._RIBBON_LAYOUT_VERSION}) - skipping, "
                      f"will save fresh on next change.")
        except Exception as _e:
            print(f"[ModelWorkshop] _restore_toolbar_state error: {_e}")
        finally:
            # Safety net: restoreState() can leave a ribbon fully hidden
            # (e.g. saved mid-drag, floating off-screen, squeezed out) with
            # no user-facing 'closed' state for these ribbons to have meant
            # intentionally - so force every one of them visible no matter
            # what happened above. Only position/floating/row should be
            # affected by the saved state, never full visibility.
            for tb in (getattr(self, '_tb_selection', None),
                       getattr(self, '_tb_snap', None),
                       getattr(self, '_tb_geometry', None),
                       getattr(self, '_tb_nav', None),
                       getattr(self, '_tb_render', None),
                       getattr(self, '_tb_overlays', None)):
                if tb is not None:
                    tb.setVisible(True)
                    tb.toggleViewAction().setChecked(True)
            self._toolbar_restored = True           # saves allowed from now on

    def _restore_outer_layout(self): #vers 3
        """Shared dock layout restore, then Model Name / IDE-TXD ribbon locations."""
        DockPanelsMixin._restore_outer_layout(self)

        # Restore Model Name / IDE-TXD ribbon locations (middle panel vs
        # right panel ribbon stack) - separate from the QMainWindow toolbar
        # state above since these toolbars aren't always parented to
        # _inner_mw.
        try:
            import json
            path = self._ribbon_config_path()
            if path.exists():
                data = json.loads(path.read_text())
                saved_locations = data.get('info_ribbon_locations', {})
                for key, saved_location in saved_locations.items():
                    if saved_location != self._info_ribbon_location.get(key):
                        self._move_info_ribbon(key, saved_location)
        except Exception as _e:
            print(f"[ModelWorkshop] Info ribbon location restore error: {_e}")

    def _create_paint_bar(self): #vers 3
        """Floating paint bar — QWidget child of preview_widget, sits at top of viewport.
        Called once from _create_right_panel after preview_widget is created."""
        vp = self.preview_widget
        ic = self._get_icon_color()

        bar = QWidget(vp)
        bar.setObjectName("paint_bar")
        bar.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        bar.setStyleSheet(
            "QWidget#paint_bar { background:palette(base); border-bottom:2px solid #ff8c00; }"
            "QLabel  { color:#ddd; background:transparent; }"
            "QComboBox { background:palette(base); color:palette(windowText); border:1px solid palette(mid); }"
            "QPushButton { background:palette(base); color:palette(windowText); border:1px solid palette(mid); border-radius:3px; }"
            "QPushButton:hover   { background:#353548; }"
            "QPushButton:checked { background:#ff8c00; color:#000; border:1px solid #ff8c00; }"
        )
        bar.setFixedHeight(34)

        lay = QHBoxLayout(bar)
        lay.setContentsMargins(6, 3, 6, 3)
        lay.setSpacing(4)

        lay.addWidget(QLabel("Mat:"))

        self.paint_swatch = QLabel()
        self.paint_swatch.setFixedSize(16, 16)
        self.paint_swatch.setStyleSheet(
            "background:#808080; border:1px solid palette(mid); border-radius:2px;")
        lay.addWidget(self.paint_swatch)

        self.paint_mat_combo = QComboBox()
        self.paint_mat_combo.setFixedHeight(26)
        self.paint_mat_combo.setMinimumWidth(160)
        self.paint_mat_combo.setMaximumWidth(260)
        lay.addWidget(self.paint_mat_combo)

        lay.addSpacing(4)

        def _tbtn(attr, icon_fn, tip, tool):  #vers 1
            b = QPushButton()
            try:
                b.setIcon(getattr(self.icon_factory, icon_fn)(color=ic))
            except Exception:
                b.setText(tool[0].upper())
            b.setIconSize(QSize(16, 16))
            b.setFixedSize(28, 28)
            b.setToolTip(tip)
            b.setCheckable(True)
            b.clicked.connect(lambda *_: self._set_paint_tool(tool))
            setattr(self, attr, b)
            lay.addWidget(b)

        _tbtn('tool_paint_btn',   'paint_icon',   'Paint faces',   'paint')
        _tbtn('tool_dropper_btn', 'dropper_icon', 'Dropper',       'dropper')
        _tbtn('tool_fill_btn',    'fill_icon',    'Flood fill',    'fill')
        if self.tool_paint_btn:
            self.tool_paint_btn.setChecked(True)

        self.paint_undo_btn = QPushButton()
        try:
            self.paint_undo_btn.setIcon(self.icon_factory.undo_paint_icon(color=ic))
        except Exception:
            self.paint_undo_btn.setText("↩")
        self.paint_undo_btn.setIconSize(QSize(16, 16))
        self.paint_undo_btn.setFixedSize(28, 28)
        self.paint_undo_btn.setToolTip("Undo last paint op")
        self.paint_undo_btn.setEnabled(False)
        self.paint_undo_btn.clicked.connect(self._undo_last_action)
        lay.addWidget(self.paint_undo_btn)

        lay.addStretch()

        self.paint_exit_btn = QPushButton("✕")
        self.paint_exit_btn.setFixedSize(28, 28)
        self.paint_exit_btn.setToolTip("Exit paint mode")
        self.paint_exit_btn.setStyleSheet(
            "color:#ff6b35; font-weight:bold; background:palette(base); border:1px solid palette(mid); border-radius:3px;")
        self.paint_exit_btn.clicked.connect(self._exit_paint_mode)
        lay.addWidget(self.paint_exit_btn)

        self.paint_toolbar = bar
        bar.setGeometry(0, 0, vp.width(), 34)
        bar.hide()

        # Reposition bar when viewport resizes
        _orig = vp.resizeEvent
        def _on_vp_resize(event, _o=_orig, _bar=bar, _vp=vp):  #vers 1
            _o(event)
            if _bar.isVisible():
                _bar.setGeometry(0, 0, _vp.width(), 34)
                _bar.raise_()
        vp.resizeEvent = _on_vp_resize


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

    def eventFilter(self, obj, event): #vers 2
        """Double-click on a quad pane maximizes/restores it"""
        from PyQt6.QtCore import QEvent
        if event.type() == QEvent.Type.Resize:
            top_row_widget = getattr(self, '_ob_top_row_widget', None)
            if top_row_widget is not None and obj is top_row_widget:
                self._update_mode_button_style()
            collapsible_rows = getattr(self, '_collapsible_button_rows', None)
            if collapsible_rows and obj in collapsible_rows:
                self._update_button_row_collapse(obj)
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
        plus the whole opposite row covers both nesting levels."""
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

    def _sync_quad_from_main(self): #vers 2
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
            if hasattr(pane, 'set_render_mode') and hasattr(src, '_mode'):
                pane.set_render_mode(src._mode)
            pane._auto_fit()
            pane.update()

    def _save_quad_layout(self): #vers 1
        """Persist per-pane view assignment + splitter sizes."""
        import json, os
        panes = getattr(self, '_quad_panes', None)
        if not panes:
            return
        try:
            # App-folder-relative, not ~/.config (Aug 20 2026)
            cfg_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))   # Map_Editor/
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

    def _load_quad_layout(self): #vers 1
        """Restore per-pane view assignment + splitter sizes, if saved."""
        import json, os
        panes = getattr(self, '_quad_panes', None)
        if not panes:
            return
        try:
            cfg_path = str(_model_workshop_config_dir() / 'model_workshop.json')
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

    # --- World viewport, LOD, ribbon/menu framework (ported from map_workshop_old_version.py) ---

    def _create_centre_panel(self): #vers 4
        """Map Workshop's own centre panel - status bar only. No DP5-style
        bitmap canvas here (removed Jul 31 2026)"""
        panel = QWidget()
        self._centre_panel = panel
        layout = QVBoxLayout(panel)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Status bar - built here but NOT added to this panel's own layout.
        # It becomes outer_mw's native status bar instead (see setup_ui) -
        # QMainWindow's built-in status bar always spans the full window
        # width, whereas embedding it in this panel meant it only got
        # whatever width was left after the right-side dock widgets took
        # their share, squeezing/covering its right-aligned permanent
        # widgets.
        self._status_bar = QStatusBar()
        self._status_bar.setSizeGripEnabled(False)
        self._status_bar.setFixedHeight(22)
        self._status_bar.setVisible(self.map_settings.get('show_statusbar'))

        return panel

    def _get_tool_menu_style(self) -> str: #vers 1
        """Read menu_style from map_settings."""
        return self.map_settings.get('menu_style', 'dropdown')

    def _apply_menu_bar_style(self): #vers 3
        """Apply font size, height and colours to the topbar menubar.
        Uses explicit colours so it stays readable regardless of app theme.
        """
        mb = getattr(self, '_menu_bar', None)
        if not mb:
            return
        bar_h  = self.map_settings.get('menu_bar_height', 22)
        bar_fs = self.map_settings.get('menu_bar_font_size', 9)
        dd_fs  = self.map_settings.get('menu_dropdown_font_size', 9)

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
            show = (self.map_settings.get('show_menubar', False) and
                    self.map_settings.get('menu_style', 'dropdown') == 'topbar')
            if show:
                c.setMinimumHeight(0)
                c.setMaximumHeight(bar_h)
            # Don't touch height here if hiding — setup_ui and set_menu_orientation handle that


    def set_menu_orientation(self, style: str): #vers 5
        """Switch DP5 menu between 'topbar' (internal) and 'dropdown' (host menubar).
        When docked the internal bar is always suppressed regardless of style —
        imgfactory's top bar owns the menus via ToolMenuMixin injection.
        """
        self.map_settings.set('menu_style', style)
        self.map_settings.set('show_menubar', style == 'topbar')

        container = getattr(self, '_menu_bar_container', None) or getattr(self, '_menu_bar', None)
        if container:
            if style == 'topbar' and self.standalone_mode:
                # Only show internal bar in standalone mode
                container.setMinimumHeight(0)
                container.setMaximumHeight(16777215)
                container.setVisible(True)
                container.updateGeometry()
                self._apply_menu_bar_style()
            else:
                # Docked always hides internal bar; dropdown mode always hides it too
                container.setVisible(False)
                container.setMinimumHeight(0)
                container.setMaximumHeight(0)
                container.setFixedHeight(0)

        # Notify imgfactory to inject/remove tool menu when docked
        if not self.standalone_mode:
            mw = getattr(self, 'main_window', None)
            if mw and hasattr(mw, 'menu_bar_system'):
                if style == 'dropdown':
                    mw.menu_bar_system._inject_tool_menu(self)
                else:
                    mw.menu_bar_system._remove_tool_menu()


    #    File I/O                                                               



#  Public factory function

    # --- Shared icon-shape rendering (ported from map_workshop_old_version.py) ---

    def _paint_variant_shape(self, p, kind, size, color): #vers 2
        """Draw a simple, reliable shape for an Annotate-ribbon variant
        that has no existing SVG icon - avoids depending on the system
        font having specific Unicode glyphs (arrows, shape symbols,
        circled digits), which isn't guaranteed and was the cause of
        blank/invisible buttons."""
        from PyQt6.QtGui import QPolygon
        qc = QColor(color)
        m = max(2, int(size * 0.15))
        pen_w = max(1, size // 10)
        p.setPen(QPen(qc, pen_w))
        p.setBrush(Qt.BrushStyle.NoBrush)

        if kind == 'arrow':
            p.drawLine(m, size - m, size - m, m)
            ah = size * 0.3
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawPolygon(QPolygon([
                QPoint(size - m, m), QPoint(int(size - m - ah), m),
                QPoint(size - m, int(m + ah))]))
        elif kind == 'double_arrow':
            cy = size // 2
            p.drawLine(m, cy, size - m, cy)
            ah = size * 0.22
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawPolygon(QPolygon([
                QPoint(size - m, cy), QPoint(int(size - m - ah), int(cy - ah)),
                QPoint(int(size - m - ah), int(cy + ah))]))
            p.drawPolygon(QPolygon([
                QPoint(m, cy), QPoint(int(m + ah), int(cy - ah)),
                QPoint(int(m + ah), int(cy + ah))]))
        elif kind == 'marker_pen':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawRect(m, int(size * 0.4), size - 2*m, int(size * 0.2))
        elif kind == 'marker_rect':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawRect(m, m, size - 2*m, size - 2*m)
        elif kind == 'marker_ellipse':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawEllipse(m, m, size - 2*m, size - 2*m)
        elif kind in ('text_pointer', 'text_arrow'):
            font = QFont(); font.setPixelSize(int(size * 0.5)); font.setBold(True)
            p.setFont(font); p.setPen(qc)
            p.drawText(0, 0, int(size * 0.55), size,
                       Qt.AlignmentFlag.AlignCenter, "A")
            if kind == 'text_pointer':
                p.drawLine(int(size*0.55), int(size*0.7), size - m, int(size*0.7))
            else:
                p.drawLine(int(size*0.55), int(size*0.65), size - m - 3, m + 3)
                p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
                ah = size * 0.16
                p.drawPolygon(QPolygon([
                    QPoint(size - m, m), QPoint(int(size - m - ah), m),
                    QPoint(size - m, int(m + ah))]))
        elif kind in ('number', 'number_pointer', 'number_arrow'):
            d = size - 2*m
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawEllipse(m, m, d, d)
            font = QFont(); font.setPixelSize(int(size * 0.45)); font.setBold(True)
            p.setFont(font); p.setPen(QColor('#ffffff'))
            p.drawText(m, m, d, d, Qt.AlignmentFlag.AlignCenter, "1")
        elif kind == 'pixelate':
            cell = size // 4
            for row in range(4):
                for col in range(4):
                    shade = qc.lighter(130) if (row+col) % 2 == 0 else qc.darker(120)
                    p.setPen(Qt.PenStyle.NoPen); p.setBrush(shade)
                    p.drawRect(col*cell, row*cell, cell, cell)
        elif kind == 'sharpen':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawPolygon(QPolygon([
                QPoint(size//2, m), QPoint(size - m, size//2),
                QPoint(size//2, size - m), QPoint(m, size//2)]))
        elif kind == 'duplicate':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            off = int(size * 0.18)
            sq = size - m - off
            p.drawRect(m, m, sq - m, sq - m)
            p.drawRect(m + off, m + off, sq - m, sq - m)
        elif kind == 'dot':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            d = int(size * 0.6)
            off = (size - d) // 2
            p.drawEllipse(off, off, d, d)
        elif kind == 'bullet':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            d = int(size * 0.35)
            off = (size - d) // 2
            p.drawEllipse(off, off, d, d)
        elif kind == 'active_zoom':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            lens_d = int(size * 0.55)
            p.drawEllipse(m, m, lens_d, lens_d)
            p.drawLine(m + int(lens_d*0.85), m + int(lens_d*0.85), size - m, size - m)
            # Crosshair inside the lens to suggest cursor-follow
            cx, cy = m + lens_d // 2, m + lens_d // 2
            p.drawLine(cx - lens_d//4, cy, cx + lens_d//4, cy)
            p.drawLine(cx, cy - lens_d//4, cx, cy + lens_d//4)
        elif kind == 'canvas_toggle':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            p.drawRect(m, m + 2, size - 2*m, size - 2*m - 4)
            p.drawLine(m, size - m - 2, size - m, m + 2)
        elif kind == 'lod_toggle':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            big = int(size * 0.65)
            small = int(size * 0.4)
            p.drawRect(m, m, big, big)
            p.drawRect(size - m - small, size - m - small, small, small)
        elif kind in ('eye_visible', 'eye_hidden'):
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            cx, cy = size // 2, size // 2
            eye_w = size - 2*m
            eye_h = int(eye_w * 0.55)
            rect_x, rect_y = m, cy - eye_h // 2
            # Eye outline as an ellipse (approximated with drawArc-style
            # via drawEllipse, simplest reliable cross-Qt-version option)
            p.drawEllipse(rect_x, rect_y, eye_w, eye_h)
            pupil_d = max(2, int(eye_h * 0.5))
            p.setBrush(QBrush(qc))
            p.drawEllipse(cx - pupil_d//2, cy - pupil_d//2, pupil_d, pupil_d)
            if kind == 'eye_hidden':
                p.setBrush(Qt.BrushStyle.NoBrush)
                p.drawLine(m, m, size - m, size - m)
        elif kind == 'cull_boxes':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            front = int(size * 0.6)
            offset = int(size * 0.22)
            fx, fy = m, size - m - front
            p.drawRect(fx, fy, front, front)
            bx, by = fx + offset, fy - offset
            p.drawRect(bx, by, front, front)
            for dx, dy in ((0, 0), (front, 0), (0, front), (front, front)):
                p.drawLine(fx + dx, fy + dy, bx + dx, by + dy)
        elif kind == 'list_all':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            step = (size - 2*m) / 3.0
            for i in range(3):
                y = int(m + step * i + step/2)
                p.drawLine(m, y, size - m, y)
        elif kind == 'bars_most_used':
            p.setPen(QPen(qc, pen_w)); p.setBrush(QBrush(qc))
            bar_w = max(2, (size - 2*m) // 4)
            gap = max(1, bar_w // 2)
            heights = (0.4, 0.7, 1.0)
            x = m
            for h in heights:
                bar_h = int((size - 2*m) * h)
                p.drawRect(x, size - m - bar_h, bar_w, bar_h)
                x += bar_w + gap
        elif kind == 'star_filled':
            from PyQt6.QtGui import QPolygonF
            p.setPen(QPen(qc, pen_w)); p.setBrush(QBrush(qc))
            cx, cy = size / 2.0, size / 2.0
            outer_r, inner_r = (size - 2*m) / 2.0, (size - 2*m) / 4.5
            pts = []
            for i in range(10):
                ang = -math.pi/2 + i * math.pi/5
                r = outer_r if i % 2 == 0 else inner_r
                pts.append(QPointF(cx + r*math.cos(ang), cy + r*math.sin(ang)))
            p.drawPolygon(QPolygonF(pts))
        elif kind == 'box_generic':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            box = size - 2*m
            p.drawRect(m, m, box, box)
            p.drawLine(m, m + box//3, size - m, m + box//3)
        elif kind == 'tab_ide':
            from PyQt6.QtGui import QPolygonF
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            fold = int(size * 0.28)
            pts = [QPointF(m, m), QPointF(size - m - fold, m), QPointF(size - m, m + fold),
                   QPointF(size - m, size - m), QPointF(m, size - m)]
            p.drawPolygon(QPolygonF(pts))
            p.drawLine(size - m - fold, m, size - m - fold, m + fold)
            p.drawLine(size - m - fold, m + fold, size - m, m + fold)
            step = (size - 2*m) / 3.5
            for i in range(2):
                y = int(m + fold + step * (i + 1))
                p.drawLine(m + 2, y, size - m - 2, y)
        elif kind == 'tab_ipl':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            cx = size / 2.0
            top = m
            tip_y = size - m
            r = (size - 2*m) * 0.32
            cy = top + r + 1
            p.drawEllipse(QPointF(cx, cy), r, r)
            from PyQt6.QtGui import QPolygonF
            pts = [QPointF(cx - r * 0.6, cy + r * 0.6), QPointF(cx, tip_y),
                   QPointF(cx + r * 0.6, cy + r * 0.6)]
            p.drawPolygon(QPolygonF(pts))
            p.setBrush(QBrush(qc))
            p.drawEllipse(QPointF(cx, cy), r * 0.35, r * 0.35)
        elif kind == 'tab_dat':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            step = (size - 2*m) / 2.5
            knob_x = (0.3, 0.65, 0.4)
            for i in range(3):
                y = int(m + step * i + step * 0.5)
                p.drawLine(m, y, size - m, y)
                kx = int(m + (size - 2*m) * knob_x[i])
                p.setBrush(QBrush(qc))
                p.drawEllipse(QPointF(kx, y), 2, 2)
                p.setBrush(Qt.BrushStyle.NoBrush)
        elif kind == 'tab_img':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            band_h = (size - 2*m) / 3.2
            for i in range(3):
                y = int(m + i * (band_h + 2))
                p.drawRect(m, y, size - 2*m, int(band_h))
        elif kind in ('chevron_left', 'chevron_left2', 'chevron_right', 'chevron_right2'):
            p.setPen(QPen(qc, max(2, pen_w))); p.setBrush(Qt.BrushStyle.NoBrush)
            cy = size // 2
            half_h = (size - 2*m) // 2
            is_right = kind.startswith('chevron_right')
            is_double = kind.endswith('2')

            def _draw_chevron(cx): #vers 1
                if is_right:
                    p.drawLine(cx - half_h//2, cy - half_h, cx + half_h//2, cy)
                    p.drawLine(cx + half_h//2, cy, cx - half_h//2, cy + half_h)
                else:
                    p.drawLine(cx + half_h//2, cy - half_h, cx - half_h//2, cy)
                    p.drawLine(cx - half_h//2, cy, cx + half_h//2, cy + half_h)

            if is_double:
                offset = max(2, size // 5)
                _draw_chevron(size//2 - offset)
                _draw_chevron(size//2 + offset)
            else:
                _draw_chevron(size // 2)

    def _render_variant_icon(self, icon_kind, icon_method, size, icon_color,
                              has_menu: bool = False) -> QIcon: #vers 2
        """Render one square icon for a dropdown-button variant: the
        existing SVG icon if icon_method is given, otherwise a shape
        drawn via _paint_variant_shape for icon_kind. If has_menu, bakes
        a small triangle into the top-right corner instead of relying
        on Qt's separate style-drawn menu-indicator, which reads as a
        detached arrow rather than part of the icon."""
        from PyQt6.QtGui import QPolygon
        px = QPixmap(size, size)
        px.fill(Qt.GlobalColor.transparent)
        p = QPainter(px)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)

        if icon_method:
            try:
                ico = getattr(SVGIconFactory, icon_method)(size, icon_color)
                ico.paint(p, 0, 0, size, size)
            except Exception:
                pass
        elif icon_kind:
            self._paint_variant_shape(p, icon_kind, size, icon_color)

        if has_menu:
            p.setPen(Qt.PenStyle.NoPen)
            p.setBrush(QColor(icon_color))
            a = max(5, size // 3)
            p.drawPolygon(QPolygon([
                QPoint(size - a, 0), QPoint(size, 0), QPoint(size, a)]))
        p.end()
        return QIcon(px)
