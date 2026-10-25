#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_toolbar_func.py - Version: 4
# X-Seti - October 14 2026 - IMG Factory 1.6 - Vehicle Workshop toolbar

"""
Toolbar, file loading, paint, animation and overlay actions.
"""

##class _ToolbarMixin: -
# _apply_theme
# _auto_load_shared_txds
# _collect_needed_textures
# _cycle_carcol
# _export_dff
# _find_game_root
# _get_accent_color
# _get_game_root
# _get_icon_color
# _get_ide_db
# _launch_theme_settings
# _load_carcols
# load_dff
# load_img
# load_txd
# _load_vehicle_meta
# _lookup_txd_for_stem
# _menu_btn_popup
# _on_img_entry_dclicked
# _on_menu_btn_clicked
# _on_shared_txds_found
# _open_app_settings
# _open_dff
# _open_txd
# _parse_sa_vehicles_ide
# _refresh_icons
# _set_anim_speed
# _set_mode
# _set_paint_pair
# _set_wheel_steer
# _show_about
# _show_dropdown_menu
# _show_popup_menu
# _show_workshop_settings
# _strip_tex_suffix
# _titlebar_actions
# _toggle_animation
# _toggle_assembly_mode
# _toggle_bounds
# _toggle_com_indicator
# _toggle_damage_mode
# _toggle_door
# _toggle_dummy_overlay
# _toggle_lod_mode
# _toggle_seat_overlay
# _toggle_show_wheels
# _toggle_suspension_vis
# _update_paint_btns
# _upload_txd_additive

import os, re

from apps.methods.titlebar_func import TitlebarMixin
from PyQt6.QtCore import QSize, QThread, QTimer, Qt
from PyQt6.QtGui import QColor, QIcon, QImage, QPixmap
from PyQt6.QtWidgets import (
    QApplication, QCheckBox, QColorDialog, QComboBox, QDialog, QDialogButtonBox,
    QFileDialog, QFontComboBox, QFormLayout, QFrame, QGroupBox, QHBoxLayout, QLabel,
    QLineEdit, QListWidgetItem, QMenu, QMessageBox, QPushButton, QSpinBox, QTabWidget,
    QTextEdit, QVBoxLayout, QWidget)

from apps.methods.dff_parser import load_dff
from apps.methods.txd_parser import parse_txd
from apps.components.Vehicle_Workshop.depends.vehicle_common_func import (
    APPSETTINGS_AVAILABLE, AppSettings, SVGIconFactory, SettingsDialog)


class _ToolbarMixin(TitlebarMixin):
    """Toolbar + Settings dialog + theme methods.
    Mixed into GUIWorkshop — not used standalone.
    """

    #    Toolbar creation

    def _menu_btn_popup(self): #vers 1
        """Menu button calls _on_menu_btn_clicked (top bar or drop-down)."""
        return False

    def _titlebar_actions(self, layout): #vers 1
        """Vehicle titlebar: Open, Save, Export, Import, Undo."""
        self.open_btn = self._tb_button("open_icon", "", "Open Ctrl+O", self._open_file)  #TODO does not always work when switching tabs, should load the needed type for other tabs.
        self.save_btn = self._tb_button("save_icon", "", "Save Ctrl+S", self._save_file)
        self.export_btn = self._tb_button("export_icon", "", "Export", self._export_file)
        self.import_btn = self._tb_button("import_icon", "", "Import", self._import_file)
        for btn in (self.open_btn, self.save_btn, self.export_btn, self.import_btn):
            layout.addWidget(btn)
        layout.addSpacing(6)
        self.undo_btn = self._tb_button("undo_icon", "", "Undo Ctrl+Z", self._undo)
        layout.addWidget(self.undo_btn)


    def _toggle_assembly_mode(self, enabled: bool): #vers 2
        self.viewport.set_assembly_mode(enabled)
        if enabled and self._dff_model:
            damaged = getattr(self,'_damage_mode',False)
            self.viewport.load_all_geometries(
                self._dff_model.geometries,
                [g.materials for g in self._dff_model.geometries],
                self._dff_model.frames,
                self._dff_model.atomics,
                damaged=damaged)
        self._geom_list.setEnabled(not enabled)

    def _toggle_damage_mode(self, enabled: bool): #vers 2
        self._damage_mode = enabled
        m = getattr(self, '_dff_model', None)
        if m and m.frames and m.atomics:
            self.viewport.load_all_geometries(
                m.geometries, [g.materials for g in m.geometries],
                m.frames, m.atomics, damaged=enabled)
            self.viewport.update()

    def _toggle_lod_mode(self, enabled: bool): #vers 1
        self.viewport.set_show_lod(enabled)
        if getattr(self,'_assemble_btn',None) and self._assemble_btn.isChecked():
            self._toggle_assembly_mode(True)

    def _toggle_animation(self, enabled: bool): #vers 2
        """Toggle door open/close animation cycle."""
        vp = self.viewport
        if enabled:
            if not hasattr(self, '_anim_timer') or not self._anim_timer:
                self._anim_timer = QTimer(self)  # parent=self → auto-stopped on delete
            self._anim_angle = 0.0
            speed = getattr(self, '_anim_speed', 1.0)
            def _step():  #vers 1
                try:
                    self._anim_angle = (self._anim_angle + speed * 2.0) % 180.0
                    vp._door_angles = getattr(vp, '_door_angles', {})
                    for dname in getattr(self, '_door_open', set()):
                        vp._door_angles[dname] = self._anim_angle if self._anim_angle < 90 else 180 - self._anim_angle
                    vp.update()
                except RuntimeError:
                    if hasattr(self, '_anim_timer') and self._anim_timer:
                        self._anim_timer.stop()
            self._anim_timer.timeout.connect(_step)
            self._anim_timer.start(33)
        else:
            if hasattr(self, '_anim_timer') and self._anim_timer:
                self._anim_timer.stop()
            vp.update()

    def _set_anim_speed(self, value: int): #vers 1
        """Set animation speed from slider."""
        self._anim_speed = value / 10.0

    def _toggle_door(self, name: str, open: bool): #vers 1
        """Track which doors are toggled open."""
        self._door_open = getattr(self, '_door_open', set())
        if open: self._door_open.add(name)
        else:    self._door_open.discard(name)
        self.viewport.update()

    def _set_wheel_steer(self, angle: int): #vers 1
        """Set front wheel steering angle in viewport."""
        if hasattr(self.viewport, 'set_wheel_heading'):
            self.viewport.set_wheel_heading(angle)
        else:
            self.viewport._wheel_steer = angle
            self.viewport.update()

    def _cycle_carcol(self): #vers 3
        """Cycle through carcols.dat pairs for the current vehicle. Asks for file if not found."""
        stem = os.path.splitext(os.path.basename(
            (self._current_dff_path or '')))[0].lower()
        if not stem: return
        game_root = self._get_game_root()
        # Try cached carcols path first
        mw = getattr(self, 'main_window', None)
        carcols_path = getattr(mw, 'vehicle_data_paths', {}).get('carcols', '')
        if not carcols_path:
            # Try standard locations
            for sub in ('data/carcols.dat','DATA/CARCOLS.DAT','data/CARCOLS.DAT'):
                p = os.path.join(game_root, sub) if game_root else ''
                if p and os.path.isfile(p):
                    carcols_path = p; break
        if not carcols_path:
            # Ask user
            carcols_path, _ = QFileDialog.getOpenFileName(
                self, "Open carcols.dat", game_root or '',
                "Carcols (carcols.dat CARCOLS.DAT);;All Files (*)")
            if not carcols_path: return
            # Cache it
            if mw:
                if not hasattr(mw, 'vehicle_data_paths'):
                    mw.vehicle_data_paths = {}
                mw.vehicle_data_paths['carcols'] = carcols_path
        try:
            from apps.methods.carcols_parser import parse_carcols
            data = parse_carcols(carcols_path)
            pairs = data.get_colours(stem)
            if not pairs:
                self._set_status(f"No carcols entry for '{stem}'")
                return
            idx = getattr(self, '_carcol_idx', -1) + 1
            if idx >= len(pairs): idx = 0
            self._carcol_idx = idx
            p1, p2 = pairs[idx]
            self._set_paint_pair(p1, p2)
            if hasattr(self, '_carcol_btn'):
                self._carcol_btn.setToolTip(
                    f"Pair {idx+1}/{len(pairs)} — "
                    f"Pri: rgb({int(p1[0]*255)},{int(p1[1]*255)},{int(p1[2]*255)}) "
                    f"Sec: rgb({int(p2[0]*255)},{int(p2[1]*255)},{int(p2[2]*255)})")
            self._set_status(f"Carcol {idx+1}/{len(pairs)} for {stem}")
        except Exception as e:
            self._set_status(f'Carcol error: {e}')

    def _toggle_com_indicator(self): #vers 1
        """Toggle Centre of Mass indicator in viewport."""
        show = not getattr(self, '_show_com', False)
        self._show_com = show
        # Read CoM from handling if available
        if show:
            try:
                m = getattr(self, '_dff_model', None)
                stem = os.path.splitext(os.path.basename((self._current_dff_path or '')))[0].lower()
                if hasattr(self,'_tab_handling') and self._tab_handling._parser:
                    entry = next((e for e in self._tab_handling._parser.entries
                                  if e.name.lower()==stem), None)
                    if entry and len(entry.values) > 7:
                        cx,cy,cz = float(entry.values[4]),float(entry.values[5]),float(entry.values[6])
                        self.viewport._com_pos = (cx,cy,cz)
                        self._set_status(f'CoM: ({cx:.2f}, {cy:.2f}, {cz:.2f})')
            except Exception:
                pass
        else:
            self.viewport._com_pos = None
        self.viewport.update()

    def _toggle_dummy_overlay(self): #vers 1
        """Toggle dummy frame position markers in viewport."""
        self.viewport._show_dummies = not getattr(self.viewport, '_show_dummies', False)
        self.viewport.update()

    def _toggle_suspension_vis(self): #vers 1
        """Toggle suspension travel range visualisation."""
        self.viewport._show_suspension = not getattr(self.viewport, '_show_suspension', False)
        self.viewport.update()

    def _toggle_seat_overlay(self): #vers 1
        """Toggle seat position markers."""
        self.viewport._show_seats = not getattr(self.viewport, '_show_seats', False)
        self.viewport.update()

    def _toggle_bounds(self): #vers 1
        """Toggle bounding box display."""
        self.viewport._show_bounds = not getattr(self.viewport, '_show_bounds', False)
        self.viewport.update()

    def _export_dff(self): #vers 1
        """Export current DFF geometry (stub — routes to model workshop)."""
        self._set_status("Export: use Model Workshop for DFF export")

    def _parse_sa_vehicles_ide(self, path: str): #vers 5
        """Parse vehicles.ide/default.ide for wheel scales - handles GTA3, VC and SA."""
        self._sa_vehicle_data = {}
        try:
            mw = getattr(self, 'main_window', None)
            ide_db = getattr(mw, 'ide_db', None) if mw else None
            raw = getattr(ide_db, '_game', None) or getattr(ide_db, 'game', None)
            game_ver = str(raw).lower().split('.')[-1] if raw else 'vc'
            # Detect from filename: vehicles.ide = SA (VC uses default.ide)
            fname_lower = os.path.basename(path).lower()
            if fname_lower == 'vehicles.ide' and game_ver in ('vc', 'gta3'):
                game_ver = 'sa'
            elif fname_lower == 'default.ide' and game_ver == 'sa':
                game_ver = 'vc'  # SA doesn't use default.ide
            print(f'[IDE] parse: {fname_lower} game_ver={game_ver} raw={raw}')

            # GTA3 wheelModelId -> wheel name (IDs 160-166)
            GTA3_WHEEL_ID = {
                160: 'wheel_sport',    161: 'wheel_saloon',
                162: 'wheel_lightvan', 163: 'wheel_classic',
                164: 'wheel_alloy',    165: 'wheel_lighttruck',
                166: 'wheel_smallcar',
            }
            # VC wheelModelId -> wheel name (IDs 237-256)
            VC_WHEEL_ID = {
                237: 'wheel_rim',        238: 'wheel_offroad',
                239: 'wheel_truck',      250: 'wheel_sport',
                251: 'wheel_saloon',     252: 'wheel_lightvan',
                253: 'wheel_classic',    254: 'wheel_alloy',
                255: 'wheel_lighttruck', 256: 'wheel_smallcar',
            }
            WHEEL_ID_MAP = GTA3_WHEEL_ID if game_ver == 'gta3' else VC_WHEEL_ID

            # Field positions:
            # GTA3: id,model,txd,type,handlingId,gameName,class,freq,level,compRules,wheelId,wheelScale
            #        0   1    2   3    4           5        6     7    8     9         10      11
            # VC:   id,model,txd,type,handlingId,gameName,animFile,class,freq,level,compRules,wheelId,wheelScale
            #        0   1    2   3    4           5        6        7     8    9     10        11      12
            # SA:   id,model,txd,type,handlingId,gameName,animFile,class,freq,level,compRules,wheelId,frontScale,rearScale
            #        0   1    2   3    4           5        6        7     8    9     10        11      12         13
            if game_ver == 'gta3':
                scale_idx = 11; class_idx = 6; type_idx = 3; wheel_id_idx = 10
            else:  # vc and sa same layout
                scale_idx = 12; class_idx = 7; type_idx = 3; wheel_id_idx = 11

            SA_CLASS_WHEEL = {
                'richfamily':'wheel_saloon', 'executive':'wheel_saloon',
                'normal':'wheel_saloon',     'poorfamily':'wheel_saloon',
                'worker':'wheel_truck',      'ignore':'wheel_truck',
                'big':'wheel_lighttruck',    'taxi':'wheel_saloon',
                'offroad':'wheel_offroad',   'sport':'wheel_sport',
                'lightvan':'wheel_lightvan', 'van':'wheel_lightvan',
                'lighttruck':'wheel_lighttruck', 'classic':'wheel_classic',
                'bicycle':'wheel_rim',       'bike':'wheel_rim',
            }

            with open(path, 'r', errors='ignore') as f:
                in_cars = False
                for line in f:
                    line = line.strip()
                    if not line or line.startswith('#') or line.startswith(';'):
                        continue
                    if line.lower() == 'cars':
                        in_cars = True; continue
                    if line.lower() == 'end':
                        in_cars = False; continue
                    if not in_cars: continue
                    parts = [p.strip() for p in line.split(',')]
                    if len(parts) < scale_idx + 1: continue
                    try:
                        model      = parts[1].strip().lower()
                        veh_type   = parts[type_idx].strip().lower() if len(parts) > type_idx else ''
                        veh_class  = parts[class_idx].strip().lower() if len(parts) > class_idx else ''
                        front_scale = float(parts[scale_idx])
                        rear_scale  = float(parts[scale_idx+1]) if (
                            game_ver == 'sa' and len(parts) > scale_idx+1) else front_scale

                        # Wheel type from numeric ID first
                        wheel_model = 'wheel_saloon'
                        if len(parts) > wheel_id_idx:
                            try:
                                wid = int(parts[wheel_id_idx])
                                wheel_model = WHEEL_ID_MAP.get(wid, '')
                            except ValueError:
                                pass
                        # Fall back to class if no ID match
                        if not wheel_model:
                            if veh_type in ('bike','mtruck') or 'bike' in veh_type:
                                wheel_model = 'wheel_rim'
                            elif 'van' in veh_type or veh_type == 'truck':
                                wheel_model = 'wheel_lightvan'
                            else:
                                wheel_model = SA_CLASS_WHEEL.get(veh_class, 'wheel_saloon')

                        self._sa_vehicle_data[model] = {
                            'front_scale': front_scale,
                            'rear_scale':  rear_scale,
                            'wheel_model': wheel_model,
                        }
                    except (ValueError, IndexError):
                        continue
            self._set_status(f"IDE ({game_ver}): {len(self._sa_vehicle_data)} vehicles")
        except Exception as e:
            self._set_status(f"IDE parse error: {e}")
    def _toggle_show_wheels(self, enabled: bool): #vers 2
        """Load wheels.DFF + wheels.txd if needed and trigger re-assembly."""
        vp = self.viewport
        vp._show_wheels = enabled
        if enabled:
            game_root = self._find_game_root()
            wheels_path = getattr(vp, '_wheels_model_path', '')
            # Find wheels.DFF
            if not wheels_path and game_root:
                import os as _os
                for p in [
                    _os.path.join(game_root,'models','generic','wheels.DFF'),
                    _os.path.join(game_root,'models','generic','wheels.dff'),
                    _os.path.join(game_root,'models','Generic','wheels.DFF'),
                    _os.path.join(game_root,'models','Generic','wheels.dff'),
                ]:
                    if _os.path.isfile(p):
                        wheels_path = p; break
            if wheels_path and not getattr(vp, '_wheels_model', None):
                vp.load_wheels_dff(wheels_path)
            # Log what wheel type will be used
            wheel_data = vp._get_wheel_geom_data()
            if wheel_data:
                wm = wheel_data[4] if len(wheel_data) > 4 else []
                needed = [getattr(m,'texture_name','') for m in (wm or []) if getattr(m,'texture_name','')]
                self._set_status(f'Wheels: {vp._wheel_type} | textures: {", ".join(needed[:4])}')
            # Find and load wheels.txd (SA) or generic.txd contains wheel textures (VC/GTA3)
            if game_root:
                import os as _os
                # VC/GTA3/LC: wheel textures are in misc.txd (+ generic.txd)
                for fname in ['misc.txd','MISC.TXD','wheels.txd','wheels.TXD','generic.txd']:
                    p = _os.path.join(game_root,'models', fname)
                    if _os.path.isfile(p):
                        try:
                            from apps.methods.txd_parser import parse_txd
                            with open(p,'rb') as f: data = f.read()
                            textures = parse_txd(data)
                            if textures:
                                self._set_status(f'{fname}: {", ".join(t["name"] for t in textures[:6])}')
                                from PyQt6.QtCore import QTimer
                                QTimer.singleShot(150, lambda t=textures: vp._upload_textures(t, additive=True))
                        except Exception as e:
                            self._set_status(f'{fname} error: {e}')
        # Rebuild assembly
        m = getattr(self, '_dff_model', None)
        if m and m.frames and m.atomics:
            vp.load_all_geometries(
                m.geometries, [g.materials for g in m.geometries],
                m.frames, m.atomics,
                damaged=getattr(self,'_damage_mode',False))
        vp.update()
        if enabled:
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(100, self._auto_load_shared_txds)

    def _update_paint_btns(self): #vers 1
        vp = self.viewport
        def _css(rgb): return f'background-color:rgb({int(rgb[0]*255)},{int(rgb[1]*255)},{int(rgb[2]*255)});color:{"#000" if sum(rgb)>1.5 else "#fff"}'  #vers 1
        if hasattr(self,'_paint1_btn'): self._paint1_btn.setStyleSheet(_css(vp._paint1))
        if hasattr(self,'_paint2_btn'): self._paint2_btn.setStyleSheet(_css(vp._paint2))

    def _pick_paint1(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp = self.viewport
        col = QColorDialog.getColor(QColor(int(vp._paint1[0]*255),int(vp._paint1[1]*255),int(vp._paint1[2]*255)),self)
        if col.isValid(): vp._paint1=(col.redF(),col.greenF(),col.blueF()); self._update_paint_btns(); vp.update()

    def _pick_paint2(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp = self.viewport
        col = QColorDialog.getColor(QColor(int(vp._paint2[0]*255),int(vp._paint2[1]*255),int(vp._paint2[2]*255)),self)
        if col.isValid(): vp._paint2=(col.redF(),col.greenF(),col.blueF()); self._update_paint_btns(); vp.update()

    def _set_paint_pair(self, p1, p2): #vers 1
        self.viewport._paint1=p1; self.viewport._paint2=p2
        self._update_paint_btns(); self.viewport.update()

    def _load_vehicle_meta(self, vehicle_name: str): #vers 2
        """Load vehicles.ide (wheel type) and carcols colours for vehicle."""
        game_root = self._get_game_root()
        # vehicles.ide — wheel type
        if game_root:
            try:
                from apps.methods.vehicles_ide_parser import get_vehicle_info
                entry = get_vehicle_info(game_root, vehicle_name)
                if entry and entry.wheel_model:
                    wheel_dff = entry.wheel_dff_name()
                    self.viewport._wheel_type = wheel_dff
                    self._set_status(f'IDE: {vehicle_name} txd={entry.txd_name} wheel={wheel_dff}')
            except Exception:
                pass
        # handling.cfg or SA vehicles.ide — front/rear tyre scales
        try:
            stem = vehicle_name.lower()
            # SA: wheel scales are in vehicles.ide
            sa_data = getattr(self, '_sa_vehicle_data', {})
            self._set_status(f'Meta: sa_data={len(sa_data)} entries, stem={stem!r}, found={stem in sa_data}')
            if sa_data and stem in sa_data:
                self.viewport._wheel_front_scale = sa_data[stem]['front_scale']
                self.viewport._wheel_rear_scale  = sa_data[stem]['rear_scale']
                wheel_model = sa_data[stem].get('wheel_model', 'wheel_saloon_l0')
                if not wheel_model.endswith('_l0'):
                    wheel_model = wheel_model + '_l0'
                self.viewport._wheel_type = wheel_model
                self._set_status(f'SA IDE: wheel={wheel_model} front={sa_data[stem]["front_scale"]:.2f} rear={sa_data[stem]["rear_scale"]:.2f}')
            else:
                # VC/GTA3: wheel scales in handling.cfg fields 38/40
                # SA handling.cfg only has 36 fields - wheel scale is in vehicles.ide
                if hasattr(self, '_tab_handling') and hasattr(self._tab_handling, '_parser'):
                    p = self._tab_handling._parser
                    if p and p.entries:
                        entry = next((e for e in p.entries if e.name.lower() == stem), None)
                        if entry and len(entry.values) > 40:
                            try:
                                front_scale = float(entry.values[38])
                                rear_scale  = float(entry.values[40])
                                self.viewport._wheel_front_scale = front_scale
                                self.viewport._wheel_rear_scale  = rear_scale
                                self._set_status(f'Wheel scale: front={front_scale:.2f} rear={rear_scale:.2f}')
                            except (ValueError, IndexError):
                                pass
                        elif entry and len(entry.values) <= 40:
                            # SA handling.cfg - no wheel scale, use IDE default 1.0
                            self.viewport._wheel_front_scale = 1.0
                            self.viewport._wheel_rear_scale  = 1.0
        except Exception:
            pass
        self._load_carcols(vehicle_name)

    def _get_game_root(self): #vers 1
        """Get game root from viewport or main_window."""
        game_root = ''
        if hasattr(self.viewport,'_find_game_root'):
            game_root = self.viewport._find_game_root()
        if not game_root:
            mw = self.main_window
            if mw:
                for attr in ('game_root','_game_root','game_directory'):
                    val=getattr(mw,attr,None)
                    if val and os.path.isdir(str(val)): game_root=str(val); break
        return game_root

    def _load_carcols(self, vehicle_name: str): #vers 1

        lay = getattr(self,'_carcols_lay',None)
        if not lay: return
        while lay.count():
            item = lay.takeAt(0)
            if item.widget(): item.widget().deleteLater()
        game_root = self._get_game_root()
        if not game_root: return
        try:
            from apps.methods.carcols_parser import get_vehicle_colours
            pairs = get_vehicle_colours(game_root, vehicle_name)
            if not pairs: return
            lbl = QLabel(f'Carcols ({len(pairs)} pairs)')
            lbl.setFont(self.infobar_font); lay.addWidget(lbl)
            for i,(p1,p2) in enumerate(pairs[:8]):
                row=QWidget(); rl=QHBoxLayout(row)
                rl.setContentsMargins(0,0,0,0); rl.setSpacing(2)
                def _css(rgb): return f'background-color:rgb({int(rgb[0]*255)},{int(rgb[1]*255)},{int(rgb[2]*255)});min-height:16px;border:1px solid #333'  #vers 1
                b1=QPushButton(); b1.setFixedSize(20,16); b1.setStyleSheet(_css(p1))
                b2=QPushButton(); b2.setFixedSize(20,16); b2.setStyleSheet(_css(p2))
                b1.setToolTip(f'Primary: rgb({int(p1[0]*255)},{int(p1[1]*255)},{int(p1[2]*255)})')
                b2.setToolTip(f'Secondary: rgb({int(p2[0]*255)},{int(p2[1]*255)},{int(p2[2]*255)})')
                ab=QPushButton(f'#{i+1}'); ab.setFixedHeight(16); ab.setFont(self.infobar_font)
                ab.setToolTip(f'Apply colour pair {i+1}')
                ab.clicked.connect(lambda _=False,a=p1,b=p2: self._set_paint_pair(a,b))
                rl.addWidget(b1); rl.addWidget(b2); rl.addWidget(ab,1)
                lay.addWidget(row)
        except Exception as e:
            print(f'[carcols] {e}')


    def _set_mode(self, mode: str): #vers 1
        self.viewport.set_render_mode(mode)


    # - file operations
    def _open_dff(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open DFF", self._last_dir,
            "DFF Models (*.dff);;All Files (*)")
        if path:
            self._last_dir = os.path.dirname(path)
            self.WS.set('last_dir', self._last_dir)
            self.load_dff(path)
            for ext in ('.txd','.TXD'):
                txd = os.path.splitext(path)[0]+ext
                if os.path.isfile(txd): self.load_txd(txd); break

    def _open_txd(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open TXD", self._last_dir,
            "TXD Files (*.txd);;All Files (*)")
        if path:
            self._last_dir = os.path.dirname(path)
            self.load_txd(path)

    def load_dff(self, path: str): #vers 4
        try:
            from apps.methods.dff_parser import load_dff
            self._show_progress(True)
            self._set_status(f"Parsing {os.path.basename(path)}…")
            from PyQt6.QtWidgets import QApplication
            QApplication.processEvents()
            model = load_dff(path)
            if not model or not model.geometries:
                self._set_status(f"Failed: {os.path.basename(path)}"); return
            self._dff_model = model
            self._current_dff_path = path
            # Clear texture cache when loading new DFF
            self.viewport.clear_textures()
            self._tex_list.clear()
            self.WS.add_recent(path); self.WS.save()
            self._populate_geom_list()
            self._geom_list.setCurrentRow(0)
            self._set_status(
                f"Loaded: {os.path.basename(path)} — "
                f"{len(model.geometries)} geometries, {len(model.frames)} frames")
            # Populate frame hierarchy tree
            self._populate_frame_tree()
            # Load vehicles.ide info (wheel type) + carcols colours
            stem = os.path.splitext(os.path.basename(path))[0]
            from PyQt6.QtCore import QTimer
            def _meta_then_wheels(s=stem):  #vers 1
                self._load_vehicle_meta(s)
                self._carcol_idx = -1
                if getattr(self.viewport, '_show_wheels', False):
                    self._toggle_show_wheels(True)
                else:
                    # No wheels — still need shared TXDs for vehicle body
                    QTimer.singleShot(200, self._auto_load_shared_txds)
            QTimer.singleShot(100, _meta_then_wheels)
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f"Error: {e}")
        finally:
            self._show_progress(False)

    def _strip_tex_suffix(self, name: str) -> str: #vers 2
        """Strip GTA texture suffix.
        Handles: buildrt4_fehihwm (alpha suffix) and vehiclegeneric256 (numeric size suffix)."""
        n = re.sub(r'_[a-z]{4,8}$', '', name)
        n = re.sub(r'\d+$', '', n)
        return n

    def _get_ide_db(self): #vers 1
        """Return IDEDatabase from mw.ide_db if loaded, else None."""
        mw = getattr(self, 'main_window', None)
        db = getattr(mw, 'ide_db', None)
        if db and getattr(db, '_loaded', False) and db.model_map:
            return db
        db2 = getattr(getattr(mw, 'dat_browser', None), '_ide_db', None)
        if db2 and db2.model_map:
            return db2
        return None

    def _lookup_txd_for_stem(self, stem: str) -> str: #vers 1
        """Return txd_name from IDE DB for model stem, or empty string."""
        db = self._get_ide_db()
        if not db:
            return ''
        obj = db.model_map.get(stem.lower())
        if obj:
            return obj.txd_name.lower()
        # Try stripped stem
        base = self._strip_tex_suffix(stem.lower())
        if base != stem.lower():
            obj = db.model_map.get(base)
            if obj:
                return obj.txd_name.lower()
        return ''

    def _collect_needed_textures(self): #vers 2
        """Return set of texture names the current DFF needs.
        Stores both full name and suffix-stripped base for fuzzy matching."""
        if not self._dff_model: return set()
        needed = set()
        for g in self._dff_model.geometries:
            for mat in g.materials:
                name = (mat.texture_name or '').strip().lower()
                if not name:
                    continue
                needed.add(name)
                base = self._strip_tex_suffix(name)
                if base != name:
                    needed.add(base)
        return needed

    def _upload_txd_additive(self, path: str): #vers 1
        """Load a TXD and upload textures WITHOUT clearing existing ones."""
        try:
            from apps.methods.txd_parser import parse_txd
            from PyQt6.QtGui import QIcon, QImage, QPixmap
            from PyQt6.QtWidgets import QListWidgetItem
            from PyQt6.QtCore import Qt
            with open(path, 'rb') as f: data = f.read()
            textures = parse_txd(data)
            if not textures: return 0
            # Only upload textures not already loaded
            new_textures = [t for t in textures
                            if t['name'].lower() not in self.viewport._tex_ids]
            if new_textures:
                # Additive upload — don't clear existing
                self.viewport.makeCurrent()
                from OpenGL.GL import (glGenTextures, glBindTexture, GL_TEXTURE_2D,
                    glTexParameteri, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR,
                    GL_TEXTURE_MAG_FILTER, GL_LINEAR, GL_TEXTURE_WRAP_S,
                    GL_TEXTURE_WRAP_T, GL_REPEAT, GL_CLAMP_TO_EDGE, GL_MIRRORED_REPEAT,
                    glTexImage2D, GL_RGBA, GL_UNSIGNED_BYTE, glGenerateMipmap, glDeleteTextures)
                def _rw_wrap(rw):  #vers 1
                    if rw == 2: return GL_CLAMP_TO_EDGE
                    if rw == 3: return GL_MIRRORED_REPEAT
                    return GL_REPEAT
                for t in new_textures:
                    name = t['name'].lower()
                    rgba = t.get('rgba_data', b'')
                    w = t.get('width', 0); h = t.get('height', 0)
                    if not (rgba and w > 0 and h > 0): continue
                    wrap_s = _rw_wrap(t.get('wrap_u', 1))
                    wrap_t = _rw_wrap(t.get('wrap_v', 1))
                    gl_id = glGenTextures(1)
                    glBindTexture(GL_TEXTURE_2D, gl_id)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrap_s)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrap_t)
                    try:
                        glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,w,h,0,GL_RGBA,GL_UNSIGNED_BYTE,rgba)
                        glGenerateMipmap(GL_TEXTURE_2D)
                        self.viewport._tex_ids[name] = gl_id
                        self.viewport._tex_wrap[name] = (t.get('wrap_u',1), t.get('wrap_v',1))
                    except Exception:
                        glDeleteTextures(1,[gl_id])
                self.viewport.doneCurrent()
                # Add to texture list
                for t in new_textures:
                    item = QListWidgetItem(f"{t['name']}  {t['width']}\xd7{t['height']}")
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
                self.viewport.update()
            return len(new_textures)
        except Exception as e:
            return 0

    def _find_game_root(self): #vers 1
        """Try to find the GTA SA game root from the DFF path or main_window settings."""
        # From main_window app_settings
        mw = self.main_window
        if mw:
            for attr in ('game_root','_game_root','game_directory'):
                val = getattr(mw, attr, None)
                if val and os.path.isdir(val): return val
            if hasattr(mw,'app_settings'):
                settings = mw.app_settings
                for key in ('game_root','game_directory','sa_root'):
                    val = getattr(settings,'get',lambda k,d=None:d)(key)
                    if val and os.path.isdir(str(val)): return str(val)
        # Walk up from DFF path looking for models/ or data/ folder
        if self._current_dff_path:
            p = os.path.dirname(self._current_dff_path)
            for _ in range(8):
                if os.path.isdir(os.path.join(p,'models')) and os.path.isdir(os.path.join(p,'data')):
                    return p
                p = os.path.dirname(p)
        return ''

    def _auto_load_shared_txds(self): #vers 5
        """Find shared TXDs — uses preloaded cache first, worker for remainder."""
        if not self._dff_model: return
        needed  = self._collect_needed_textures()
        if getattr(self.viewport, '_show_wheels', False):
            wheel_data = self.viewport._get_wheel_geom_data()
            if wheel_data:
                wm = wheel_data[4] if len(wheel_data) > 4 else []
                for mat in (wm or []):
                    tname = getattr(mat, 'texture_name', '') or ''
                    if tname:
                        needed.add(tname.lower())
                        base = self._strip_tex_suffix(tname.lower())
                        if base != tname.lower(): needed.add(base)
        already = set(self.viewport._tex_ids.keys())
        missing = needed - already
        if not missing: return

        # Step 0: Use preloaded shared cache from DAT Browser (instant, no worker)
        mw = getattr(self, 'main_window', None)
        preloaded = getattr(mw, 'shared_vehicle_textures', {})
        if preloaded:
            hits = []
            for name in list(missing):
                t = preloaded.get(name)
                if not t:
                    t = preloaded.get(self._strip_tex_suffix(name))
                if t:
                    hits.append(t); missing.discard(name)
                    missing.discard(self._strip_tex_suffix(name))
            if hits:
                self.viewport._upload_textures(hits, additive=True)
                self.viewport.update()
                if hasattr(mw, 'log_message'):
                    mw.log_message(f"+{len(hits)} shared textures loaded")
            if not missing:
                return  # everything satisfied from cache

        # Snapshot everything the worker needs — no shared mutable state
        game_root  = self._find_game_root()
        dff_dir    = os.path.dirname(self._current_dff_path) if self._current_dff_path else ''
        dff_stem   = os.path.splitext(os.path.basename(self._current_dff_path))[0].lower() if self._current_dff_path else ''
        img        = getattr(self, '_current_img', None)
        ide_txd    = self._lookup_txd_for_stem(dff_stem)  # from IDE DB
        _ide_db    = self._get_ide_db()
        game_ver   = getattr(_ide_db, '_game', None) or getattr(_ide_db, 'game', 'vc')

        from PyQt6.QtCore import QThread, pyqtSignal as _sig

        strip_suffix = self._strip_tex_suffix
        viewer_ref = self

        class _Worker(QThread):
            found = _sig(list)   # emits list of {'name','rgba_data','width','height','format'}
            status = _sig(str)

            def run(self):  #vers 1
                from apps.methods.txd_parser import parse_txd
                import tempfile
                collected = []
                
                miss = set(missing)  # local copy
                # Build suffix-stripped alias map: base_name -> full_name
                # so we can match TXD textures against suffixed DFF names
                alias = {}
                for n in list(miss):
                    base = strip_suffix(n)
                    if base != n:
                        alias[base] = n  # base -> suffixed

                def _try_txd_data(data):  #vers 1
                    nonlocal miss
                    try:
                        textures = parse_txd(data)
                        hits = []
                        for t in textures:
                            tname = t['name'].lower()
                            if not (t.get('rgba_data') and t['width'] > 0):
                                continue
                            tbase = strip_suffix(tname)
                            if tname in miss:
                                # Exact match
                                hits.append(t)
                            elif tname in alias:
                                # TXD has base name, miss has suffixed name
                                t2 = dict(t); t2['name'] = alias[tname]
                                hits.append(t2); hits.append(t)
                            elif tbase in miss:
                                # TXD has suffixed name (armytruk8bit128), miss has base (armytruk8bit)
                                hits.append(t)
                            elif tbase in alias:
                                # Both suffixed — map to what DFF expects
                                t2 = dict(t); t2['name'] = alias[tbase]
                                hits.append(t2); hits.append(t)
                        if hits:
                            collected.extend(hits)
                            for t in hits:
                                miss.discard(t['name'].lower())
                                miss.discard(strip_suffix(t['name'].lower()))
                        return len(hits)
                    except Exception:
                        return 0

                # 0. Shared TXDs from models/ — game-version aware
                # GTA3/LC: models/generic.txd, models/Generic/wheels.DFF
                # VC:      models/generic.txd, models/Generic/wheels.DFF
                # SA:      models/generic/vehicle.txd, models/generic/wheels.txd, models/generic/wheels.DFF
                if game_root:
                    m = os.path.join(game_root, 'models')
                    if game_ver == 'sa':
                        shared_txds = [
                            os.path.join(m, 'generic', 'vehicle.txd'),
                            os.path.join(m, 'generic', 'wheels.txd'),
                            os.path.join(m, 'misc.txd'),  # SA has misc.txd in models/
                            # Additional SA shared vehicle TXDs on disk
                            os.path.join(m, 'generic', 'vehiclecommon.txd'),
                            os.path.join(m, 'generic', 'vehiclegeneric.txd'),
                            os.path.join(m, 'generic', 'vehiclegrunge.txd'),
                            os.path.join(m, 'generic', 'vehiclelights.txd'),
                            os.path.join(m, 'generic', 'vehicletyres.txd'),
                        ]
                        # Also scan models/txd/ if it exists
                        txd_dir = os.path.join(m, 'txd')
                        if os.path.isdir(txd_dir):
                            for fn in os.listdir(txd_dir):
                                if fn.lower().startswith('vehicle') and fn.lower().endswith('.txd'):
                                    shared_txds.append(os.path.join(txd_dir, fn))
                        wheel_dffs = [
                            os.path.join(m, 'generic', 'wheels.DFF'),
                            os.path.join(m, 'generic', 'wheels.dff'),
                        ]
                    else:
                        # GTA3 / VC / LC: generic.txd + misc.txd in models/, wheels.DFF in models/Generic/
                        shared_txds = [
                            os.path.join(m, 'generic.txd'),
                            os.path.join(m, 'particle.txd'),
                            os.path.join(m, 'misc.txd'),
                            os.path.join(m, 'MISC.TXD'),
                            os.path.join(m, 'wheels.txd'),
                            os.path.join(m, 'wheels.TXD'),
                        ]
                        wheel_dffs = [
                            os.path.join(m, 'Generic', 'wheels.DFF'),
                            os.path.join(m, 'Generic', 'wheels.dff'),
                            os.path.join(m, 'generic', 'wheels.DFF'),
                            os.path.join(m, 'generic', 'wheels.dff'),
                        ]
                    for p in shared_txds:
                        if os.path.isfile(p) and miss:
                            try:
                                with open(p, 'rb') as f: _try_txd_data(f.read())
                            except Exception: pass
                    for wp in wheel_dffs:
                        if os.path.isfile(wp):
                            viewer_ref._wheels_model_path = wp; break
                    if not miss:
                        if collected: self.found.emit(collected)
                        return

                # 1. Same directory as DFF
                if dff_dir and miss:
                    try:
                        for fn in os.listdir(dff_dir):
                            if not fn.lower().endswith('.txd'): continue
                            if fn[:-4].lower() == dff_stem: continue
                            if not miss: break
                            try:
                                with open(os.path.join(dff_dir,fn),'rb') as f: _try_txd_data(f.read())
                            except Exception: pass
                    except Exception: pass

                # 1a. IDE DB lookup — exact TXD name from parsed IDE files
                if miss and img and hasattr(img, 'entries') and ide_txd:
                    txd_key = ide_txd if ide_txd.endswith('.txd') else ide_txd + '.txd'
                    txd_map = {e.name.lower(): e for e in img.entries if e.name.lower().endswith('.txd')}
                    entry = txd_map.get(txd_key)
                    if entry:
                        self.status.emit(f'IDE: loading {txd_key}...')
                        try:
                            data = img.read_entry_data(entry)
                            if data: _try_txd_data(data)
                        except Exception: pass

                # 1b. GTA3 only: vehicle TXDs are in models/txd.img not gta3.img
                if miss and img and getattr(img, 'file_path', '') and game_ver == 'gta3':
                    img_dir = os.path.dirname(img.file_path)
                    txd_img_path = os.path.join(img_dir, 'txd.img')
                    if os.path.isfile(txd_img_path) and ide_txd:
                        try:
                            from apps.methods.img_core_classes import IMGFile
                            sib = IMGFile(txd_img_path)
                            txd_key = ide_txd if ide_txd.endswith('.txd') else ide_txd + '.txd'
                            for e in sib.entries:
                                if e.name.lower() == txd_key:
                                    data = sib.read_entry_data(e)
                                    if data: _try_txd_data(data)
                                    break
                        except Exception: pass

                # 1b. Current IMG (gta3.img) — look for vehicle*.txd entries
                # SA stores vehiclegeneric256 etc inside gta3.img as separate TXDs
                if miss and img and hasattr(img,'entries'):
                    self.status.emit('Scanning gta3.img for vehicle textures...')
                    txd_map={e.name.lower():e for e in img.entries if e.name.lower().endswith('.txd')}
                    # Known SA shared vehicle TXD names inside gta3.img
                    candidates=['vehiclecommon.txd','vehicle.txd','vehicles.txd',
                                 'vehiclegeneric.txd','vehiclegrunge.txd',
                                 'vehiclelights.txd','vehicletyres.txd',
                                 # SA uses full names with size suffix inside gta3.img
                                 'vehiclegeneric256.txd','vehiclegrunge256.txd',
                                 'vehiclelights128.txd','vehicletyres128.txd',
                                 'carcommon.txd','carpback.txd','carplate.txd',]
                    # Also try prefix match for any vehicle*.txd or car*.txd
                    candidates += [n for n in txd_map if n.startswith('vehicle') and n not in candidates]
                    candidates += [n for n in txd_map if n.startswith('car') and n not in candidates]
                    tried=set()
                    for cand in candidates:
                        if not miss: break
                        if cand in txd_map and cand not in tried:
                            tried.add(cand)
                            try:
                                data=img.read_entry_data(txd_map[cand])
                                if data: _try_txd_data(data)
                            except Exception: pass

                # 2. Current IMG — ONLY look up exact stem.txd entries, no full scan
                if miss and img and hasattr(img,'entries'):
                    miss_names = ', '.join(sorted(miss)[:8])
                    self.status.emit(f'Missing textures: {miss_names}')
                    # Build a map of entry names first (fast, no data read)
                    txd_entries = {e.name.lower(): e for e in img.entries
                                   if e.name.lower().endswith('.txd')}
                    # Only try TXDs whose name hints at containing missing textures
                    # Heuristic: match first 6 chars of texture name to TXD stem
                    tried = set()
                    for tex_name in list(miss):
                        if not miss: break
                        stem6 = tex_name[:6].lower()
                        for txd_name, entry in txd_entries.items():
                            if txd_name[:-4] in tried: continue
                            if txd_name.startswith(stem6) or stem6.startswith(txd_name[:4]):
                                tried.add(txd_name[:-4])
                                try:
                                    data = img.read_entry_data(entry)
                                    if data: _try_txd_data(data)
                                except Exception: pass
                                break

                # 3. Fallback — scan all vehicle*.txd in models/ subdirs on disk
                if miss and game_root:
                    m2 = os.path.join(game_root, 'models')
                    for subdir in ('', 'generic', 'txd'):
                        d = os.path.join(m2, subdir) if subdir else m2
                        if not os.path.isdir(d): continue
                        for fn in sorted(os.listdir(d)):
                            if not miss: break
                            fnl = fn.lower()
                            if not fnl.endswith('.txd'): continue
                            if not any(fnl.startswith(p) for p in ('vehicle','carpl','carpb','misc','generic')):
                                continue
                            try:
                                with open(os.path.join(d, fn),'rb') as f: _try_txd_data(f.read())
                            except Exception: pass
                if collected:
                    found_names = ', '.join(t['name'] for t in collected[:8])
                    self.status.emit(f'Found textures: {found_names}')
                    self.found.emit(collected)
                if miss:
                    self.status.emit(f'Still missing: {", ".join(sorted(miss)[:8])}')

        self._shared_txd_worker = _Worker()
        self._shared_txd_worker.status.connect(self._set_status)
        self._shared_txd_worker.found.connect(self._on_shared_txds_found)
        self._shared_txd_worker.finished.connect(lambda: self._show_progress(False))
        self._show_progress(True)
        self._shared_txd_worker.start()

    def _on_shared_txds_found(self, textures: list): #vers 2
        """Receive shared textures from worker thread and upload to GL on main thread."""
        # Load wheels.DFF if path was discovered by worker
        wheels_path = getattr(self.viewport,'_wheels_model_path',None)
        if wheels_path and not getattr(self.viewport,'_wheels_model',None):
            self.viewport.load_wheels_dff(wheels_path)
        if not textures: return
        # Filter already-loaded
        new = [t for t in textures if t['name'].lower() not in self.viewport._tex_ids]
        if not new: return
        # Defer upload until GL context ready
        def _do_shared_upload():  #vers 1
            try:
                vp = self.viewport
                if hasattr(vp, 'isValid') and not vp.isValid():
                    from PyQt6.QtCore import QTimer
                    QTimer.singleShot(200, _do_shared_upload)
                    return
                vp._upload_textures(new, additive=True)
                vp.update()
            except Exception:
                pass
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(50, _do_shared_upload)
        # Add to tex list
        from PyQt6.QtGui import QIcon, QImage, QPixmap
        from PyQt6.QtWidgets import QListWidgetItem
        from PyQt6.QtCore import Qt
        for t in new:
            item = QListWidgetItem(f"{t['name']}  {t['width']}\xd7{t['height']}")
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
        self._set_status(f'+{len(new)} shared textures loaded')
        self.viewport.update()

    def load_txd(self, path: str): #vers 2
        try:
            from apps.methods.txd_parser import parse_txd
            from PyQt6.QtGui import QIcon, QImage, QPixmap
            from PyQt6.QtWidgets import QListWidgetItem
            from PyQt6.QtCore import Qt, QTimer
            with open(path,'rb') as f: data=f.read()
            textures = parse_txd(data)
            if not textures:
                self._set_status(f'No textures: {os.path.basename(path)}'); return
            # GL upload — defer until GL context is initialized
            def _do_upload():  #vers 1
                try:
                    vp = self.viewport
                    if hasattr(vp, 'isValid') and not vp.isValid():
                        QTimer.singleShot(200, _do_upload)
                        return
                    vp._upload_textures(textures)
                    vp.update()
                except Exception as ue:
                    self._set_status(f'GL upload: {ue}')
            QTimer.singleShot(50, _do_upload)
            # Texture list with thumbnails
            self._tex_list.clear()
            for t in textures:
                item = QListWidgetItem(f"{t['name']}  {t['width']}\xd7{t['height']}")
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
            self._set_status(f'Textures: {len(textures)} from {os.path.basename(path)}')
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f'TXD error: {e}')

    def load_img(self, img): #vers 3
        """Populate IMG list with vehicle DFF entries only (from handling.cfg / IDE DB).
        Also auto-loads vehicle data files from mw.vehicle_data_paths if not yet loaded."""
        self._current_img = img
        # Auto-load vehicle data files if not already loaded
        mw = getattr(self, 'main_window', None)
        vdata = getattr(mw, 'vehicle_data_paths', {})
        if vdata and not getattr(self, '_handling_loaded', False):
            for key in ('handling', 'carcols', 'carmods', 'cargrp', 'vehicles_ide'):
                p = vdata.get(key, '')
                if p and os.path.isfile(p):
                    self._open_file(p)
            self._handling_loaded = True
        self._img_list.clear()
        if not img or not hasattr(img, 'entries'):
            return
        # Build vehicle name filter from handling parser or mw.vehicle_names
        vehicle_names = set()
        try:
            if hasattr(self, '_tab_handling') and hasattr(self._tab_handling, '_parser'):
                p = self._tab_handling._parser
                if p and p.entries:
                    vehicle_names = {e.name.lower() for e in p.entries if e.name}
        except Exception:
            pass
        if not vehicle_names:
            mw = getattr(self, 'main_window', None)
            vehicle_names = getattr(mw, 'vehicle_names', set())
        for entry in img.entries:
            if not entry.name.lower().endswith('.dff'):
                continue
            stem = entry.name.rsplit('.', 1)[0].lower()
            # Only show if known vehicle, or no filter available (show all)
            if vehicle_names and stem not in vehicle_names:
                continue
            stem, ext = os.path.splitext(entry.name)
            display = re.sub(r'_[a-z0-9]{6,12}$', '', stem, flags=re.IGNORECASE) + ext
            item = QListWidgetItem(display)
            item.setData(Qt.ItemDataRole.UserRole, entry)
            self._img_list.addItem(item)
        count = self._img_list.count()
        total = sum(1 for e in img.entries if e.name.lower().endswith('.dff'))
        self._set_status(f"IMG: {count} vehicles ({total} DFFs total)")

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

            # Step 2: Parse + load DFF, switch to 3D Preview tab first
            self._set_status(f"Parsing {name}…")
            QApplication.processEvents()
            if hasattr(self, '_tabs'):
                self._tabs.setCurrentIndex(0)
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


    #    Menu button handler
    def _on_menu_btn_clicked(self): #vers 2
        pm = QMenu(self)
        self._build_menus_into_qmenu(pm)
        btn = getattr(self, 'menu_btn', None)
        if btn:
            pm.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            pm.exec(self.cursor().pos())


    def _show_dropdown_menu(self): #vers 1
        """Pop up the workshop menus as a QMenu below the [Menu] button."""
        menu = QMenu(self)
        self._build_menus_into_qmenu(menu)
        btn = getattr(self, "menu_btn", None)
        pos = btn.mapToGlobal(btn.rect().bottomLeft()) if btn else self.cursor().pos()
        menu.exec(pos)

    def _show_popup_menu(self): #vers 1
        self._show_dropdown_menu()

    #    [ℹ] Info — About dialog

    def _show_about(self): #vers 1
        """[ℹ] button — show About / Info for this workshop."""
        author = getattr(self, "App_author",      "X-Seti")
        year   = getattr(self, "App_year",        "2026")
        desc   = getattr(self, "App_description", "")
        QMessageBox.information(self, f"About {self.App_name}",
            f"{self.App_name}   {self.App_build}\n\n"
            + (f"{desc}\n\n" if desc else "")
            + f"Copyright \u00a9 {year}  {author}\n"
              f"Part of IMG Factory 1.6 — a GTA modding toolkit.")

    def _open_app_settings(self): #vers 1
        try:
            try:
                from apps.utils.app_settings_system import SettingsDialog
            except ImportError:
                from app_settings_system import SettingsDialog
            if self.app_settings:
                dlg = SettingsDialog(self.app_settings, self)
                dlg.exec()
            else:
                self._show_workshop_settings()
        except Exception as e:
            self._set_status(f'Settings error: {e}')
            self._show_workshop_settings()

    #    [⚙] Cog — Global AppSettings theme dialog

    def _launch_theme_settings(self): #vers 1
        """[⚙] Cog — opens the global AppSettings / SettingsDialog.
        Identical pattern to radar_workshop._launch_theme_settings.
        """
        try:
            if not APPSETTINGS_AVAILABLE:
                QMessageBox.information(self, "Theme",
                    "AppSettings not available in this environment.")
                return
            if not self.app_settings:
                self.app_settings = AppSettings()
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
                self._refresh_icons()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error",
                f"Could not open theme settings:\n{e}")

    #    [Settings] — Workshop-local settings dialog

    def _show_workshop_settings(self): #vers 2
        """[Settings] button — workshop-local settings.
        Tabs: Fonts / Display / Menu / About
        Reads/writes WorkshopSettings (per-app JSON).
        """
        dlg = QDialog(self)
        dlg.setWindowTitle(f"{self.App_name} — Settings")
        dlg.setMinimumSize(520, 460)
        try:
            from apps.core.theme_utils import apply_dialog_theme
            apply_dialog_theme(dlg, self.main_window)
        except Exception:
            pass

        showws  = QVBoxLayout(dlg)
        tabs = QTabWidget()
        ws  = self.WS

        #    Tab 1: Fonts
        ft  = QWidget(); fl = QVBoxLayout(ft)

        def _font_row(label, fam_key, sz_key, def_fam, def_sz, mn=7, mx=32):  #vers 1
            grp = QGroupBox(label); row = QHBoxLayout(grp)
            fc = QFontComboBox()
            fc.setCurrentFont(__import__("PyQt6.QtGui", fromlist=["QFont"])
                              .QFont(ws.get(fam_key, def_fam)))
            sc = QSpinBox(); sc.setRange(mn, mx)
            sc.setValue(ws.get(sz_key, def_sz))
            sc.setSuffix(" pt"); sc.setFixedWidth(75)
            row.addWidget(fc); row.addWidget(sc)
            fl.addWidget(grp)
            return fc, sc

        fc_tit, sc_tit = _font_row("Title Font",
            "font_title_family",  "font_title_size",  "Arial",       14, 10, 32)
        fc_pan, sc_pan = _font_row("Panel / Header Font",
            "font_panel_family",  "font_panel_size",  "Arial",       10)
        fc_btn, sc_btn = _font_row("Button Font",
            "font_button_family", "font_button_size", "Arial",       10)
        fc_inf, sc_inf = _font_row("Info Bar Font",
            "font_info_family",   "font_info_size",   "Courier New",  9)
        fl.addStretch()
        tabs.addTab(ft, "Fonts")

        #    Tab 2: Display
        dt = QWidget(); dl = QVBoxLayout(dt)

        bm_grp = QGroupBox("Button Display Mode"); bm_lo = QVBoxLayout(bm_grp)
        bm_cb  = QComboBox()
        bm_cb.addItems(["Icons + Text", "Icons Only", "Text Only"])
        bm_cb.setCurrentIndex(
            {"both":0,"icons":1,"text":2}.get(ws.get("button_display_mode","both"),0))
        bm_lo.addWidget(bm_cb)
        bm_lo.addWidget(QLabel("Restart required to change button mode.",
                               styleSheet="color:#888;font-style:italic;"))
        dl.addWidget(bm_grp)

        sb_grp = QGroupBox("Status Bar"); sb_lo = QVBoxLayout(sb_grp)
        sb_chk = QCheckBox("Show status bar at bottom")
        sb_chk.setChecked(bool(ws.get("show_statusbar", True)))
        sb_lo.addWidget(sb_chk)
        dl.addWidget(sb_grp)

        sw_grp = QGroupBox("Sidebar"); sw_lo = QVBoxLayout(sw_grp)
        from PyQt6.QtWidgets import QFormLayout
        sw_form = QFormLayout()
        sw_spin = QSpinBox(); sw_spin.setRange(60,200)
        sw_spin.setValue(ws.get("sidebar_width", 82)); sw_spin.setSuffix(" px")
        sw_form.addRow("Sidebar width:", sw_spin)
        sw_lo.addLayout(sw_form)
        dl.addWidget(sw_grp)

        dl.addStretch()
        tabs.addTab(dt, "Display")

        #    Tab 3: Menu
        mt = QWidget(); ml = QVBoxLayout(mt)

        ms_grp = QGroupBox("Menu Style"); ms_lo = QVBoxLayout(ms_grp)
        ms_cb  = QComboBox()
        ms_cb.addItems(["Dropdown  ☰  (default)", "Top menu bar"])
        ms_cb.setCurrentIndex(
            0 if ws.get("menu_style","dropdown") == "dropdown" else 1)
        ms_lo.addWidget(ms_cb)
        ms_lo.addWidget(QLabel("Restart required to switch menu style.",
                               styleSheet="color:#888;font-style:italic;"))
        ml.addWidget(ms_grp)

        mf_grp = QGroupBox("Menu Font Size"); mf_lo = QFormLayout(mf_grp)
        mf_dd  = QSpinBox(); mf_dd.setRange(7,16)
        mf_dd.setValue(ws.get("menu_dropdown_font_size",9)); mf_dd.setSuffix(" pt")
        mf_lo.addRow("Dropdown font:", mf_dd)
        mf_bh  = QSpinBox(); mf_bh.setRange(18,40)
        mf_bh.setValue(ws.get("menu_bar_height",22)); mf_bh.setSuffix(" px")
        mf_lo.addRow("Bar height:", mf_bh)
        ml.addWidget(mf_grp)
        ml.addStretch()
        tabs.addTab(mt, "Menu")

        #    Tab 4: About
        at  = QWidget(); al = QVBoxLayout(at)
        atx = QTextEdit(); atx.setReadOnly(True)
        author = getattr(self, "App_author",      "X-Seti")
        year   = getattr(self, "App_year",        "2026")
        desc   = getattr(self, "App_description", "GUIWorkshop — IMG Factory 1.6")
        atx.setHtml(
            f"<h2>{self.App_name}</h2>"
            f"<p><b>Build:</b> {self.App_build}</p>"
            f"<p>{desc}</p>"
            f"<hr>"
            f"<p>Copyright &copy; {year} <b>{author}</b></p>"
            f"<p>Part of <b>IMG Factory 1.6</b> — a GTA modding toolkit.</p>"
            f"<p style='color:#888;'>Not affiliated with Rockstar Games "
            f"or Take-Two Interactive.</p>")
        al.addWidget(atx)
        from apps.methods.ui_sizes import add_sizes_tab       # own sizes or global
        add_sizes_tab(self, tabs)
        tabs.addTab(at, "About")

        #    Tab: Vehicle (game path, viewport settings)
        vt = QWidget(); vl = QVBoxLayout(vt)

        game_grp = QGroupBox("GTA Game Root"); gl = QHBoxLayout(game_grp)
        game_edit = QLineEdit(ws.get("game_root", ""))
        game_edit.setPlaceholderText("e.g. /home/user/GTASA-PC")
        browse_btn = QPushButton("Browse…")
        def _browse_game():  #vers 1
            from PyQt6.QtWidgets import QFileDialog
            d = QFileDialog.getExistingDirectory(dlg, "Select GTA Game Root", game_edit.text())
            if d: game_edit.setText(d)
        browse_btn.clicked.connect(_browse_game)
        gl.addWidget(game_edit, 1); gl.addWidget(browse_btn)
        vl.addWidget(game_grp)

        vp_grp = QGroupBox("3D Viewport"); vpl = QVBoxLayout(vp_grp)
        grid_cb = QCheckBox("Show grid on startup")
        grid_cb.setChecked(ws.get("viewport_grid", True))
        cull_cb = QCheckBox("Backface culling on startup")
        cull_cb.setChecked(ws.get("viewport_cull", True))
        vpl.addWidget(grid_cb); vpl.addWidget(cull_cb)
        vl.addWidget(vp_grp)
        vl.addStretch()
        tabs.addTab(vt, "Vehicle")

        #    Dialog buttons
        showws.addWidget(tabs)
        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        showws.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        #    Save
        ws.set("font_title_family",        fc_tit.currentFont().family())
        ws.set("font_title_size",          sc_tit.value())
        ws.set("font_panel_family",        fc_pan.currentFont().family())
        ws.set("font_panel_size",          sc_pan.value())
        ws.set("font_button_family",       fc_btn.currentFont().family())
        ws.set("font_button_size",         sc_btn.value())
        ws.set("font_info_family",         fc_inf.currentFont().family())
        ws.set("font_info_size",           sc_inf.value())
        ws.set("button_display_mode",      ["both","icons","text"][bm_cb.currentIndex()])
        ws.set("show_statusbar",           sb_chk.isChecked())
        ws.set("sidebar_width",            sw_spin.value())
        ws.set("menu_style",               "dropdown" if ms_cb.currentIndex()==0 else "topbar")
        ws.set("menu_dropdown_font_size",  mf_dd.value())
        ws.set("menu_bar_height",          mf_bh.value())
        ws.set("game_root",                game_edit.text())
        ws.set("viewport_grid",            grid_cb.isChecked())
        ws.set("viewport_cull",            cull_cb.isChecked())
        ws.save()

        # Live-apply without restart where possible
        self._load_fonts_from_settings()
        if hasattr(self, "title_label"):
            self.title_label.setFont(self.title_font)
        if hasattr(self, "_status_widget"):
            self._status_widget.setVisible(ws.get("show_statusbar", True))
        if hasattr(self, "_sidebar_frame"):
            self._sidebar_frame.setFixedWidth(ws.get("sidebar_width", 82))
        self._set_status("Settings saved.")

    #    Theme helpers

    def _get_icon_color(self) -> str: #vers 1
        """Returns text_primary from current theme."""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            try:
                return self.app_settings.get_theme_colors().get(
                    "text_primary", "#e0e0e0")
            except Exception:
                pass
        bg = self.palette().window().color()
        return "#e0e0e0" if bg.lightness() < 128 else "#202020"

    def _get_accent_color(self) -> str: #vers 1
        """Returns accent_primary from current theme."""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            try:
                return self.app_settings.get_theme_colors().get(
                    "accent_primary", "#4682FF")
            except Exception:
                pass
        return "#4682FF"

    def _apply_theme(self): #vers 1
        """Apply QSS from AppSettings."""
        if self.app_settings:
            try:
                qss = self.app_settings.get_stylesheet()
                if qss: self.setStyleSheet(qss)
            except Exception:
                pass

    def _refresh_icons(self): #vers 3
        """Called on theme change — re-apply theme and recolour titlebar icons."""
        self._apply_theme()
        self.update()                                   # corner handle colour
        self._refresh_titlebar_icons()


#
# SECTION 3 — Layout: setup_ui, left panel, centre panel, right panel, status
#
