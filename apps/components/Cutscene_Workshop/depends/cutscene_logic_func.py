#this belongs in apps/components/Cutscene_Workshop/depends/cutscene_logic_func.py - Version: 27
# X-Seti - November 02 2026 - IMG Factory 1.6 - Cutscene Workshop logic

"""
Cutscene Workshop: open cuts.img or a folder of .cut / .dat files, edit the
script sections and camera tracks, save back (IMG entries or loose files),
and play the cutscene in 3D (models from cutscene.img / gta3.img; GTA IV:
cuts.img .cut + .wad, models from cutsprops.img / componentpeds.img).
"""

##Methods list -
#
##class CutsceneLogicMixin: -
# _cam_add_key
# _cam_del_key
# _cam_drag_key
# _cam_show_track
# _cam_table_changed
# _cam_time_changed
# _cam_update_view
# _cs_add_row
# _cs_audio_find
# _cs_audio_load
# _cs_autoload
# _cs_cam_eye
# _cs_clothes_rules
# _cs_del_row
# _cs_fetch
# _cs_locate
# _cs_model_arcs
# _cs_actor_drag
# _cs_cut_text
# _cs_fill_list
# _cs_iv_camera
# _cs_iv_camera_save
# _cs_map_load
# _cs_map_offset
# _cs_map_toggle
# _cs_match_flag
# _cs_open_img
# _cs_orbit_from_cut
# _cs_preview_camera
# _cs_preview_centre
# _cs_preview_draw
# _cs_preview_free
# _cs_preview_load
# _cs_preview_play
# _cs_preview_set_time
# _cs_preview_step
# _cs_preview_tick
# _cs_preview_zoom
# _cs_read
# _cs_select
# _cs_show_section
# _cs_tab_changed
# _cs_table_changed
# _cs_write
# _open_file
# _save_file

import os

from PyQt6.QtWidgets import QFileDialog, QMessageBox, QTableWidgetItem

from apps.methods.cutscene_files import (CUT_COLUMNS, IV_CUT_COLUMNS, build_cam_dat, build_cut, build_iv_cut,
                                         cam_at, iv_cut_parts, parse_cam_dat, parse_cut, parse_iv_cut)


class CutsceneLogicMixin: #vers 1
    """Open / edit / save cutscene scripts and camera paths."""

    def _cs_cut_text(self) -> str: #vers 1
        """The .cut text of the current sections (IV: original layout kept)."""
        if getattr(self, '_cs_iv', False):
            return build_iv_cut(self._cs_iv_text, self._cs_iv_spans, self._cs_sections)
        return build_cut(self._cs_sections)

    def _cs_iv_camera(self) -> list: #vers 2
        """GTA IV camera (.wad camera anims) as FOV / roll / position / target tracks."""
        import math
        from apps.methods.iv_anim import cam_quat_view, wad_extra_tracks
        tracks, t0 = [[], [], [], []], 0.0
        for part in iv_cut_parts(self._cs_sections):
            raw = self._cs_iv_wads.get(part['anim'].lower())
            cam = wad_extra_tracks(raw).get(part['camera'].lower(), {}) if raw else {}
            pos, rot, fov = cam.get('cam_pos'), cam.get('cam_rot'), cam.get('cam_fov')
            if pos is not None and rot is not None:
                for f in range(len(pos)):                    # every frame: camera cuts stay sharp
                    t = t0 + f / 30.0
                    fwd, roll = cam_quat_view(rot[min(f, len(rot) - 1)])
                    v = float(fov[min(f, len(fov) - 1)][0]) if fov is not None else 45.0
                    h = math.degrees(2 * math.atan(math.tan(math.radians(v) / 2) / 0.75))   # vertical -> 4:3 h
                    tracks[0].append((t, [h]))
                    tracks[1].append((t, [roll]))
                    tracks[2].append((t, [float(x) for x in pos[f]]))
                    tracks[3].append((t, [float(x) for x in pos[f] + fwd * 5.0]))
            t0 += part['duration']
        return tracks

    def _cs_iv_camera_save(self) -> list: #vers 1
        """Edited camera tracks back into each section's .wad; names of the .wads written."""
        import math
        import numpy as np
        from apps.methods.iv_anim import cam_view_quat, wad_extra_tracks, write_wad_camera
        tr, done, t0 = self._cam_tracks, [], 0.0
        for part in iv_cut_parts(self._cs_sections):
            raw = self._cs_iv_wads.get(part['anim'].lower())
            cam = wad_extra_tracks(raw).get(part['camera'].lower(), {}) if raw else {}
            old_rot, n = cam.get('cam_rot'), len(cam.get('cam_pos', []))
            if n and old_rot is not None:
                pos, rot, fov = np.zeros((n, 3)), np.zeros((n, 4)), np.zeros((n, 1))
                for f in range(n):
                    t = t0 + f / 30.0
                    e, g = np.array(cam_at(tr[2], t)[:3]), np.array(cam_at(tr[3], t)[:3])
                    roll = cam_at(tr[1], t)
                    h = cam_at(tr[0], t)
                    pos[f] = e
                    rot[f] = cam_view_quat(g - e, roll[0] if roll else 0.0, old_rot[min(f, len(old_rot) - 1)])
                    hv = h[0] if h else 45.0
                    fov[f] = math.degrees(2 * math.atan(math.tan(math.radians(hv) / 2) * 0.75))
                new = write_wad_camera(raw, part['camera'], pos, rot, fov)
                self._cs_write(part['anim'] + '.wad', new)
                self._cs_iv_wads[part['anim'].lower()] = new
                done.append(part['anim'] + '.wad')
            t0 += part['duration']
        return done

    def _open_file(self, path=None): #vers 3
        """cuts.img, or a .cut / .dat (its folder's cutscenes are listed)."""
        if not path:
            path, _ = QFileDialog.getOpenFileName(self, "Open cutscenes", "",
                                                  "Cutscenes (*.img *.dir *.cut *.dat);;All files (*)")
        if not path:
            return
        self._cs_img, self._cs_dir, names = None, None, []
        if path.lower().endswith(('.img', '.dir')):
            arc = self._cs_open_img(path)
            if arc is None:
                QMessageBox.warning(self, self.App_name, f"Couldn't open {path}")
                return
            self._cs_img = arc
            files = [e.name for e in arc.entries]
        else:
            self._cs_dir = os.path.dirname(path)
            files = os.listdir(self._cs_dir)
        cuts = {f[:-4] for f in files if f.lower().endswith('.cut')}
        dats = {f[:-4] for f in files if f.lower().endswith('.dat')}
        names = list(cuts or dats)                       # III / VC: no .cut, list the camera files
        self.WS.set('last_path', path)                   # reopened next time
        self.WS.add_recent(path)
        self.WS.save()
        self._cs_names = sorted(names, key=str.lower)
        self._cs_cur, self._cs_dirty = None, False
        self._cs_src_lbl.setText(f"{os.path.basename(path)}: {len(names)} cutscenes")
        self._cs_fill_list()
        if not path.lower().endswith(('.img', '.dir')):
            stem = os.path.splitext(os.path.basename(path))[0]
            hits = self._cs_list.findItems(stem, self._cs_match_flag())
            if hits:
                self._cs_list.setCurrentItem(hits[0])

    def _cs_autoload(self, game_root: str = ''): #vers 1
        """Game folder given: its anim/cuts (.dir or .img); else the last cutscenes opened."""
        if game_root and os.path.isdir(game_root):
            anim = next((os.path.join(game_root, d) for d in os.listdir(game_root) if d.lower() == 'anim'), None)
            if anim:
                files = {f.lower(): f for f in os.listdir(anim)}
                hit = files.get('cuts.dir') or files.get('cuts.img')
                if hit:
                    self._open_file(os.path.join(anim, hit))
                    return
        last = self.WS.get('last_path', '')
        if last and os.path.exists(last):
            self._open_file(last)

    @staticmethod
    def _cs_open_img(path: str): #vers 2
        """IMGFile for path: SA VER2 .img directly (a stray .dir beside it is ignored);
        III / VC .img through its .dir."""
        from apps.methods.img_core_classes import IMGFile
        stem = os.path.splitext(path)[0]
        img = next((stem + e for e in ('.img', '.IMG') if os.path.isfile(stem + e)), None)
        ver2 = False
        if img:
            with open(img, 'rb') as f:
                ver2 = f.read(4) == b'VER2'
        d = next((stem + e for e in ('.dir', '.DIR') if os.path.isfile(stem + e)), None)
        arc = IMGFile(img if ver2 else (d or path))
        return arc if arc.open() else None

    def _cs_map_offset(self): #vers 2
        """(x, y, z, heading): SA .cut info offset, IV first [OFFSET] / [ORIENT]; or None."""
        if getattr(self, '_cs_iv', False):
            secs = dict(reversed(self._cs_sections or []))      # first of each tag
            try:
                x, y, z = (float(v) for v in secs['offset'][0].split()[:3])
                return x, y, z, float(secs.get('orient', ['0'])[0].split()[0])
            except (KeyError, IndexError, ValueError):
                return None
        for row in dict(self._cs_sections or []).get('info', []):
            parts = row.replace(',', ' ').split()
            if len(parts) == 4 and parts[0].lower() == 'offset':
                try:
                    return tuple(float(v) for v in parts[1:]) + (0.0,)
                except ValueError:
                    return None
        return None

    def _cs_map_toggle(self, on: bool): #vers 1
        """Map around the scene on / off (saved)."""
        self.WS.set('show_map', bool(on))
        self.WS.save()
        self._cs_map_load()

    def _cs_map_load(self): #vers 4
        """Load the game map once (background thread), then show objects around the offset."""
        import threading
        vp = self._pv_vp
        if vp is None:
            return
        if not self.WS.get('show_map', False):
            vp.clear_world_instances()
            self._cam_view.set_backdrop([])
            return
        offset = self._cs_map_offset()
        if offset is None:
            vp.clear_world_instances()
            self._cam_view.set_backdrop([])
            self._set_status(f"{self._cs_cur}: no offset in the .cut (III / VC), no map")
            return
        base = os.path.dirname(self._cs_img.file_path) if self._cs_img is not None else self._cs_dir
        root = os.path.dirname(base or '')
        game = 'iv' if getattr(self, '_cs_iv', False) else 'sa'
        if game == 'iv':                                      # pc/anim -> game root (common/ + pc/)
            root = os.path.dirname(root)
        offset, heading = offset[:3], offset[3]
        world = getattr(self, '_cs_world', None)
        if world is not None and world[0] == root:
            if world[1] is None:                              # still loading
                return
            from apps.methods.world_scene import build_entries, instances_near
            near = instances_near(world[1], offset, float(self.WS.get('map_radius', 150)))
            entries, tex = build_entries(world[2], near, offset, heading)
            vp._upload_textures(tex, additive=True)
            vp.set_world_instances(entries, auto_fit=False)
            from apps.methods.world_scene import footprints
            self._cam_view.set_backdrop(footprints(entries))      # plan view: map from above
            self._set_status(f"{self._cs_cur}: map, {len(entries)} objects around "
                             f"{offset[0]:.0f}, {offset[1]:.0f}, {offset[2]:.0f}")
            return
        self._cs_world = (root, None, None)
        self._set_status("Loading the game map (first time, about 20 s)...")
        result = {}

        def work():
            from apps.methods.world_scene import load_world
            result['w'] = load_world(root, game)
        threading.Thread(target=work, daemon=True).start()
        from PyQt6.QtCore import QTimer
        timer = QTimer(self)
        timer.setInterval(250)

        def poll():
            if 'w' not in result:
                return
            timer.stop()
            loader, cache = result['w']
            if loader is None:
                self._cs_world = None
                self._set_status("Map: game data not readable")
                return
            self._cs_world = (root, loader, cache)
            self._cs_map_load()
        timer.timeout.connect(poll)
        timer.start()

    def _cs_match_flag(self): #vers 1
        from PyQt6.QtCore import Qt
        return Qt.MatchFlag.MatchFixedString

    def _cs_fill_list(self, *_): #vers 1
        """List the cutscenes matching the filter."""
        q = self._cs_filter.text().lower()
        self._cs_list.blockSignals(True)
        self._cs_list.clear()
        self._cs_list.addItems([n for n in getattr(self, '_cs_names', []) if q in n.lower()])
        self._cs_list.blockSignals(False)

    def _cs_read(self, filename: str) -> str: #vers 1
        """Text of a cutscene file from the IMG or folder ('' if missing)."""
        if self._cs_img is not None:
            e = next((x for x in self._cs_img.entries if x.name.lower() == filename.lower()), None)
            data = self._cs_img.read_entry_data(e) if e is not None else b''
        else:
            p = os.path.join(self._cs_dir, filename)
            match = [f for f in os.listdir(self._cs_dir) if f.lower() == filename.lower()]
            p = os.path.join(self._cs_dir, match[0]) if match else p
            data = open(p, 'rb').read() if os.path.isfile(p) else b''
        return data.rstrip(b'\0').decode('latin-1')

    def _cs_write(self, filename: str, text) -> bool: #vers 2
        """Write a cutscene file back (IMG entry replaced, or loose file); text or bytes."""
        data = text if isinstance(text, bytes) else text.encode('latin-1')
        if self._cs_img is not None:
            e = next((x for x in self._cs_img.entries if x.name.lower() == filename.lower()), None)
            return self._cs_img.add_entry(e.name if e else filename, data)
        from apps.methods.file_backup import safe_write_bytes
        match = [f for f in os.listdir(self._cs_dir) if f.lower() == filename.lower()]
        safe_write_bytes(os.path.join(self._cs_dir, match[0] if match else filename), data)
        return True

    def _cs_select(self, name: str): #vers 7
        """Load a cutscene's .cut and .dat (asks to save edits first)."""
        if not name or name == getattr(self, '_cs_cur', None):
            return
        if getattr(self, '_cs_dirty', False):
            r = QMessageBox.question(self, self.App_name, f"Save changes to {self._cs_cur}?",
                                     QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard)
            if r == QMessageBox.StandardButton.Save:
                self._save_file()
        raw_cut = self._cs_read(name + '.cut')
        iv = raw_cut.lstrip().startswith('[')            # GTA IV: [TAG] blocks, camera in the .wad
        try:
            if iv:
                sections, spans = parse_iv_cut(raw_cut)
            else:
                sections = parse_cut(raw_cut)
                tracks = parse_cam_dat(self._cs_read(name + '.dat'))
        except (ValueError, IndexError) as e:
            QMessageBox.warning(self, self.App_name, f"{name}: camera / script file not readable ({e})")
            return
        self._cs_cur = name
        self._cs_iv = iv
        self._cs_sections = sections
        if iv:
            self._cs_iv_text, self._cs_iv_spans = raw_cut, spans
            self._cs_iv_wads = {p['anim'].lower(): self._cs_fetch(p['anim'], '.wad') for p in iv_cut_parts(sections)}
            tracks = self._cs_iv_camera()
            import copy
            self._cam_orig = copy.deepcopy(tracks)               # camera saved only when edited
        if not self._cs_sections:
            self._set_status(f"{name}: no .cut (III / VC keep the script in main.scm); camera and preview only")
        self._cam_tracks = tracks
        self._cs_loading = True
        self._cs_sec_combo.clear()
        self._cs_sec_combo.addItems([f"{s}  ({len(r)})" for s, r in self._cs_sections])
        self._cs_text.setPlainText(self._cs_cut_text().replace('\r\n', '\n'))
        self._cs_loading = False
        self._cs_show_section(0)
        end = max((tr[-1][0] for tr in self._cam_tracks if tr), default=0.0)
        self._cam_end = end
        self._cam_show_track(self._cam_combo.currentIndex())
        self._cs_dirty = False
        self._set_status(f"{name}: {len(self._cs_sections)} sections, camera {end:.2f} s")
        self._cast_swaps = {}                            # stand-ins belong to one cutscene
        self._cast_fill()
        self._cs_preview_load()                          # preview is always on show

    def _cs_show_section(self, idx: int): #vers 2
        """Fill the table with one section's rows split at commas."""
        secs = getattr(self, '_cs_sections', [])
        self._cs_loading = True
        t = self._cs_table
        t.clear()
        if not (0 <= idx < len(secs)):
            t.setRowCount(0)
            self._cs_loading = False
            return
        name, rows = secs[idx]
        iv = getattr(self, '_cs_iv', False)
        sep = None if name == 'info' or iv else ','
        cells = [[c.strip() for c in (r.split() if iv else r.split(None, 1) if sep is None else r.split(','))]
                 for r in rows]
        cols = list((IV_CUT_COLUMNS if iv else CUT_COLUMNS).get(name, []))
        width = max([len(c) for c in cells] + [len(cols), 1])
        cols += [f"Value {i + 1}" for i in range(len(cols), width)]
        t.setColumnCount(width)
        t.setHorizontalHeaderLabels(cols)
        t.setRowCount(len(cells))
        for r, row in enumerate(cells):
            for c in range(width):
                t.setItem(r, c, QTableWidgetItem(row[c] if c < len(row) else ''))
        t.resizeColumnsToContents()
        self._cs_loading = False

    def _cs_table_changed(self, _item=None): #vers 2
        """Table edited: rebuild the section's rows."""
        if getattr(self, '_cs_loading', False):
            return
        idx = self._cs_sec_combo.currentIndex()
        if not (0 <= idx < len(self._cs_sections)):
            return
        name = self._cs_sections[idx][0]
        t, rows = self._cs_table, []
        for r in range(t.rowCount()):
            vals = [(t.item(r, c).text().strip() if t.item(r, c) else '') for c in range(t.columnCount())]
            while vals and not vals[-1]:
                vals.pop()
            if vals:
                if getattr(self, '_cs_iv', False):           # IV: tabs or spaces, as the file had
                    tab = any('\t' in x for x in self._cs_sections[idx][1])
                    rows.append(('\t' if tab else ' ').join(vals))
                else:
                    rows.append(' '.join(vals) if name == 'info' else ','.join(vals))
        self._cs_sections[idx] = (name, rows)
        self._cs_dirty = True

    def _cs_add_row(self): #vers 1
        t = self._cs_table
        t.insertRow(max(0, t.currentRow() + 1) if t.rowCount() else 0)

    def _cs_del_row(self): #vers 1
        t = self._cs_table
        if t.currentRow() >= 0:
            t.removeRow(t.currentRow())
            self._cs_table_changed()

    def _cs_tab_changed(self, idx: int): #vers 5
        """Script text tab: show the .cut text on entry, take it back on leaving."""
        text_tab = self._cs_tabs.indexOf(self._cs_text)
        prev, self._cs_prev_tab = getattr(self, '_cs_prev_tab', -1), idx
        if getattr(self, '_cs_cur', None) is None:
            return
        if idx == text_tab:
            self._cs_text.setPlainText(self._cs_cut_text().replace('\r\n', '\n'))
        elif prev == text_tab:
            text = self._cs_text.toPlainText()
            if getattr(self, '_cs_iv', False):
                if text == self._cs_cut_text().replace('\r\n', '\n'):
                    return
                text = text.replace('\n', '\r\n') if '\r\n' in self._cs_iv_text else text
                new, self._cs_iv_spans = parse_iv_cut(text)
                self._cs_iv_text = text
            else:
                new = parse_cut(text)
            if new != self._cs_sections:
                self._cs_sections = new
                self._cs_dirty = True
                cur = self._cs_sec_combo.currentIndex()
                self._cs_sec_combo.blockSignals(True)
                self._cs_sec_combo.clear()
                self._cs_sec_combo.addItems([f"{s}  ({len(r)})" for s, r in new])
                self._cs_sec_combo.setCurrentIndex(min(max(cur, 0), len(new) - 1))
                self._cs_sec_combo.blockSignals(False)
                self._cs_show_section(self._cs_sec_combo.currentIndex())

    def _cam_show_track(self, idx: int): #vers 2
        """Fill the key table for one camera track."""
        tracks = getattr(self, '_cam_tracks', None)
        if not tracks:
            return
        tr = tracks[idx]
        width = max([len(v) for _t, v in tr] + [1])
        heads = ["Time s"] + (["X", "Y", "Z", "In X", "In Y", "In Z", "Out X", "Out Y", "Out Z"][:width]
                              if width == 9 else ["Value", "In", "Out"][:width] + [f"V{i}" for i in range(4, width + 1)])
        self._cs_loading = True
        t = self._cam_table
        t.clear()
        t.setColumnCount(1 + width)
        t.setHorizontalHeaderLabels(heads)
        t.setRowCount(len(tr))
        for r, (time_s, vals) in enumerate(tr):
            t.setItem(r, 0, QTableWidgetItem(f"{time_s:.3f}"))
            for c, v in enumerate(vals):
                t.setItem(r, c + 1, QTableWidgetItem(f"{v:.4f}"))
        t.resizeColumnsToContents()
        self._cs_loading = False
        self._cam_update_view()
        if hasattr(self, '_shots_tbl'):
            self._shots_fill()                           # shot list follows the keys

    def _cam_table_changed(self, _item=None): #vers 1
        """Key table edited: rebuild the track (sorted by time)."""
        if getattr(self, '_cs_loading', False) or not getattr(self, '_cam_tracks', None):
            return
        t, keys = self._cam_table, []
        for r in range(t.rowCount()):
            try:
                vals = [float(t.item(r, c).text()) for c in range(t.columnCount()) if t.item(r, c)]
            except ValueError:
                self._set_status(f"Row {r + 1}: numbers only")
                return
            if vals:
                keys.append((vals[0], vals[1:]))
        self._cam_tracks[self._cam_combo.currentIndex()] = sorted(keys, key=lambda k: k[0])
        self._cam_end = max((tr[-1][0] for tr in self._cam_tracks if tr), default=0.0)
        self._cs_dirty = True
        self._cam_update_view()

    def _cam_add_key(self): #vers 1
        """New key copied from the selected one, half a second later."""
        tr = self._cam_tracks[self._cam_combo.currentIndex()] if getattr(self, '_cam_tracks', None) else None
        if tr is None:
            return
        r = self._cam_table.currentRow()
        base = tr[r] if 0 <= r < len(tr) else (tr[-1] if tr else (0.0, [0.0, 0.0, 0.0]))
        tr.append((base[0] + 0.5, list(base[1])))
        tr.sort(key=lambda k: k[0])
        self._cs_dirty = True
        self._cam_show_track(self._cam_combo.currentIndex())

    def _cam_del_key(self): #vers 1
        tr = self._cam_tracks[self._cam_combo.currentIndex()] if getattr(self, '_cam_tracks', None) else None
        r = self._cam_table.currentRow()
        if tr is not None and 0 <= r < len(tr):
            del tr[r]
            self._cs_dirty = True
            self._cam_show_track(self._cam_combo.currentIndex())

    def _cam_drag_key(self, track: int, key: int, x: float, y: float, done: bool): #vers 1
        """Path view drag: move a position / target key (its tangents follow)."""
        tr = self._cam_tracks[track]
        if not (0 <= key < len(tr)):
            return
        t, vals = tr[key]
        dx, dy = x - vals[0], y - vals[1]
        nv = list(vals)
        for i in range(0, len(nv) - 1, 3):               # pos, in, out: x / y moved together
            nv[i], nv[i + 1] = nv[i] + dx, nv[i + 1] + dy
        tr[key] = (t, nv)
        self._cs_dirty = True
        if done:
            if self._cam_combo.currentIndex() != track:
                self._cam_combo.setCurrentIndex(track)
            else:
                self._cam_show_track(track)
            self._cam_table.selectRow(key)
        else:
            self._cam_update_view()

    def _cam_time_changed(self, _v=None): #vers 1
        self._cam_update_view()

    def _cam_update_view(self): #vers 4
        """Path view: shots, played trail and view cone at the slider time."""
        tracks = getattr(self, '_cam_tracks', None) or [[], [], [], []]
        end = getattr(self, '_cam_end', 0.0)
        now_t = end * self._cam_slider.value() / 1000.0
        self._cam_time_lbl.setText(f"{now_t:.2f} / {end:.2f} s")
        fov = tracks[0]                                 # a repeated FOV key time is a camera cut
        cuts = sorted({fov[i][0] for i in range(1, len(fov)) if fov[i][0] == fov[i - 1][0]})

        def shots(tr):                                  # also split at big jumps (unmarked cuts)
            pts = [(t, (v[0], v[1], k)) for k, (t, v) in enumerate(tr) if len(v) >= 2]
            steps = sorted(((b[0] - a[0]) ** 2 + (b[1] - a[1]) ** 2) ** 0.5
                           for (_s, a), (_u, b) in zip(pts, pts[1:]))
            jump = max(2.0, 8 * steps[len(steps) // 2]) if steps else 1e9
            out, k = [], 0
            for i, (t, q) in enumerate(pts):
                new = not out or ((q[0] - pts[i - 1][1][0]) ** 2 + (q[1] - pts[i - 1][1][1]) ** 2) ** 0.5 > jump
                while k < len(cuts) and t >= cuts[k]:
                    k, new = k + 1, True
                if new:
                    out.append([])
                out[-1].append(q)
            return out

        def played(tr, cur):                            # keys up to now, then the current point
            return [tuple(v[:2]) for t, v in tr if t <= now_t and len(v) >= 2] + ([tuple(cur[:2])] if len(cur) >= 2 else [])
        cam, tgt = shots(tracks[2]), shots(tracks[3])
        c, g = cam_at(tracks[2], now_t), cam_at(tracks[3], now_t)
        now = (tuple(c[:2]), tuple(g[:2])) if len(c) >= 2 and len(g) >= 2 else None
        f = cam_at(tracks[0], now_t)
        self._cam_view.set_path(cam, tgt, now, (played(tracks[2], c), played(tracks[3], g)),
                                f[0] if f else None)
        if f:
            self._cam_time_lbl.setText(f"{now_t:.2f} / {end:.2f} s   FOV {f[0]:.1f}")

    def _save_file(self): #vers 5
        """Write the current cutscene's .cut and .dat back."""
        name = getattr(self, '_cs_cur', None)
        if not name:
            return
        if self._cs_tabs.currentWidget() is self._cs_text:
            self._cs_tab_changed(-1)                         # take the edited text
            self._cs_prev_tab = self._cs_tabs.currentIndex()
        ok = self._cs_write(name + '.cut', self._cs_cut_text())
        iv = getattr(self, '_cs_iv', False)
        anim = ''
        if getattr(self, '_pv_anim_dirty', False) and getattr(self._pv_scene, 'ifp', None):
            from apps.methods.ifp_format import build_ifp
            ok = self._cs_write(name + '.ifp', build_ifp(self._pv_scene.ifp)) and ok   # moved routes
            self._pv_anim_dirty, anim = False, f", {name}.ifp"
        wads = []
        if not iv:
            ok = self._cs_write(name + '.dat', build_cam_dat(self._cam_tracks)) and ok
        elif self._cam_tracks != getattr(self, '_cam_orig', None):
            try:
                wads = self._cs_iv_camera_save()               # IV camera lives in the .wad parts
                import copy
                self._cam_orig = copy.deepcopy(self._cam_tracks)
            except ValueError as e:
                QMessageBox.warning(self, self.App_name, f"Camera not saved: {e}")
        if ok:
            self._cs_dirty = False
            self._set_status(f"Saved {name}.cut" + (f" and {', '.join(wads)}" if wads else "" if iv
                                                     else f" and {name}.dat") + anim)
        else:
            QMessageBox.warning(self, self.App_name, f"Couldn't save {name}")

    def _cs_actor_drag(self, n: int, t: float, x: float, y: float, finished: bool): #vers 1
        """Plan view: an actor node dragged to (x, y): bend its route around time t."""
        sc = getattr(self, '_pv_scene', None)
        paths = getattr(self, '_pv_paths', None)
        if sc is None or not paths or n >= len(paths) or not paths[n][1]:
            return
        for _ in range(3):                               # node lands on the cursor (keys between frames)
            cur = min(paths[n][1], key=lambda r: abs(r[0] - t))
            if abs(x - cur[1]) + abs(y - cur[2]) < 0.02:
                break
            if not sc.move_actor(n, t, x - cur[1], y - cur[2]):
                self._set_status(f"{paths[n][0]}: route can't be moved here (GTA IV mover, or no root track)")
                return
            for i, it in enumerate(sc.items):            # items sharing the anim move too
                if it.get('anim') is sc.items[n].get('anim'):
                    paths[i] = sc.actor_paths(only=i)[0]
        self._cs_preview_draw()
        if finished:
            self._pv_anim_dirty = self._cs_dirty = True
            self._set_status(f"{paths[n][0]}: route moved at {t:.1f} s (Save writes {self._cs_cur}.ifp)")

    def _cs_model_arcs(self) -> list: #vers 3
        """[(path, IMGFile)] of the model IMGs (SA cutscene / gta3 / player; IV cutsprops / peds / cars)."""
        base = os.path.dirname(self._cs_img.file_path) if self._cs_img is not None else self._cs_dir
        if not base:
            return []
        root = os.path.dirname(base)
        cache = self.__dict__.setdefault('_cs_arcs', {})
        out = []
        for rel in (('models', 'cutscene.img'), ('models', 'gta3.img'), ('models', 'player.img'),
                    ('anim', 'cutsprops.img'), ('models/cdimages', 'componentpeds.img'),     # GTA IV
                    ('models/cdimages', 'pedprops.img'), ('models/cdimages', 'vehicles.img'),
                    ('models/cdimages', 'playerped.rpf')):
            path = next((os.path.join(d, rel[1]) for d in (os.path.join(root, rel[0]), os.path.join(base, rel[0]))
                         if os.path.isfile(os.path.join(d, rel[1]))), None)
            if path is None:
                continue
            arc = cache.get(path)
            if arc is None:
                arc = self._cs_open_img(path)
                if arc is None:
                    continue
                cache[path] = arc
            out.append((path, arc))
        return out

    def _cs_locate(self, name: str, ext: str): #vers 2
        """Where name+ext is: ('file', path), (IMGFile, entry), or None."""
        want = (name + ext).lower()
        if self._cs_dir is not None:
            hit = [f for f in os.listdir(self._cs_dir) if f.lower() == want]
            if hit:
                return 'file', os.path.join(self._cs_dir, hit[0])
        if ext in ('.ifp', '.wad') and self._cs_img is not None:
            e = next((x for x in self._cs_img.entries if x.name.lower() == want), None)
            return (self._cs_img, e) if e is not None else None
        for _path, arc in self._cs_model_arcs():
            e = next((x for x in arc.entries if x.name.lower() == want), None)
            if e is not None:
                return arc, e
        return None

    def _cs_fetch(self, name: str, ext: str): #vers 4
        """Bytes of name+ext: cutscene folder, then models/cutscene.img, gta3.img, player.img."""
        hit = self._cs_locate(name, ext)
        if hit is None:
            return None
        if hit[0] == 'file':
            with open(hit[1], 'rb') as f:
                return f.read()
        return hit[0].read_entry_data(hit[1])

    def _cs_clothes_rules(self): #vers 1
        """data/clothes.dat of the game around the cutscenes, parsed (None if missing)."""
        from apps.methods.cj_player import parse_clothes_dat
        base = os.path.dirname(self._cs_img.file_path) if self._cs_img is not None else self._cs_dir
        for root in (os.path.dirname(base), base):
            data = next((os.path.join(root, d) for d in (os.listdir(root) if os.path.isdir(root) else [])
                         if d.lower() == 'data'), None)
            if data:
                hit = next((f for f in os.listdir(data) if f.lower() == 'clothes.dat'), None)
                if hit:
                    with open(os.path.join(data, hit), encoding='latin-1') as f:
                        return parse_clothes_dat(f.read())
        return None

    def _cs_preview_load(self): #vers 13
        """Build the 3D scene and sound for the current cutscene (viewport made on first use)."""
        if self._pv_vp is None:
            from PyQt6.QtCore import QObject, Qt
            from apps.methods.dff_viewport import DFFViewport
            self._pv_vp = DFFViewport()
            self._pv_vp.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
            self._cs_apply_view()
            self._pv_vp.set_array_assembly(True)          # vertex arrays: playback speed
            from PyQt6.QtWidgets import QLCDNumber
            lcd = self._pv_fps = QLCDNumber(3, self._pv_vp)  # fps, top left
            lcd.setSegmentStyle(QLCDNumber.SegmentStyle.Flat)
            lcd.setStyleSheet("QLCDNumber { color: #39FF5A; background: rgba(0, 0, 0, 150); border: 1px solid #1E7A30; }")
            lcd.setFixedSize(54, 24)
            lcd.move(6, 6)
            lcd.setToolTip("Frames per second while playing")
            lcd.display('--')
            self._pv_frames, self._pv_fps_clock = 0, 0.0
            self._pv_host.addWidget(self._pv_vp)
            ws = self

            class _Filter(QObject):
                def eventFilter(self, obj, event): #vers 1
                    return ws._cs_preview_free(obj, event)
            self._pv_filter = _Filter(self)
            self._pv_vp.installEventFilter(self._pv_filter)
        if self._pv_for == self._cs_cur and self._pv_scene is not None:
            return
        from apps.methods.cutscene_scene import CutsceneScene
        self._set_status(f"Loading {self._cs_cur} models...")
        if self._pv_play.isChecked():
            self._pv_play.setChecked(False)
        if getattr(self, '_cs_iv', False):
            names = frozenset(e.name.lower() for _p, a in self._cs_model_arcs() for e in a.entries)
            self._pv_scene = CutsceneScene.from_iv(iv_cut_parts(self._cs_sections), self._cs_iv_wads,
                                                   self._cs_cast_fetch, names)
        else:
            self._pv_scene = CutsceneScene(self._cs_sections, self._cs_fetch(self._cs_cur, '.ifp'),
                                           self._cs_cast_fetch, self._cs_clothes_rules())
            self._pv_scene.lift = float(self.WS.get('model_lift', 1.0))   # SA: 1 m = map ground
        self._pv_for = self._cs_cur
        self._pv_paths = self._pv_scene.actor_paths()              # plan view routes
        self._pv_vp.clear_textures()
        self._pv_vp._upload_textures(self._pv_scene.textures())
        sc = self._pv_scene
        sound = self._cs_audio_load()
        self._pv_notes.setText(f"{len(sc.items)} models, {sc.duration:.1f} s, {sound}"
                               + ("   " + "; ".join(sc.notes) if sc.notes else ""))
        self._pv_t = 0.0
        self._pv_slider.blockSignals(True)
        self._pv_slider.setValue(0)
        self._pv_slider.blockSignals(False)
        self._cs_preview_draw()
        if not self._pv_camera.isChecked():
            self._cs_orbit_from_cut()
        self._set_status(f"{self._cs_cur}: preview ready")
        if self.WS.get('show_map', False):
            self._cs_map_load()

    def _cs_preview_draw(self): #vers 5
        """Pose models at the preview time; cutscene camera when locked; path view follows."""
        import math
        sc, vp = self._pv_scene, self._pv_vp
        if sc is None or vp is None:
            return
        t = self._pv_t
        vp._vertices = []
        vp._all_geoms = sc.geoms_at(t)
        tr = self._cam_tracks
        end = max(sc.duration, getattr(self, '_cam_end', 0.0)) or 1.0
        self._pv_time.setText(f"{t:.2f} / {end:.2f} s  F{round(t * 30)}")
        if not self._cam_view.isHidden():                # plan view shown
            self._cam_view.set_actors(getattr(self, '_pv_paths', []), t)
        if self._pv_camera.isChecked() and tr[2] and tr[3]:
            fov, roll = cam_at(tr[0], t), cam_at(tr[1], t)
            h = fov[0] if fov else 70.0                  # game FOV is horizontal at 4:3
            v = math.degrees(2 * math.atan(math.tan(math.radians(h) / 2) * 0.75))
            vp.set_lookat(self._cs_cam_eye(t), cam_at(tr[3], t)[:3], roll[0] if roll else 0.0, v)
        else:
            vp.update()
        cam_end = getattr(self, '_cam_end', 0.0)
        if cam_end:
            self._cam_slider.blockSignals(True)
            self._cam_slider.setValue(int(1000 * min(t, cam_end) / cam_end))
            self._cam_slider.blockSignals(False)
            self._cam_update_view()

    def _cs_audio_find(self): #vers 2
        """Cutscene sound: audio/<name>.mp3 / .wav / .ogg, else SA audio/streams/CUTSCENE track."""
        base = os.path.dirname(self._cs_img.file_path) if self._cs_img is not None else self._cs_dir
        if not base or not self._cs_cur:
            return None
        want = self._cs_cur.lower()
        for root in (os.path.dirname(base), base):
            if not os.path.isdir(root):
                continue
            for d in os.listdir(root):
                full = os.path.join(root, d)
                if d.lower() == 'audio' and os.path.isdir(full):
                    for f in os.listdir(full):
                        stem, ext = os.path.splitext(f.lower())
                        if stem == want and ext in ('.mp3', '.wav', '.ogg'):
                            return os.path.join(full, f)
                    from apps.methods.sa_streams import cutscene_ogg
                    ogg = cutscene_ogg(full, want)          # SA: decrypted to a temp file
                    if ogg:
                        import tempfile
                        out = os.path.join(tempfile.gettempdir(), 'imgfactory_cutscene_audio')
                        os.makedirs(out, exist_ok=True)
                        path = os.path.join(out, want + '.ogg')
                        if not os.path.isfile(path) or os.path.getsize(path) != len(ogg):
                            with open(path, 'wb') as fh:
                                fh.write(ogg)
                        return path
        return None

    def _cs_audio_load(self): #vers 1
        """Load the cutscene's sound file into the player; note when none."""
        path = self._cs_audio_find()
        self._pv_audio_path = path
        if path is None:
            if self._pv_audio is not None:
                self._pv_audio.stop()
            return "no cutscene sound found"
        if self._pv_audio is None:
            from PyQt6.QtMultimedia import QAudioOutput, QMediaPlayer
            self._pv_audio = QMediaPlayer(self)
            self._pv_audio_out = QAudioOutput(self)
            self._pv_audio.setAudioOutput(self._pv_audio_out)
        from PyQt6.QtCore import QUrl
        self._pv_audio.stop()
        self._pv_audio.setSource(QUrl.fromLocalFile(path))
        return f"sound {os.path.basename(path)}"

    def _cs_cam_eye(self, t: float) -> list: #vers 2
        """Cutscene camera position, pulled back along the view by the zoom setting."""
        tr = self._cam_tracks
        e, g = cam_at(tr[2], t)[:3], cam_at(tr[3], t)[:3]
        if len(e) < 3 or len(g) < 3:
            return e
        zoom = 0 if self.WS.get('game_view', False) else int(self.WS.get('cam_zoom', -50))
        k = 100.0 / (100.0 + max(-90, min(90, zoom)))
        return [b + (a - b) * k for a, b in zip(e, g)]

    def _cs_orbit_from_cut(self): #vers 2
        """Free camera placed where the cutscene camera is now."""
        import math
        tr, t, vp = self._cam_tracks, self._pv_t, self._pv_vp
        e = self._cs_cam_eye(t) if tr[2] and tr[3] else []
        g = cam_at(tr[3], t)[:3] if tr[3] else []
        if vp is None or len(e) < 3 or len(g) < 3:
            self._cs_preview_centre()
            return
        vx, vy, vz = e[0] - g[0], e[1] - g[1], e[2]      # orbit pivot: target x / y at z 0
        d = math.sqrt(vx * vx + vy * vy + vz * vz) or 1.0
        pitch = max(1.0, min(89.0, math.degrees(math.acos(max(-1.0, min(1.0, vz / d))))))
        vp.set_camera_state(dist=d, pan_x=-g[0], pan_y=-g[1],
                            yaw=math.degrees(math.atan2(-vx, -vy)), pitch=pitch)

    def _cs_preview_free(self, obj, event): #vers 2
        """Viewport events: any mouse / arrow use unlocks the camera; Ctrl +/- zoom."""
        from PyQt6 import sip
        from PyQt6.QtCore import QEvent, Qt
        if sip.isdeleted(self._pv_camera):             # workshop closing
            return False
        et = event.type()
        if et == QEvent.Type.KeyPress and event.modifiers() & Qt.KeyboardModifier.ControlModifier \
                and event.key() in (Qt.Key.Key_Plus, Qt.Key.Key_Equal, Qt.Key.Key_Minus):
            self._cs_preview_zoom(0.85 if event.key() != Qt.Key.Key_Minus else 1.15)
            return True
        arrows = (Qt.Key.Key_Left, Qt.Key.Key_Right, Qt.Key.Key_Up, Qt.Key.Key_Down)
        if self._pv_camera.isChecked() and (et in (QEvent.Type.MouseButtonPress, QEvent.Type.Wheel)
                                            or (et == QEvent.Type.KeyPress and event.key() in arrows)):
            self._pv_camera.setChecked(False)
        return False

    def _cs_preview_zoom(self, factor: float): #vers 1
        """Free camera zoom (unlocks first)."""
        if self._pv_vp is None:
            return
        if self._pv_camera.isChecked():
            self._pv_camera.setChecked(False)
        vp = self._pv_vp
        vp._dist = max(0.1, min(50000.0, vp._dist * factor))
        vp.update()

    def _cs_preview_camera(self, on: bool): #vers 2
        """Locked: cutscene camera. Unlocked: free camera starting from that view."""
        if self._pv_vp is None:
            return
        if not on:
            self._pv_vp.set_lookat(None)
            self._cs_orbit_from_cut()
        self._cs_preview_draw()

    def _cs_preview_centre(self): #vers 3
        """Free camera: centre on the models at eye level."""
        import numpy as np
        if self._pv_vp is None:
            return
        if self._pv_camera.isChecked():
            self._pv_camera.setChecked(False)              # unlocks, then fits below
        pts = [p for g in self._pv_vp._all_geoms for p in g[0]]
        if not pts:
            return
        a = np.asarray(pts)
        lo, hi = a.min(0), a.max(0)
        c = (lo + hi) / 2
        self._pv_vp.set_camera_state(dist=max(4.0, float(np.linalg.norm(hi[:2] - lo[:2])) * 0.9),
                                     pan_x=-float(c[0]), pan_y=-float(c[1]), pitch=75.0)

    def _cs_preview_play(self, on: bool): #vers 4
        """Play / pause; the sound file, when found, is the clock."""
        import time
        if self._pv_scene is None:
            self._pv_play.setChecked(False)
            return
        self._pv_play.setToolTip("Pause" if on else "Play / pause the cutscene")
        self._pv_clock = time.monotonic()
        self._pv_frames, self._pv_fps_clock = 0, self._pv_clock
        if self._pv_audio_path and self._pv_audio is not None:
            if on:
                self._pv_audio.setPosition(int(self._pv_t * 1000))
                self._pv_audio.play()
            else:
                self._pv_audio.pause()
        (self._pv_timer.start if on else self._pv_timer.stop)()

    def _cs_preview_step(self, frames: int): #vers 1
        """Pause and move by whole frames (30 per second); sound follows."""
        if self._pv_scene is None:
            return
        if self._pv_play.isChecked():
            self._pv_play.setChecked(False)
            self._cs_preview_play(False)
        end = max(self._pv_scene.duration, getattr(self, '_cam_end', 0.0)) or 1.0
        self._pv_t = max(0.0, min(end, round(self._pv_t * 30) / 30 + frames / 30.0))
        self._pv_slider.blockSignals(True)
        self._pv_slider.setValue(int(1000 * self._pv_t / end))
        self._pv_slider.blockSignals(False)
        if self._pv_audio_path and self._pv_audio is not None:
            self._pv_audio.setPosition(int(self._pv_t * 1000))
        self._cs_preview_draw()

    def _cs_preview_tick(self): #vers 4
        """Advance by the sound position (or real time); stop at the end."""
        import time
        from PyQt6.QtMultimedia import QMediaPlayer
        now = time.monotonic()
        end = max(self._pv_scene.duration if self._pv_scene else 0.0, getattr(self, '_cam_end', 0.0)) or 1.0
        a = self._pv_audio
        if self._pv_audio_path and a is not None and a.playbackState() == QMediaPlayer.PlaybackState.PlayingState:
            self._pv_t = min(end, a.position() / 1000.0)
        else:
            self._pv_t = min(end, self._pv_t + (now - self._pv_clock))
        self._pv_clock = now
        self._pv_slider.blockSignals(True)
        self._pv_slider.setValue(int(1000 * self._pv_t / end))
        self._pv_slider.blockSignals(False)
        self._cs_preview_draw()
        self._pv_frames += 1                              # fps counter, once a second
        if now - self._pv_fps_clock >= 1.0:
            self._pv_fps.display(int(round(self._pv_frames / (now - self._pv_fps_clock))))
            self._pv_frames, self._pv_fps_clock = 0, now
        if self._pv_t >= end:
            self._pv_play.setChecked(False)
            self._cs_preview_play(False)

    def _cs_preview_set_time(self, v: int): #vers 2
        """Slider seek; the sound follows."""
        end = max(self._pv_scene.duration if self._pv_scene else 0.0, getattr(self, '_cam_end', 0.0)) or 1.0
        self._pv_t = end * v / 1000.0
        if self._pv_audio_path and self._pv_audio is not None:
            self._pv_audio.setPosition(int(self._pv_t * 1000))
        self._cs_preview_draw()

