#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_mission_func.py - Version: 5
# X-Seti - October 24 2026 - IMG Factory 1.6 - Retro GTA Workshop mission markers

"""
Missions combo (Files dock): GTA1 MISSION.INI missions / GTA2 .scr scripts
of the game folder; the picked one's objects are drawn on the map.
"""

##Methods list -
#
##class RetroMissionMixin: -
# _mission_apply
# _mission_export
# _mission_for_map
# _mission_import
# _mission_list_folder
# _mission_picked
# _mission_rows
# _mission_save
# _mission_source
# _mission_widgets

import os

from PyQt6.QtWidgets import QComboBox, QMessageBox

from apps.methods.retro_missions import gta1_line, gta1_missions, gta2_put, gta2_script_markers


class RetroMissionMixin: #vers 1
    """Mission / script markers of the Retro GTA Workshop."""

    def _mission_widgets(self, lay, style_combo): #vers 5
        """Style / Missions combos in one row; Import / Export / Save missions; icons when narrow."""
        from PyQt6.QtWidgets import QHBoxLayout, QToolButton
        from apps.methods.fold_widgets import ButtonRow, IconLabel
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as I
        self._missions = []                                 # [(label, ini mission or .scr path)]
        self._mission_combo = QComboBox()
        self._mission_combo.setToolTip("Mission objects (GTA1) / script placements (GTA2) shown on the map")
        self._mission_combo.activated.connect(lambda i: self._mission_picked(i))
        self._mission_ini = None                            # GTA1: [path, lines, newline, dirty]
        self._mission_scr = None                            # GTA2: [path, bytearray, dirty]
        ic = self._get_icon_color()
        row = QHBoxLayout()                              # one row; labels become icons when narrow
        for text, icon, combo in (("Style:", I.get_palette_icon, style_combo),
                                  ("Missions:", I.get_pin_icon, self._mission_combo)):
            combo.setSizeAdjustPolicy(QComboBox.SizeAdjustPolicy.AdjustToMinimumContentsLengthWithIcon)
            combo.setMinimumContentsLength(4)
            row.addWidget(IconLabel(text, icon(16, ic)))
            row.addWidget(combo, 1)
        lay.addLayout(row)
        btns = []
        for text, icon, tip, fn in (("Import", I.import_icon, "Copy a MISSION.INI / .scr into the game folder (backup first)",
                                     self._mission_import),
                                    ("Export", I.export_icon, "Save the picked mission file (MISSION.INI / .scr) elsewhere",
                                     self._mission_export),
                                    ("Save missions", I.save_icon,
                                     "Write moved mission objects back (MISSION.INI / .scr, backup first)",
                                     self._mission_save)):
            b = QToolButton()
            b.setText(text)
            b.setIcon(icon(16, ic))
            b.setToolTip(tip)
            b.clicked.connect(lambda _c=False, f=fn: f())
            btns.append(b)
        lay.addWidget(ButtonRow(btns))

    def _mission_list_folder(self, folder: str): #vers 2
        """GTA1: MISSION.INI sections; GTA2: .scr files."""
        self._missions = [("(none)", None)]
        self._mission_ini = self._mission_scr = None
        names = {n.lower(): n for n in os.listdir(folder)}
        if 'mission.ini' in names:
            path = os.path.join(folder, names['mission.ini'])
            with open(path, 'rb') as f:
                text = f.read().decode('latin-1')
            self._mission_ini = [path, text.splitlines(), '\r\n' if '\r\n' in text else '\n', False]
            for m in gta1_missions(text):
                self._missions.append((f"[{m['id']}] {m['name']} ({m['map']})", m))
        for low, n in sorted(names.items()):
            if low.endswith('.scr'):
                self._missions.append((n, os.path.join(folder, n)))
        self._mission_combo.clear()
        for label, _m in self._missions:
            self._mission_combo.addItem(label)

    def _mission_for_map(self, map_path: str): #vers 1
        """Pick the first mission / script made for this map."""
        stem = os.path.splitext(os.path.basename(map_path))[0].lower()
        for i, (_label, m) in enumerate(self._missions):
            if isinstance(m, dict) and m['map'].lower() == stem + '.cmp' or \
                    isinstance(m, str) and os.path.splitext(os.path.basename(m))[0].lower() == stem:
                self._mission_combo.setCurrentIndex(i)
                self._mission_picked(i)
                return
        self._mission_combo.setCurrentIndex(0)
        self._mission_picked(0)

    def _mission_picked(self, i: int): #vers 2
        if not 0 <= i < len(self._missions):
            return
        m = self._missions[i][1]
        if m is None:
            markers = []
        elif isinstance(m, dict):
            markers = m['markers']
        else:
            if self._mission_scr is not None and self._mission_scr[2] and self._mission_scr[0] != m and \
                    QMessageBox.question(self, self.App_name, "Discard moved script objects?") != \
                    QMessageBox.StandardButton.Yes:
                return
            try:
                with open(m, 'rb') as f:
                    data = bytearray(f.read())
                markers = gta2_script_markers(bytes(data))
                self._mission_scr = [m, data, False]
            except (OSError, ValueError) as ex:
                self._set_status(str(ex))
                markers = []
        self._mission_markers = markers
        self._view.set_markers(markers)
        if self._data_kind.currentText() == 'Mission objects':
            self._data_fill()
        self._set_status(f"{self._missions[i][0]}: {len(markers)} markers")

    def _mission_rows(self) -> list: #vers 1
        """Markers of the picked mission / script (edited in the Data tab)."""
        return list(getattr(self, '_mission_markers', []))

    def _mission_apply(self, rows: list): #vers 1
        """Moved positions (and GTA1 args / GTA2 area sizes) into the mission source."""
        marks = getattr(self, '_mission_markers', [])
        if len(rows) != len(marks):
            raise ValueError("Mission objects can be moved, not added or deleted")
        for m, r in zip(marks, rows):
            for k in ('x', 'y', 'z', 'w', 'h'):
                if k in r:
                    m[k] = r[k]
            if 'line' in m:
                m['args'] = r.get('args', m['args'])
                self._mission_ini[1][m['line']] = gta1_line(m)
                self._mission_ini[3] = True
            elif self._mission_scr is not None:
                gta2_put(self._mission_scr[1], m)
                self._mission_scr[2] = True
        self._view.update()

    def _mission_save(self): #vers 1
        """Write MISSION.INI / the .scr (backup first)."""
        from apps.methods.file_backup import safe_write_bytes
        done = []
        try:
            if self._mission_ini is not None and self._mission_ini[3]:
                path, lines, nl, _d = self._mission_ini
                safe_write_bytes(path, (nl.join(lines) + nl).encode('latin-1'), f"{self.App_name}: MISSION.INI")
                self._mission_ini[3] = False
                done.append(path)
            if self._mission_scr is not None and self._mission_scr[2]:
                safe_write_bytes(self._mission_scr[0], bytes(self._mission_scr[1]),
                                 f"{self.App_name}: {os.path.basename(self._mission_scr[0])}")
                self._mission_scr[2] = False
                done.append(self._mission_scr[0])
        except Exception as ex:
            QMessageBox.critical(self, "Save missions", str(ex))
            return
        self._set_status(f"Saved {', '.join(done)}" if done else "No mission changes to save")

    def _mission_source(self): #vers 1
        """(path, bytes) of the picked mission's file: the .scr, or MISSION.INI as edited."""
        m = self._missions[self._mission_combo.currentIndex()][1] if self._missions else None
        if isinstance(m, str) and self._mission_scr is not None and self._mission_scr[0] == m:
            return m, bytes(self._mission_scr[1])
        if isinstance(m, dict) and self._mission_ini is not None:
            path, lines, nl, _d = self._mission_ini
            return path, (nl.join(lines) + nl).encode('latin-1')
        return None, b''

    def _mission_export(self): #vers 1
        from PyQt6.QtWidgets import QFileDialog
        path, data = self._mission_source()
        if path is None:
            self._set_status("Pick a mission first")
            return
        out, _ = QFileDialog.getSaveFileName(self, "Export mission file", os.path.basename(path))
        if out:
            with open(out, 'wb') as f:
                f.write(data)
            self._set_status(f"Exported {out}")

    def _mission_import(self): #vers 1
        """Copy a mission file into the game data folder (backup of one it replaces), then relist."""
        from PyQt6.QtWidgets import QFileDialog
        from apps.methods.file_backup import safe_write_bytes
        if not self._data_dir:
            self._set_status("Open a game folder first")
            return
        src, _ = QFileDialog.getOpenFileName(self, "Import mission file", '', "Missions (MISSION.INI *.scr);;All files (*)")
        if not src:
            return
        dst = os.path.join(self._data_dir, os.path.basename(src))
        if os.path.exists(dst) and QMessageBox.question(
                self, self.App_name, f"Replace {os.path.basename(dst)} in the game folder?") != QMessageBox.StandardButton.Yes:
            return
        with open(src, 'rb') as f:
            data = f.read()
        safe_write_bytes(dst, data, f"{self.App_name}: {os.path.basename(dst)}")
        self._mission_list_folder(self._data_dir)
        self._set_status(f"Imported {dst}")
