#this belongs in apps/components/Cutscene_Workshop/depends/cutscene_shots_func.py - Version: 1
# X-Seti - November 06 2026 - IMG Factory 1.6 - Cutscene Workshop shot list

"""
Shots panel: the camera .dat as a list of shots (start, length, camera and
target). Pick a shot to jump there; change its length; add a held or
follow-an-actor shot; split at the preview time; delete; move up / down.
III / VC / SA (GTA IV camera is per frame in the .wad: view only).
Shot maths: apps/methods/cutscene_shots.py.
"""

##class CutsceneShotsMixin: -
# _create_shots_panel
# _shots_add
# _shots_apply
# _shots_delete
# _shots_editable
# _shots_fill
# _shots_len_edited
# _shots_move
# _shots_pick
# _shots_split

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QComboBox, QDialog, QDialogButtonBox, QDoubleSpinBox, QFormLayout, QHBoxLayout,
                             QHeaderView, QLabel, QMessageBox, QPushButton, QTableWidget, QTableWidgetItem,
                             QVBoxLayout, QWidget)

_COLS = ("#", "Start", "Length", "Camera", "Target")


class CutsceneShotsMixin: #vers 1
    """Shot list for Cutscene Workshop."""

    def _create_shots_panel(self): #vers 1
        """Shot table and its buttons."""
        w = QWidget()
        lay = QVBoxLayout(w)
        lay.setContentsMargins(2, 2, 2, 2)
        row = QHBoxLayout()
        for text, tip, fn in (("Add", "New shot after the selected one: hold or follow an actor", self._shots_add),
                              ("Split", "Cut the shot at the preview time", self._shots_split),
                              ("Delete", "Remove the selected shot", self._shots_delete),
                              ("Up", "Move the shot earlier", lambda: self._shots_move(-1)),
                              ("Down", "Move the shot later", lambda: self._shots_move(1))):
            b = QPushButton(text)
            b.setToolTip(tip)
            b.clicked.connect(fn)
            row.addWidget(b)
        lay.addLayout(row)
        self._shots_tbl = QTableWidget(0, len(_COLS))
        self._shots_tbl.setHorizontalHeaderLabels(_COLS)
        self._shots_tbl.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.ResizeToContents)
        self._shots_tbl.horizontalHeader().setStretchLastSection(True)
        self._shots_tbl.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self._shots_tbl.cellClicked.connect(self._shots_pick)
        self._shots_tbl.itemChanged.connect(self._shots_len_edited)
        lay.addWidget(self._shots_tbl, 1)
        self._shots_note = QLabel("Double-click a length to change it")
        self._shots_note.setWordWrap(True)
        lay.addWidget(self._shots_note)
        self._shots = []
        return w

    def _shots_fill(self): #vers 1
        """Rows from the current camera tracks."""
        from apps.methods.cutscene_shots import split_shots
        tracks = getattr(self, '_cam_tracks', None) or [[], [], [], []]
        self._shots = split_shots(tracks) if any(tracks) else []
        t = self._shots_tbl
        t.blockSignals(True)
        t.setRowCount(0)
        start = 0.0
        for n, sh in enumerate(self._shots, 1):
            cam = sh['keys'][2][0][1][:3] if sh['keys'][2] else []
            tgt = sh['keys'][3][0][1][:3] if sh['keys'][3] else []
            r = t.rowCount()
            t.insertRow(r)
            for c, v in enumerate((n, f"{start:.2f}", f"{sh['len']:.2f}",
                                   ", ".join(f"{x:.1f}" for x in cam), ", ".join(f"{x:.1f}" for x in tgt))):
                it = QTableWidgetItem(str(v))
                if c != 2 or getattr(self, '_cs_iv', False):
                    it.setFlags(it.flags() & ~Qt.ItemFlag.ItemIsEditable)
                t.setItem(r, c, it)
            start += sh['len']
        t.blockSignals(False)
        iv = getattr(self, '_cs_iv', False)
        self._shots_note.setText("GTA IV: shots shown, edit the camera keys instead" if iv
                                 else "Double-click a length to change it")

    def _shots_apply(self, shots, keep_row: int = -1): #vers 1
        """Shots back into the camera tracks; views and table refreshed."""
        from apps.methods.cutscene_shots import join_shots
        self._cam_tracks = join_shots(shots)
        self._cam_end = max((tr[-1][0] for tr in self._cam_tracks if tr), default=0.0)
        self._cs_dirty = True
        self._cam_show_track(self._cam_combo.currentIndex())
        self._shots_fill()
        if 0 <= keep_row < self._shots_tbl.rowCount():
            self._shots_tbl.selectRow(keep_row)
        self._cs_preview_draw()

    def _shots_editable(self) -> bool: #vers 1
        if getattr(self, '_cs_iv', False):
            self._set_status("GTA IV camera: edit keys on the Camera panel (per-frame .wad track)")
            return False
        return bool(self._shots)

    def _shots_pick(self, row: int, _col: int = 0): #vers 1
        """Jump the preview to the shot's start."""
        if not (0 <= row < len(self._shots)) or self._pv_scene is None:
            return
        start = sum(s['len'] for s in self._shots[:row]) + 0.01
        end = max(self._pv_scene.duration, getattr(self, '_cam_end', 0.0)) or 1.0
        self._pv_slider.setValue(int(1000 * min(start, end) / end))

    def _shots_len_edited(self, item): #vers 1
        """New length: the shot's keys stretch, later shots move."""
        if item.column() != 2 or not self._shots_editable():
            return
        r = item.row()
        try:
            new = float(item.text())
        except ValueError:
            new = 0.0
        if new <= 0.05:
            self._set_status("Length: a number of seconds above 0.05")
            self._shots_fill()
            return
        sh = self._shots[r]
        f = new / sh['len'] if sh['len'] > 0 else 1.0
        sh['keys'] = [[(rt * f, v) for rt, v in tr] for tr in sh['keys']]
        sh['len'] = new
        self._shots_apply(self._shots, r)

    def _shots_add(self): #vers 1
        """Add a shot after the selected one: hold its last frame, or follow an actor."""
        from apps.methods.cutscene_shots import follow_keys, shot_keys
        if not self._shots_editable():
            return
        r = self._shots_tbl.currentRow()
        r = r if 0 <= r < len(self._shots) else len(self._shots) - 1
        sc = self._pv_scene
        names = [it['name'] for it in sc.items] if sc is not None else []
        dlg = QDialog(self)
        dlg.setWindowTitle(f"New shot after shot {r + 1}")
        form = QFormLayout(dlg)
        length = QDoubleSpinBox()
        length.setRange(0.1, 120.0)
        length.setValue(3.0)
        length.setSuffix(" s")
        form.addRow("Length", length)
        kind = QComboBox()
        kind.addItem("Hold the end of this shot")
        kind.addItems([f"Follow {n}" for n in names])
        form.addRow("Camera", kind)
        boxes = {}
        for key, lo, hi, val, suf in (("dist", 0.5, 50.0, 4.0, " m"), ("height", -5.0, 20.0, 1.6, " m"),
                                      ("side", -180.0, 180.0, 0.0, " deg")):
            sp = QDoubleSpinBox()
            sp.setRange(lo, hi)
            sp.setValue(val)
            sp.setSuffix(suf)
            boxes[key] = sp
        form.addRow("Follow distance", boxes['dist'])
        form.addRow("Follow height", boxes['height'])
        form.addRow("Follow angle (0 = behind)", boxes['side'])
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept)
        bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        ln = length.value()
        last = self._shots[r]['keys']
        fov = last[0][-1][1][0] if last[0] else 70.0
        roll = last[1][-1][1][0] if last[1] else 0.0
        if kind.currentIndex() == 0:
            cam = last[2][-1][1][:3] if last[2] else (0.0, -5.0, 2.0)
            tgt = last[3][-1][1][:3] if last[3] else (0.0, 0.0, 1.0)
            new = shot_keys(ln, [(0.0, cam), (ln, cam)], [(0.0, tgt), (ln, tgt)], fov, roll)
        else:
            idx = kind.currentIndex() - 1
            start = sum(s['len'] for s in self._shots[:r + 1])
            cam, tgt = follow_keys(lambda t: sc.actor_at(idx, t), start, ln, boxes['dist'].value(),
                                   boxes['height'].value(), boxes['side'].value())
            new = shot_keys(ln, cam, tgt, fov, roll)
        self._shots_apply(self._shots[:r + 1] + [new] + self._shots[r + 1:], r + 1)

    def _shots_split(self): #vers 1
        """Cut the shot under the preview time into two."""
        if not self._shots_editable():
            return
        t, start = self._pv_t, 0.0
        for i, sh in enumerate(self._shots):
            if start < t < start + sh['len'] - 0.05:
                rel = t - start
                from apps.methods.cutscene_files import cam_at
                from apps.methods.cutscene_shots import GAP
                a = {'start': 0.0, 'len': rel, 'keys': [[], [], [], []]}
                b = {'start': 0.0, 'len': sh['len'] - rel, 'keys': [[], [], [], []]}
                for ti, tr in enumerate(sh['keys']):
                    mid = cam_at([(rt, v) for rt, v in tr], rel)
                    a['keys'][ti] = [(rt, v) for rt, v in tr if rt < rel] + ([(rel, mid)] if mid else [])
                    b['keys'][ti] = ([(0.0 if ti < 2 else GAP, mid)] if mid else []) + \
                        [(rt - rel, v) for rt, v in tr if rt > rel]
                self._shots_apply(self._shots[:i] + [a, b] + self._shots[i + 1:], i + 1)
                return
            start += sh['len']
        self._set_status("Move the preview inside a shot to split it")

    def _shots_delete(self): #vers 1
        r = self._shots_tbl.currentRow()
        if not self._shots_editable() or not (0 <= r < len(self._shots)) or len(self._shots) < 2:
            return
        if QMessageBox.question(self, self.App_name, f"Delete shot {r + 1}?") != QMessageBox.StandardButton.Yes:
            return
        self._shots_apply(self._shots[:r] + self._shots[r + 1:], max(0, r - 1))

    def _shots_move(self, d: int): #vers 1
        r = self._shots_tbl.currentRow()
        if not self._shots_editable() or not (0 <= r < len(self._shots)) or not (0 <= r + d < len(self._shots)):
            return
        sh = list(self._shots)
        sh[r], sh[r + d] = sh[r + d], sh[r]
        self._shots_apply(sh, r + d)
