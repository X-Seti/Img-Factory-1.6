#this belongs in apps/components/Ped_Workshop/depends/ped_anim_func.py - Version: 10
# X-Seti - October 21 2026 - IMG Factory 1.6 - Ped Workshop animation

"""
Ped animation: IFP playback on skinned (VC / SA) and segmented (III) peds,
and stop-motion editing - pick a bone, rotate it, key it, step the frame.
"""

##Methods list -
# euler_from_matrix
# matrix_from_euler
# matrix_to_quat
#
##class KeySlider: -
# __init__
# paintEvent
# set_keys
#
##class PedAnimator: -
# __init__
# bone_track
# delete_key
# key_times
# local_matrix
# pose
# pose_at
# render
# set_key
#
##class PedAnimMixin: -
# _anim_bone_changed
# _anim_copy_pose
# _anim_delete_key
# _anim_drag_joint
# _anim_euler_changed
# _anim_jump_key
# _anim_key_all
# _anim_new
# _anim_open_ifp
# _anim_paste_pose
# _anim_pick_joint
# _anim_play
# _anim_refresh_bone_fields
# _anim_render
# _anim_ring_rotate
# _anim_save_ifp
# _anim_select
# _anim_set_key
# _anim_set_time
# _anim_step
# _anim_tick
# _anim_tool_button
# _anim_tree_pick
# _create_anim_dock
# _create_bones_dock
# _fill_ifp_sources
# _filter_anim_list
# _init_animator

import math

import numpy as np

from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtWidgets import (
    QCheckBox, QComboBox, QDockWidget, QDoubleSpinBox, QFileDialog, QGridLayout, QHBoxLayout, QInputDialog,
    QLabel, QLineEdit, QListWidget, QListWidgetItem, QMessageBox, QPushButton, QSlider,
    QSpinBox, QVBoxLayout, QWidget)

from apps.methods.dff_skin import quat_to_matrix, read_skeleton
from apps.methods.ifp_format import IFPAnim, IFPBone, IFPKey, load_ifp, save_ifp

FPS = 30.0


def matrix_to_quat(m) -> tuple: #vers 1
    """IFP quat (x, y, z, w) from a 3x3 row-vector rotation."""
    r = np.asarray(m, dtype=np.float64).T
    tr = r[0, 0] + r[1, 1] + r[2, 2]
    if tr > 0:
        s = math.sqrt(tr + 1.0) * 2
        q = ((r[2, 1] - r[1, 2]) / s, (r[0, 2] - r[2, 0]) / s, (r[1, 0] - r[0, 1]) / s, 0.25 * s)
    elif r[0, 0] > r[1, 1] and r[0, 0] > r[2, 2]:
        s = math.sqrt(1.0 + r[0, 0] - r[1, 1] - r[2, 2]) * 2
        q = (0.25 * s, (r[0, 1] + r[1, 0]) / s, (r[0, 2] + r[2, 0]) / s, (r[2, 1] - r[1, 2]) / s)
    elif r[1, 1] > r[2, 2]:
        s = math.sqrt(1.0 + r[1, 1] - r[0, 0] - r[2, 2]) * 2
        q = ((r[0, 1] + r[1, 0]) / s, 0.25 * s, (r[1, 2] + r[2, 1]) / s, (r[0, 2] - r[2, 0]) / s)
    else:
        s = math.sqrt(1.0 + r[2, 2] - r[0, 0] - r[1, 1]) * 2
        q = ((r[0, 2] + r[2, 0]) / s, (r[1, 2] + r[2, 1]) / s, 0.25 * s, (r[1, 0] - r[0, 1]) / s)
    n = math.sqrt(sum(v * v for v in q)) or 1.0
    return tuple(v / n for v in q)


def matrix_from_euler(rx, ry, rz) -> np.ndarray: #vers 1
    """3x3 row-vector rotation from X / Y / Z degrees (Z * Y * X order)."""
    a, b, c = (math.radians(v) for v in (rx, ry, rz))
    X = np.array([[1, 0, 0], [0, math.cos(a), -math.sin(a)], [0, math.sin(a), math.cos(a)]])
    Y = np.array([[math.cos(b), 0, math.sin(b)], [0, 1, 0], [-math.sin(b), 0, math.cos(b)]])
    Z = np.array([[math.cos(c), -math.sin(c), 0], [math.sin(c), math.cos(c), 0], [0, 0, 1]])
    return (Z @ Y @ X).T


def euler_from_matrix(m) -> tuple: #vers 1
    """X / Y / Z degrees of a 3x3 row-vector rotation."""
    r = np.asarray(m, dtype=np.float64).T
    b = math.asin(max(-1.0, min(1.0, -r[2, 0])))
    a = math.atan2(r[2, 1], r[2, 2])
    c = math.atan2(r[1, 0], r[0, 0])
    return tuple(math.degrees(v) for v in (a, b, c))


class KeySlider(QSlider): #vers 1
    """Time slider with diamond marks at the selected bone's keys."""

    def __init__(self): #vers 1
        super().__init__(Qt.Orientation.Horizontal)
        self._keys = []

    def set_keys(self, frames): #vers 1
        self._keys = list(frames)
        self.update()

    def paintEvent(self, event): #vers 1
        super().paintEvent(event)
        if not self._keys or self.maximum() <= self.minimum():
            return
        from PyQt6.QtGui import QPainter, QPolygon, QColor
        from PyQt6.QtCore import QPoint
        from PyQt6.QtWidgets import QStyle, QStyleOptionSlider
        opt = QStyleOptionSlider()
        self.initStyleOption(opt)
        groove = self.style().subControlRect(QStyle.ComplexControl.CC_Slider, opt,
                                             QStyle.SubControl.SC_SliderGroove, self)
        handle = self.style().subControlRect(QStyle.ComplexControl.CC_Slider, opt,
                                             QStyle.SubControl.SC_SliderHandle, self)
        x0, span = groove.left() + handle.width() // 2, groove.width() - handle.width()
        y = self.height() - 4
        p = QPainter(self)
        p.setPen(QColor(0, 0, 0, 160))
        p.setBrush(QColor('#ffc84a'))
        for f in self._keys:
            x = x0 + int(span * (f - self.minimum()) / (self.maximum() - self.minimum()))
            p.drawPolygon(QPolygon([QPoint(x, y - 4), QPoint(x + 3, y - 1), QPoint(x, y + 2), QPoint(x - 3, y - 1)]))
        p.end()


class PedAnimator: #vers 1
    """Skeleton + current IFP anim; poses and draws the ped."""

    def __init__(self, model, raw: bytes): #vers 1
        self.model = model
        self.sk = read_skeleton(raw)
        self.anim = None
        self.time = 0.0
        self.override = {}        # frame -> 4x4 local (unkeyed edit)
        self.anim_version = 'ANPK'
        self.in_place = True      # keep the top bone's XY at its bind spot
        self.sa = any(f['node_id'] >= 0 for f in self.sk.frames) and bool(self.sk.hierarchy)
        self.geo = {}
        for gi, g in enumerate(model.geometries):
            self.geo[gi] = (
                np.array([(v.x, v.y, v.z) for v in g.vertices], dtype=np.float64),
                np.array([(n.x, n.y, n.z) for n in g.normals], dtype=np.float64) if g.normals else None,
                [(u.u, u.v) for u in g.uv_layers[0]] if g.uv_layers else [],
                [(t.v1, t.v2, t.v3, t.material_id) for t in g.triangles],
                [(c.r, c.g, c.b, c.a) for c in g.colors] if g.colors else [])

    def bone_track(self, fi: int, create: bool = False): #vers 1
        """IFPBone of frame fi in the current anim (made on request)."""
        if self.anim is None:
            return None
        f = self.sk.frames[fi]
        b = None
        if f['node_id'] >= 0:
            b = next((x for x in self.anim.bones if x.bone_id == f['node_id']), None)
        if b is None and f['name']:
            b = self.anim.bone(f['name'].strip())
        if b is None and create:
            sa = self.anim_version == 'ANP3'
            b = IFPBone(f['name'].strip() or f"bone{fi}", f['node_id'] if sa else -1,
                        'KR00', sa)
            self.anim.bones.append(b)
        return b

    def local_matrix(self, fi: int) -> np.ndarray: #vers 1
        """Current local 4x4 of frame fi (edit, anim key or bind)."""
        if fi in self.override:
            return self.override[fi]
        f = self.sk.frames[fi]
        b = self.bone_track(fi)
        if b is None or not b.keys:
            return f['local']
        q, tr = b.sample(self.time)
        m = np.eye(4)
        m[:3, :3] = quat_to_matrix(q)
        m[3, :3] = tr if tr is not None else f['local'][3, :3]
        if self.in_place and tr is not None and f['parent'] == 0:
            m[3, :2] = f['local'][3, :2]
        return m

    def pose(self) -> list: #vers 1
        return self.sk.pose_ltms({i: self.local_matrix(i) for i in range(len(self.sk.frames))})

    def pose_at(self, t: float) -> list: #vers 1
        """Frame LTMs at another time (no unkeyed edits)."""
        keep, over = self.time, self.override
        self.time, self.override = t, {}
        try:
            return self.pose()
        finally:
            self.time, self.override = keep, over

    def render(self, vp, mesh=True, bones=True, selected=-1, pick_cb=None, drag_cb=None,
               ring_cb=None, ghost_times=()): #vers 2
        """Push the posed meshes, stick-man bones, rings and onion ghosts into the viewport."""
        ltms = self.pose()
        out = []
        space = np.eye(4)
        if self.sk.skins:
            space = np.linalg.inv(ltms[self.sk.atomics.get(next(iter(self.sk.skins)), 0)])
        if bones:
            joints = [tuple((L @ space)[3, :3]) for L in ltms]
            pairs = [(i, f['parent']) for i, f in enumerate(self.sk.frames) if f['parent'] > 0]
            axes = None
            if ring_cb is not None and 0 < selected < len(ltms):
                m = (ltms[selected] @ space)[:3, :3]
                axes = [tuple(r / (np.linalg.norm(r) or 1.0)) for r in m]
            ghosts = []
            for gt in ghost_times:
                g = self.pose_at(gt)
                gs = np.linalg.inv(g[self.sk.atomics.get(next(iter(self.sk.skins)), 0)]) if self.sk.skins else space
                ghosts.append([tuple((L @ gs)[3, :3]) for L in g])
            vp.set_skeleton(joints, pairs, selected, pick_cb, drag_cb, axes, ring_cb, ghosts)
        else:
            vp.set_skeleton(None)
        if not mesh:
            vp._vertices = []
            vp._all_geoms = []
            vp.update()
            return
        for gi, fi in sorted(self.sk.atomics.items()):
            V, N, uvs, tris, prelit = self.geo[gi]
            g = self.model.geometries[gi]
            if gi in self.sk.skins:
                P, PN = self.sk.skin_vertices(gi, V, N, ltms)
            else:
                M = ltms[fi]
                P = V @ M[:3, :3] + M[3, :3]
                PN = N @ M[:3, :3] if N is not None else None
            out.append((P.tolist(), PN.tolist() if PN is not None else [], uvs, tris,
                        g.materials, prelit, getattr(g, 'flags', 0)))
        vp._vertices = []
        vp._all_geoms = out
        vp.update()

    def key_times(self, fi: int) -> list: #vers 1
        b = self.bone_track(fi)
        return [k.time for k in b.keys] if b else []

    def set_key(self, fi: int, local: np.ndarray): #vers 1
        """Key frame fi's rotation (and root translation) at the current time."""
        b = self.bone_track(fi, create=True)
        q = matrix_to_quat(local[:3, :3])
        tr = tuple(local[3, :3]) if b.kind in ('KRT0', 'KRTS') else None
        key = next((k for k in b.keys if abs(k.time - self.time) < 1e-4), None)
        if key is None:
            b.keys.append(IFPKey(self.time, q, tr))
            b.keys.sort(key=lambda k: k.time)
        else:
            key.rot, key.trans = q, tr if tr is not None else key.trans
        self.override.pop(fi, None)

    def delete_key(self, fi: int) -> bool: #vers 1
        b = self.bone_track(fi)
        if b is None:
            return False
        n = len(b.keys)
        b.keys = [k for k in b.keys if abs(k.time - self.time) >= 1e-4]
        return len(b.keys) != n


class PedAnimMixin: #vers 1
    """Animation dock for PedWorkshop."""

    def _create_anim_dock(self): #vers 8
        """Anim info + filter, anim list, transport, time slider.
        The IFP combo and load / save / new go in the title bar (title_widgets)."""
        self._animator = None
        self._ifp = None
        self._ifp_dirty = False
        self._anim_timer = QTimer(self)
        self._anim_timer.setInterval(int(1000 / FPS))
        self._anim_timer.timeout.connect(self._anim_tick)
        tb = self._anim_tool_button
        self._ifp_source = QComboBox()
        self._ifp_source.setToolTip("IFP file: ped.ifp and the IFPs inside anim/anim.img (SA)")
        self._ifp_source.activated.connect(lambda _i: self._anim_open_ifp(self._ifp_source.currentData() or ''))
        self._ifp_label = QLabel("No IFP")
        self._anim_search = QLineEdit()
        self._anim_search.setPlaceholderText("Filter anims")
        self._anim_search.setMinimumWidth(60)
        self._anim_search.textChanged.connect(self._filter_anim_list)
        self._anim_title_widgets = [                       # IFP | name (anims) | filter | load save new
            self._ifp_source, self._ifp_label, self._anim_search,
            tb('open_icon', "Load an IFP file", lambda: self._anim_open_ifp('')),
            tb('save_icon', "Save the IFP (backup first)", self._anim_save_ifp),
            tb('add_icon', "New anim: copy of the selected one", self._anim_new)]
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(3)
        self._anim_list = QListWidget()
        self._anim_list.setMinimumHeight(60)
        self._anim_list.currentItemChanged.connect(lambda cur, _p: self._anim_select(cur))
        lay.addWidget(self._anim_list, 1)
        row = QHBoxLayout()
        row.setSpacing(2)
        row.addWidget(tb('key_prev_icon', "Previous key of this bone", lambda: self._anim_jump_key(-1)))
        row.addWidget(tb('frame_prev_icon', "Previous frame", lambda: self._anim_step(-1)))
        self._play_btn = tb('play_icon', "Play / pause", None, checkable=True)
        self._play_btn.toggled.connect(self._anim_play)
        row.addWidget(self._play_btn)
        row.addWidget(tb('frame_next_icon', "Next frame", lambda: self._anim_step(1)))
        row.addWidget(tb('key_next_icon', "Next key of this bone", lambda: self._anim_jump_key(1)))
        row.addSpacing(6)
        self._loop_chk = tb('loop_icon', "Loop", None, checkable=True, checked=True)
        self._inplace_chk = tb('in_place_icon', "In place: no root walk motion", None, checkable=True, checked=True)
        self._inplace_chk.toggled.connect(lambda on: (setattr(self._animator, 'in_place', on)
                                                      if self._animator else None, self._anim_render()))
        row.addWidget(self._loop_chk)
        row.addWidget(self._inplace_chk)
        row.addStretch(1)
        self._speed_spin = QDoubleSpinBox()
        self._speed_spin.setRange(0.05, 4.0)
        self._speed_spin.setSingleStep(0.1)
        self._speed_spin.setValue(1.0)
        self._speed_spin.setSuffix("x")
        self._speed_spin.setToolTip("Playback speed")
        row.addWidget(self._speed_spin)
        lay.addLayout(row)
        row = QHBoxLayout()
        self._time_slider = KeySlider()
        self._time_slider.setRange(0, 0)
        self._time_slider.valueChanged.connect(lambda v: self._anim_set_time(v / FPS))
        self._time_label = QLabel("0 / 0")
        self._time_label.setMinimumWidth(60)
        row.addWidget(self._time_slider, 1)
        row.addWidget(self._time_label)
        lay.addLayout(row)
        dock = QDockWidget("Animation", self)
        dock.setObjectName("Animation")
        dock.setWidget(panel)
        self._anim_dock = dock
        return dock

    def _create_bones_dock(self): #vers 2
        """Bone list, pose fields and key row; view toggles go in the title bar."""
        tb = self._anim_tool_button
        self._show_mesh_btn = tb('mesh_icon', "Show the mesh", None, checkable=True, checked=True)
        self._show_bones_btn = tb('bones_icon', "Show stick-man bones: click a joint to pick it,\n"
                                  "drag to rotate (X / Z, Shift+drag Y), or drag a ring", None,
                                  checkable=True, checked=True)
        self._onion_btn = tb('onion_skin_icon', "Onion skin: ghost poses before (blue) and after (red)",
                             None, checkable=True)
        self._onion_spin = QSpinBox()
        self._onion_spin.setRange(1, 30)
        self._onion_spin.setValue(3)
        self._onion_spin.setSuffix(" frames")
        self._onion_spin.setToolTip("Onion skin spacing in frames")
        self._onion_spin.valueChanged.connect(lambda _v: self._anim_render())
        for b in (self._show_mesh_btn, self._show_bones_btn, self._onion_btn):
            b.toggled.connect(lambda _on: self._anim_render())
        self._bones_title_widgets = [self._show_mesh_btn, self._show_bones_btn, self._onion_btn,
                                     self._onion_spin]
        panel = QWidget()
        bl = QVBoxLayout(panel)
        bl.setContentsMargins(4, 4, 4, 2)
        bl.setSpacing(2)
        self._bone_list = QListWidget()
        self._bone_list.setMinimumHeight(60)
        self._bone_list.currentRowChanged.connect(self._anim_bone_changed)
        bl.addWidget(self._bone_list, 1)
        grid = QGridLayout()
        grid.setHorizontalSpacing(3)
        grid.setVerticalSpacing(2)
        self._rot_spins, self._pos_spins = [], []
        for r, (label, spins, rng, dec, step) in enumerate((("Rot", self._rot_spins, 180.0, 1, 5.0),
                                                            ("Pos", self._pos_spins, 100.0, 3, 0.01))):
            lbl = QLabel(label)
            lbl.setFixedHeight(24)
            grid.addWidget(lbl, r, 0, Qt.AlignmentFlag.AlignVCenter)
            for i, axis in enumerate("XYZ"):
                sp = QDoubleSpinBox()
                sp.setRange(-rng, rng)
                sp.setDecimals(dec)
                sp.setSingleStep(step)
                sp.setPrefix(f"{axis} ")
                sp.setWrapping(label == "Rot")
                sp.valueChanged.connect(self._anim_euler_changed)
                grid.addWidget(sp, r, i + 1, Qt.AlignmentFlag.AlignVCenter)
                spins.append(sp)
        bl.addLayout(grid)
        row = QHBoxLayout()
        row.setSpacing(2)
        row.addWidget(tb('key_set_icon', "Set key: this bone's pose at this frame", self._anim_set_key))
        row.addWidget(tb('key_delete_icon', "Delete this bone's key at this frame", self._anim_delete_key))
        row.addWidget(tb('key_all_icon', "Key all bones: whole pose at this frame", self._anim_key_all))
        row.addWidget(tb('copy_icon', "Copy pose (all bones) at this frame", self._anim_copy_pose))
        row.addWidget(tb('paste_icon', "Paste pose onto this frame (keys every bone)", self._anim_paste_pose))
        row.addSpacing(6)
        self._auto_key_chk = tb('record_icon', "Auto key: key every change straight away", None, checkable=True)
        self._auto_step_chk = tb('frame_next_icon', "Stop motion: step forward after Set key", None, checkable=True)
        row.addWidget(self._auto_key_chk)
        row.addWidget(self._auto_step_chk)
        self._step_spin = QSpinBox()
        self._step_spin.setRange(1, 60)
        self._step_spin.setValue(5)
        self._step_spin.setSuffix(" frames")
        self._step_spin.setToolTip("Frames to step after a key")
        row.addWidget(self._step_spin)
        self._key_info = QLabel("")                        # key count, frames in its tooltip
        row.addWidget(self._key_info, 1)
        for i in range(row.count()):                       # one centre line for the whole row
            w = row.itemAt(i).widget()
            if w is not None:
                row.setAlignment(w, Qt.AlignmentFlag.AlignVCenter)
        bl.addLayout(row)
        dock = QDockWidget("Bones", self)
        dock.setObjectName("Bones")
        dock.setWidget(panel)
        self._bones_dock = dock
        return dock

    def _anim_tool_button(self, icon: str, tip: str, slot, checkable=False, checked=False): #vers 2
        """Small themed icon button."""
        from PyQt6.QtCore import QSize
        from PyQt6.QtWidgets import QToolButton
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        b = QToolButton()
        colour = self._get_icon_color() if hasattr(self, '_get_icon_color') else None
        b.setIcon(getattr(SVGIconFactory, icon)(16, colour))
        b.setIconSize(QSize(16, 16))
        b.setFixedSize(22, 22)
        b.setToolTip(tip)
        b.setCheckable(checkable)
        b.setChecked(checked)
        b.setAutoRaise(True)
        if slot is not None:
            b.clicked.connect(lambda _c=False: slot())
        return b

    def _init_animator(self, raw: bytes): #vers 2
        """New ped loaded: skeleton, bone list, current anim reapplied."""
        self._anim_timer.stop()
        self._play_btn.setChecked(False)
        model = getattr(self, '_current_dff_model', None)
        self._animator = None
        self._bone_list.clear()
        if model is None:
            return
        self._animator = PedAnimator(model, raw)
        self._animator.anim_version = self._ifp.version if self._ifp else 'ANPK'
        self._animator.in_place = self._inplace_chk.isChecked()
        for i, f in enumerate(self._animator.sk.frames):
            depth, p = 0, f['parent']
            while p >= 0 and depth < 40:
                depth, p = depth + 1, self._animator.sk.frames[p]['parent']
            self._bone_list.addItem(QListWidgetItem("  " * depth + (f['name'].strip() or f"frame {i}")))
        tree = getattr(self, '_frame_tree', None)
        if tree is not None and not getattr(self, '_frame_tree_synced', False):
            tree.currentItemChanged.connect(self._anim_tree_pick)
            self._frame_tree_synced = True
        cur = self._anim_list.currentItem()
        self._anim_select(cur)

    def _anim_render(self): #vers 3
        """Draw the ped with the Show option and bone picking."""
        an = self._animator
        if an is None:
            return
        ghosts = ()
        if self._onion_btn.isChecked() and an.anim is not None:
            step, n = self._onion_spin.value() / FPS, 2
            ghosts = [an.time - step * k for k in range(n, 0, -1)] + [an.time + step * k for k in range(1, n + 1)]
            ghosts = [g for g in ghosts if 0.0 <= g <= an.anim.duration()]
        an.render(self.preview_widget, mesh=self._show_mesh_btn.isChecked(),
                  bones=self._show_bones_btn.isChecked(), ring_cb=self._anim_ring_rotate,
                  ghost_times=ghosts,
                  selected=self._bone_list.currentRow(),
                  pick_cb=self._anim_pick_joint, drag_cb=self._anim_drag_joint)

    def _anim_pick_joint(self, fi, _mods): #vers 1
        """Joint clicked in the viewport: select that bone."""
        self._bone_list.setCurrentRow(fi)

    def _anim_drag_joint(self, fi, dx, dy, mods): #vers 1
        """Drag a joint: rotate its bone (X / Z, Shift for Y)."""
        rx, ry, rz = (sp.value() for sp in self._rot_spins)
        if mods & Qt.KeyboardModifier.ShiftModifier:
            ry += dx * 0.5
        else:
            rz += dx * 0.5
            rx += dy * 0.5
        wrap = lambda v: (v + 180.0) % 360.0 - 180.0
        for sp, v in zip(self._rot_spins, (wrap(rx), wrap(ry), wrap(rz))):
            sp.blockSignals(True)
            sp.setValue(v)
            sp.blockSignals(False)
        self._anim_euler_changed()

    def _fill_ifp_sources(self, paths: dict): #vers 1
        """IFP list for a game folder; opens ped.ifp."""
        self._ifp_source.clear()
        if paths.get('ifp'):
            self._ifp_source.addItem("ped.ifp", paths['ifp'])
        if paths.get('anim_img'):
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(paths['anim_img'])
            if arc.open():
                for e in sorted(arc.entries, key=lambda x: x.name.lower()):
                    if e.name.lower().endswith('.ifp'):
                        self._ifp_source.addItem(f"anim.img: {e.name}", f"{paths['anim_img']}:{e.name}")
        if self._ifp_source.count():
            self._anim_open_ifp(self._ifp_source.itemData(0))

    def _anim_ring_rotate(self, axis: int, degrees: float): #vers 1
        """Rotate ring dragged: turn the selected bone about its own axis."""
        an, fi = self._animator, self._bone_list.currentRow()
        if an is None or fi <= 0:
            return
        ang = [0.0, 0.0, 0.0]
        ang[axis] = degrees
        m = an.local_matrix(fi).copy()
        m[:3, :3] = matrix_from_euler(*ang) @ m[:3, :3]
        an.override[fi] = m
        if self._auto_key_chk.isChecked() and an.anim is not None:
            an.set_key(fi, m)
            self._ifp_dirty = True
        for sp, v in zip(self._rot_spins, euler_from_matrix(m[:3, :3])):
            sp.blockSignals(True)
            sp.setValue(v)
            sp.blockSignals(False)
        self._anim_render()

    def _anim_open_ifp(self, path: str): #vers 3
        if not path:
            path, _ = QFileDialog.getOpenFileName(self, "Open IFP", getattr(self, '_ped_root', '') or '',
                                                  "IFP animations (*.ifp);;All files (*)")
            if not path:
                return
        try:
            img_path, sep, entry = path.rpartition(':')
            if sep and img_path.lower().endswith('.img'):
                from apps.methods.img_core_classes import IMGFile
                from apps.methods.ifp_format import parse_ifp
                arc = IMGFile(img_path)
                arc.open()
                e = next(x for x in arc.entries if x.name.lower() == entry.lower())
                self._ifp = parse_ifp(arc.read_entry_data(e))
                self._ifp.path = path
            else:
                self._ifp = load_ifp(path)
        except Exception as e:
            QMessageBox.warning(self, "Open IFP", str(e))
            return
        if self._animator is not None:
            self._animator.anim_version = self._ifp.version
        self._ifp_dirty = False
        self._ifp_label.setText(f"{self._ifp.name} ({len(self._ifp.anims)} anims)")
        self._ifp_label.setToolTip(f"{self._ifp.name}: {self._ifp.version}, {len(self._ifp.anims)} anims\n"
                                   f"{self._ifp.path}")
        self._anim_list.clear()
        for a in self._ifp.anims:
            it = QListWidgetItem(a.name)
            it.setData(Qt.ItemDataRole.UserRole, a)
            self._anim_list.addItem(it)
        self._filter_anim_list(self._anim_search.text())

    def _filter_anim_list(self, text: str): #vers 1
        t = text.lower()
        for i in range(self._anim_list.count()):
            it = self._anim_list.item(i)
            it.setHidden(bool(t) and t not in it.text().lower())

    def _anim_select(self, item): #vers 1
        an = self._animator
        if an is None:
            return
        an.anim = item.data(Qt.ItemDataRole.UserRole) if item is not None else None
        an.override.clear()
        dur = an.anim.duration() if an.anim else 0.0
        self._time_slider.blockSignals(True)
        self._time_slider.setRange(0, int(round(dur * FPS)))
        self._time_slider.setValue(0)
        self._time_slider.blockSignals(False)
        self._anim_set_time(0.0)

    def _anim_set_time(self, t: float): #vers 1
        an = self._animator
        if an is None:
            return
        an.time = t
        an.override.clear()
        self._anim_render()
        self._time_label.setText(f"{int(round(t * FPS))} / {self._time_slider.maximum()}")
        self._anim_refresh_bone_fields()

    def _anim_play(self, on: bool): #vers 2
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        colour = self._get_icon_color() if hasattr(self, '_get_icon_color') else None
        self._play_btn.setIcon((SVGIconFactory.pause_icon if on else SVGIconFactory.play_icon)(20, colour))
        if on and self._animator is not None and self._animator.anim is not None:
            self._anim_timer.start()
        else:
            self._anim_timer.stop()

    def _anim_tick(self): #vers 1
        an = self._animator
        if an is None or an.anim is None:
            return
        dur = an.anim.duration()
        t = an.time + self._speed_spin.value() / FPS
        if t > dur:
            if not self._loop_chk.isChecked():
                self._play_btn.setChecked(False)
                return
            t = 0.0
        self._time_slider.blockSignals(True)
        self._time_slider.setValue(int(round(t * FPS)))
        self._time_slider.blockSignals(False)
        self._anim_set_time(t)

    def _anim_step(self, frames: int): #vers 1
        """Move the time by whole frames; grows the slider past the end."""
        v = max(0, self._time_slider.value() + frames)
        if v > self._time_slider.maximum():
            self._time_slider.setMaximum(v)
        self._time_slider.setValue(v)

    def _anim_bone_changed(self, row: int): #vers 2
        """Bone picked: pose fields, viewport highlight, Frame Hierarchy row."""
        self._anim_refresh_bone_fields()
        self._anim_render()
        tree = getattr(self, '_frame_tree', None)
        if tree is None:
            return
        from PyQt6.QtWidgets import QTreeWidgetItemIterator
        it = QTreeWidgetItemIterator(tree)
        while it.value():
            item = it.value()
            if item.data(0, 0x0100) == row:
                tree.blockSignals(True)
                tree.setCurrentItem(item)
                tree.blockSignals(False)
                break
            it += 1

    def _anim_tree_pick(self, item, _prev): #vers 1
        """Frame Hierarchy click selects the same bone."""
        if item is not None and self._animator is not None:
            fi = item.data(0, 0x0100)
            if isinstance(fi, int) and 0 <= fi < self._bone_list.count():
                self._bone_list.setCurrentRow(fi)

    def _anim_refresh_bone_fields(self): #vers 3
        """Show the selected bone's current local rotation / position."""
        an, fi = self._animator, self._bone_list.currentRow()
        if an is None or fi < 0:
            return
        m = an.local_matrix(fi)
        for sp, v in zip(self._rot_spins + self._pos_spins, euler_from_matrix(m[:3, :3]) + tuple(m[3, :3])):
            sp.blockSignals(True)
            sp.setValue(v)
            sp.blockSignals(False)
        times = an.key_times(fi)
        self._key_info.setText(f"{len(times)} keys")
        self._key_info.setToolTip("Key frames: " + ", ".join(str(int(round(t * FPS))) for t in times))
        self._time_slider.set_keys([int(round(t * FPS)) for t in times])

    def _anim_euler_changed(self, *_a): #vers 2
        """Spin boxes moved: pose the selected bone (keyed if Auto key)."""
        an, fi = self._animator, self._bone_list.currentRow()
        if an is None or fi < 0:
            return
        m = np.eye(4)
        m[:3, :3] = matrix_from_euler(*(sp.value() for sp in self._rot_spins))
        m[3, :3] = [sp.value() for sp in self._pos_spins]
        an.override[fi] = m
        if self._auto_key_chk.isChecked() and an.anim is not None:
            an.set_key(fi, m)
            self._ifp_dirty = True
        self._anim_render()

    def _anim_set_key(self): #vers 2
        an, fi = self._animator, self._bone_list.currentRow()
        if an is None or fi < 0 or an.anim is None:
            self._set_status("Animation: select an anim and a bone first")
            return
        an.set_key(fi, an.local_matrix(fi))
        self._ifp_dirty = True
        dur_frames = int(round(an.anim.duration() * FPS))
        if dur_frames > self._time_slider.maximum():
            self._time_slider.setMaximum(dur_frames)
        self._anim_refresh_bone_fields()
        if self._auto_step_chk.isChecked():
            self._anim_step(self._step_spin.value())

    def _anim_key_all(self): #vers 1
        """Key every bone's current pose at this frame."""
        an = self._animator
        if an is None or an.anim is None:
            self._set_status("Animation: select an anim first")
            return
        for fi in range(1, len(an.sk.frames)):
            an.set_key(fi, an.local_matrix(fi))
        self._ifp_dirty = True
        self._anim_refresh_bone_fields()
        if self._auto_step_chk.isChecked():
            self._anim_step(self._step_spin.value())

    def _anim_copy_pose(self): #vers 1
        """Copy all bone locals at this frame."""
        an = self._animator
        if an is not None:
            self._pose_clip = {fi: an.local_matrix(fi).copy() for fi in range(1, len(an.sk.frames))}
            self._set_status(f"Pose copied ({len(self._pose_clip)} bones)")

    def _anim_paste_pose(self): #vers 1
        """Paste the copied pose here and key every bone."""
        an, clip = self._animator, getattr(self, '_pose_clip', None)
        if an is None or an.anim is None or not clip:
            self._set_status("Animation: copy a pose first")
            return
        for fi, m in clip.items():
            if fi < len(an.sk.frames):
                an.set_key(fi, m)
        self._ifp_dirty = True
        self._anim_render()
        self._anim_refresh_bone_fields()

    def _anim_delete_key(self): #vers 2
        an, fi = self._animator, self._bone_list.currentRow()
        if an is not None and fi >= 0 and an.delete_key(fi):
            self._ifp_dirty = True
            an.override.clear()
            self._anim_render()
            self._anim_refresh_bone_fields()

    def _anim_jump_key(self, direction: int): #vers 1
        an, fi = self._animator, self._bone_list.currentRow()
        if an is None or fi < 0:
            return
        now = round(an.time * FPS)
        frames = sorted({int(round(t * FPS)) for t in an.key_times(fi)})
        hits = [f for f in frames if (f > now if direction > 0 else f < now)]
        if hits:
            self._time_slider.setValue(hits[0] if direction > 0 else hits[-1])

    def _anim_new(self): #vers 2
        """New anim: copy of the selected one, or empty (bind pose)."""
        if self._ifp is None:
            QMessageBox.information(self, "New anim", "Open an IFP first.")
            return
        name, ok = QInputDialog.getText(self, "New anim", "Name:")
        if not ok or not name.strip():
            return
        cur = self._anim_list.currentItem()
        src = cur.data(Qt.ItemDataRole.UserRole) if cur is not None else None
        anim = IFPAnim(name.strip())
        if src is not None:
            anim.bones = [IFPBone(b.name, b.bone_id, b.kind, b.compressed, [k.copy() for k in b.keys],
                                  b.name_raw, b.extra) for b in src.bones]
            anim.extra = src.extra
        self._ifp.anims.append(anim)
        self._ifp_dirty = True
        it = QListWidgetItem(anim.name)
        it.setData(Qt.ItemDataRole.UserRole, anim)
        self._anim_list.addItem(it)
        self._anim_list.setCurrentItem(it)

    def _anim_save_ifp(self): #vers 3
        if self._ifp is None:
            return
        path = self._ifp.path
        if not path:
            path, _ = QFileDialog.getSaveFileName(self, "Save IFP", '', "IFP animations (*.ifp)")
            if not path:
                return
        try:
            img_path, sep, _entry = path.rpartition(':')
            if sep and img_path.lower().endswith('.img'):
                from apps.methods.ifp_format import build_ifp
                from apps.methods.path_nodes import write_back
                data = build_ifp(self._ifp)
                write_back(path, lambda _old: data)            # IMG entry, backup first
            else:
                save_ifp(self._ifp, path)
            self._ifp_dirty = False
            self._set_status(f"Saved {path}")
        except Exception as e:
            QMessageBox.critical(self, "Save IFP", str(e))
