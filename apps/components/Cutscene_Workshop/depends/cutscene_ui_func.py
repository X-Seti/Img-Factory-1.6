#this belongs in apps/components/Cutscene_Workshop/depends/cutscene_ui_func.py - Version: 13
# X-Seti - November 02 2026 - IMG Factory 1.6 - Cutscene Workshop UI

"""
Cutscene Workshop panels: cutscene list, script sections, script text,
camera tracks with the top-down path view, and the 3D preview (centre).
"""

##Methods list -
#
##class CamPathView: -
# __init__
# _to_screen
# _to_world
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# set_path
#
##class CutsceneUIMixin: -
# _create_left_panel
# _cs_apply_view
# _cs_camera_tab
# _cs_game_view
# _cs_preview_tab
# _cs_script_tab
# _cs_text_tab
# _cs_timecyc_path
# _cs_view_settings

from PyQt6.QtCore import QPointF, Qt, pyqtSignal
from PyQt6.QtGui import QColor, QPainter, QPen
from PyQt6.QtWidgets import (QComboBox, QHBoxLayout, QLabel, QLineEdit, QListWidget, QPlainTextEdit,
                             QSlider, QSplitter, QTableWidget, QVBoxLayout, QWidget)

from apps.methods.cutscene_files import CAM_TRACKS, CUT_COLUMNS


class CamPathView(QWidget): #vers 3
    """Top-down view (X / Y) of the camera and target paths; drag a key to move it."""
    key_moved = pyqtSignal(int, int, float, float, bool)       # track, key, x, y, finished

    def __init__(self, parent=None): #vers 3
        super().__init__(parent)
        self.cam, self.target, self.now = [], [], None
        self._drag = None
        self.trail, self.fov = ([], []), None
        self._view = (0.0, 0.0, 1.0)
        self.setMinimumSize(240, 200)

    def set_path(self, cam, target, now=None, trail=None, fov=None): #vers 4
        """cam / target: [[(x, y, key index)] per shot]; now: ((cx, cy), (tx, ty));
        trail: ([cam xy so far], [target xy so far]); fov: degrees for the view cone."""
        self.cam, self.target, self.now = cam, target, now
        self.trail, self.fov = trail or ([], []), fov
        if self._drag is None:                           # keep the scale steady while dragging
            pts = [q for shot in cam + target for q in shot]
            if pts:
                xs, ys = [q[0] for q in pts], [q[1] for q in pts]
                span = max(max(xs) - min(xs), max(ys) - min(ys), 1.0)
                self._view = ((max(xs) + min(xs)) / 2, (max(ys) + min(ys)) / 2, span)
        self.update()

    def _to_screen(self, x, y) -> QPointF: #vers 1
        cx, cy, span = self._view
        s = min(self.width() - 40, self.height() - 40) / span
        return QPointF(self.width() / 2 + (x - cx) * s, self.height() / 2 - (y - cy) * s)

    def _to_world(self, p) -> tuple: #vers 1
        cx, cy, span = self._view
        s = min(self.width() - 40, self.height() - 40) / span
        return cx + (p.x() - self.width() / 2) / s, cy - (p.y() - self.height() / 2) / s

    def paintEvent(self, _e): #vers 4
        """Faded paths, numbered shots, played trail, camera icon with view cone."""
        import math
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        pal = self.palette()
        p.fillRect(self.rect(), pal.base())
        if not any(self.cam + self.target):
            p.setPen(pal.text().color())
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No camera path")
            return
        blue, orange = QColor(70, 160, 255), QColor(255, 150, 60)
        for shots, col, tag in ((self.cam, blue, "C"), (self.target, orange, "T")):
            faded = QColor(col)
            faded.setAlpha(90)
            for n, path in enumerate(shots, 1):          # no line across a cut
                p.setPen(QPen(faded, 1.5))
                p.setBrush(faded)
                for a, b in zip(path, path[1:]):
                    p.drawLine(self._to_screen(a[0], a[1]), self._to_screen(b[0], b[1]))
                for q in path:
                    p.drawEllipse(self._to_screen(q[0], q[1]), 2.5, 2.5)
                if path:
                    p.setPen(col)
                    p.drawText(self._to_screen(path[0][0], path[0][1]) + QPointF(5, -5), f"{tag}{n}")
        for pts, col in zip(self.trail, (blue, orange)):  # played so far
            p.setPen(QPen(col, 2.5))
            for a, b in zip(pts, pts[1:]):
                if abs(a[0] - b[0]) + abs(a[1] - b[1]) < self._view[2] * 0.25:   # skip cuts
                    p.drawLine(self._to_screen(*a), self._to_screen(*b))
        if self.now:
            c, t = self._to_screen(*self.now[0]), self._to_screen(*self.now[1])
            ang = math.atan2(t.y() - c.y(), t.x() - c.x())
            dist = math.hypot(t.x() - c.x(), t.y() - c.y())
            dash = QPen(blue, 1, Qt.PenStyle.DotLine)
            p.setPen(dash)
            p.drawLine(c, t)
            if self.fov:                                 # horizontal view cone
                half = math.radians(self.fov) / 2
                for s_ in (-1, 1):
                    p.drawLine(c, c + QPointF(math.cos(ang + s_ * half), math.sin(ang + s_ * half)) * dist)
            p.setPen(QPen(orange, 2))
            p.setBrush(Qt.BrushStyle.NoBrush)
            p.drawEllipse(t, 5, 5)
            p.drawLine(t - QPointF(8, 0), t + QPointF(8, 0))
            p.drawLine(t - QPointF(0, 8), t + QPointF(0, 8))
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            p.save()
            p.translate(c)
            p.rotate(math.degrees(ang))
            SVGIconFactory.cam_marker_icon(24).paint(p, -12, -12, 24, 24)
            p.restore()
        p.setPen(pal.text().color())
        p.drawText(6, 14, "C camera, T target, bold = played; drag a key")

    def mousePressEvent(self, e): #vers 1
        """Pick the nearest key within 10 px."""
        pos, best = e.position(), None
        for track, shots in ((2, self.cam), (3, self.target)):
            for shot in shots:
                for q in shot:
                    d = (self._to_screen(q[0], q[1]) - pos).manhattanLength()
                    if d < 10 and (best is None or d < best[0]):
                        best = (d, track, q[2])
        self._drag = best[1:] if best else None

    def mouseMoveEvent(self, e): #vers 1
        if self._drag is not None and e.buttons() & Qt.MouseButton.LeftButton:
            x, y = self._to_world(e.position())
            self.key_moved.emit(self._drag[0], self._drag[1], x, y, False)

    def mouseReleaseEvent(self, e): #vers 1
        if self._drag is not None:
            x, y = self._to_world(e.position())
            track, key = self._drag
            self._drag = None
            self.key_moved.emit(track, key, x, y, True)


class CutsceneUIMixin: #vers 1
    """Panels for Cutscene Workshop."""

    def _create_left_panel(self): #vers 1
        """Cutscene list with filter and source label."""
        w = QWidget()
        lay = QVBoxLayout(w)
        lay.setContentsMargins(2, 2, 2, 2)
        self._cs_src_lbl = QLabel("Open anim/cuts.img, or a .cut file")
        self._cs_src_lbl.setWordWrap(True)
        lay.addWidget(self._cs_src_lbl)
        self._cs_filter = QLineEdit()
        self._cs_filter.setPlaceholderText("Filter cutscenes")
        self._cs_filter.textChanged.connect(self._cs_fill_list)
        lay.addWidget(self._cs_filter)
        self._cs_list = QListWidget()
        self._cs_list.currentTextChanged.connect(self._cs_select)
        lay.addWidget(self._cs_list, 1)
        return w

    def _cs_text_tab(self): #vers 1
        """Whole .cut as text."""
        self._cs_text = QPlainTextEdit()
        self._cs_text.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        return self._cs_text

    def _cs_script_tab(self): #vers 2
        """Section picker and an editable table of its rows (row buttons on the Script ribbon)."""
        w = QWidget()
        lay = QVBoxLayout(w)
        lay.setContentsMargins(2, 2, 2, 2)
        top = QHBoxLayout()
        top.addWidget(QLabel("Section:"))
        self._cs_sec_combo = QComboBox()
        self._cs_sec_combo.currentIndexChanged.connect(self._cs_show_section)
        top.addWidget(self._cs_sec_combo, 1)
        lay.addLayout(top)
        self._cs_table = QTableWidget(0, 1)
        self._cs_table.itemChanged.connect(self._cs_table_changed)
        lay.addWidget(self._cs_table, 1)
        return w

    def _cs_camera_tab(self): #vers 3
        """Track table over the path view, time slider (key buttons on the Camera ribbon)."""
        w = QWidget()
        lay = QVBoxLayout(w)
        lay.setContentsMargins(2, 2, 2, 2)
        top = QHBoxLayout()
        top.addWidget(QLabel("Track:"))
        self._cam_combo = QComboBox()
        self._cam_combo.addItems(CAM_TRACKS)
        self._cam_combo.currentIndexChanged.connect(self._cam_show_track)
        top.addWidget(self._cam_combo, 1)
        lay.addLayout(top)
        sp = QSplitter(Qt.Orientation.Vertical)          # narrow panel: table above, path below
        self._cam_table = QTableWidget(0, 1)
        self._cam_table.itemChanged.connect(self._cam_table_changed)
        sp.addWidget(self._cam_table)
        self._cam_view = CamPathView()
        self._cam_view.key_moved.connect(self._cam_drag_key)
        sp.addWidget(self._cam_view)
        sp.setSizes([260, 260])
        lay.addWidget(sp, 1)
        row = QHBoxLayout()
        self._cam_time_lbl = QLabel("0.00 s")
        self._cam_slider = QSlider(Qt.Orientation.Horizontal)
        self._cam_slider.setRange(0, 1000)
        self._cam_slider.valueChanged.connect(self._cam_time_changed)
        row.addWidget(self._cam_slider, 1)
        row.addWidget(self._cam_time_lbl)
        lay.addLayout(row)
        return w

    def _cs_preview_tab(self): #vers 4
        """3D playback: viewport made on first use; time slider and camera toggle (Play on the ribbon)."""
        from PyQt6.QtCore import QTimer
        from PyQt6.QtWidgets import QCheckBox
        w = QWidget()
        lay = QVBoxLayout(w)
        lay.setContentsMargins(0, 0, 0, 0)
        lay.setSpacing(2)
        self._pv_host = QVBoxLayout()
        lay.addLayout(self._pv_host, 1)
        row = QHBoxLayout()
        self._pv_slider = QSlider(Qt.Orientation.Horizontal)
        self._pv_slider.setRange(0, 1000)
        self._pv_slider.valueChanged.connect(self._cs_preview_set_time)
        row.addWidget(self._pv_slider, 1)
        self._pv_time = QLabel("0.00 s")
        row.addWidget(self._pv_time)
        self._pv_camera = QCheckBox("Lock camera")
        self._pv_camera.setToolTip("Locked: the cutscene camera. Unlocked: free camera from that view -\n"
                                   "arrows pan, wheel / Ctrl +/- zoom, right drag turn, middle drag pan")
        self._pv_camera.setChecked(True)
        self._pv_camera.toggled.connect(self._cs_preview_camera)
        row.addWidget(self._pv_camera)
        lay.addLayout(row)
        self._pv_notes = QLabel("")
        self._pv_notes.setWordWrap(True)
        lay.addWidget(self._pv_notes)
        self._pv_timer = QTimer(self)
        self._pv_timer.setInterval(16)                    # up to ~60 fps
        self._pv_timer.timeout.connect(self._cs_preview_tick)
        self._pv_vp, self._pv_scene, self._pv_for, self._pv_t = None, None, None, 0.0
        self._pv_audio, self._pv_audio_path = None, None
        return w

    def _cs_apply_view(self): #vers 3
        """Render mode, wire, grid, game view and timecyc from settings onto the preview."""
        vp = self._pv_vp
        if vp is None:
            return
        vp.set_render_mode(self.WS.get('render_mode', 'textured'))
        vp.set_wire_hint(bool(self.WS.get('wire', False)))
        vp.set_show_grid(bool(self.WS.get('show_grid', True)))
        vp.set_lookat_aspect(4 / 3 if self.WS.get('game_view', False) else None)
        on = bool(self.WS.get('timecyc', False))
        if on and not getattr(vp, '_timecyc_entries', None):
            vp.set_timecyc_path(self._cs_timecyc_path())
        vp.set_timecyc_hour(int(self.WS.get('hour', 12)))
        vp.set_timecyc_playing(on and bool(vp._timecyc_entries))

    def _cs_view_settings(self): #vers 4
        """View settings: render, wire, zoom, grid, map radius, model height, timecyc (saved)."""
        from PyQt6.QtWidgets import QCheckBox, QDialog, QDialogButtonBox, QFormLayout, QSpinBox
        dlg = QDialog(self)
        dlg.setWindowTitle("Cutscene view settings")
        form = QFormLayout(dlg)
        modes = [('Textured', 'textured'), ('Texture only', 'texture_only'), ('Semi solid', 'semi_solid'),
                 ('Solid', 'solid'), ('Wireframe', 'wireframe')]
        mode = QComboBox()
        for label, key in modes:
            mode.addItem(label, key)
        mode.setCurrentIndex(max(0, mode.findData(self.WS.get('render_mode', 'textured'))))
        wire = QCheckBox("Wire over the model")
        wire.setChecked(bool(self.WS.get('wire', False)))
        zoom = QSpinBox()
        zoom.setRange(-90, 90)
        zoom.setSuffix(" %")
        zoom.setValue(int(self.WS.get('cam_zoom', -50)))
        zoom.setToolTip("Locked camera: -50 % is twice as far back as the game camera")
        radius = QSpinBox()
        radius.setRange(20, 1000)
        radius.setSuffix(" m")
        radius.setValue(int(self.WS.get('map_radius', 150)))
        radius.setToolTip("Map objects loaded around the cutscene offset")
        from PyQt6.QtWidgets import QDoubleSpinBox
        lift = QDoubleSpinBox()
        lift.setRange(-5.0, 5.0)
        lift.setSingleStep(0.05)
        lift.setSuffix(" m")
        lift.setValue(float(self.WS.get('model_lift', 1.0)))
        lift.setToolTip("Raise the cutscene models (SA: 1 m puts them on the map ground)")
        tcyc = QCheckBox("Sky and light from the game's timecyc.dat")
        tcyc.setChecked(bool(self.WS.get('timecyc', False)))
        hour = QSpinBox()
        hour.setRange(0, 23)
        hour.setSuffix(":00")
        hour.setValue(int(self.WS.get('hour', 12)))
        grid = QCheckBox("Show grid")
        grid.setChecked(bool(self.WS.get('show_grid', True)))
        form.addRow("Render", mode)
        form.addRow("", wire)
        form.addRow("Camera zoom", zoom)
        form.addRow("", grid)
        form.addRow("Map radius", radius)
        form.addRow("Model height", lift)
        form.addRow("", tcyc)
        form.addRow("Hour", hour)

        def apply(*_):
            self.WS.set('render_mode', mode.currentData())
            self.WS.set('wire', wire.isChecked())
            self.WS.set('cam_zoom', zoom.value())
            self.WS.set('show_grid', grid.isChecked())
            map_changed = radius.value() != self.WS.get('map_radius', 150)
            self.WS.set('map_radius', radius.value())
            self.WS.set('model_lift', lift.value())
            self.WS.set('timecyc', tcyc.isChecked())
            self.WS.set('hour', hour.value())
            if self._pv_scene is not None:
                self._pv_scene.lift = lift.value()
            self.WS.save()
            self._cs_apply_view()
            self._cs_preview_draw()
            if map_changed and self.WS.get('show_map', False):
                self._cs_map_load()
        mode.currentIndexChanged.connect(apply)
        wire.toggled.connect(apply)
        zoom.valueChanged.connect(apply)
        grid.toggled.connect(apply)
        radius.editingFinished.connect(apply)
        lift.valueChanged.connect(apply)
        tcyc.toggled.connect(apply)
        hour.valueChanged.connect(apply)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Close)
        bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        dlg.exec()

    def _cs_game_view(self, on: bool): #vers 1
        """Game view: locked cutscene camera, 4:3 picture, no zoom (saved)."""
        self.WS.set('game_view', bool(on))
        self.WS.save()
        if on and not self._pv_camera.isChecked():
            self._pv_camera.setChecked(True)
        self._cs_apply_view()
        self._cs_preview_draw()

    def _cs_timecyc_path(self) -> str: #vers 1
        """data/timecyc.dat of the game around the cutscenes, or ''."""
        import os
        base = os.path.dirname(self._cs_img.file_path) if self._cs_img is not None else self._cs_dir
        root = os.path.dirname(base or '')
        for d in (os.listdir(root) if root and os.path.isdir(root) else []):
            if d.lower() == 'data':
                hit = next((f for f in os.listdir(os.path.join(root, d)) if f.lower() == 'timecyc.dat'), None)
                if hit:
                    return os.path.join(root, d, hit)
        return ''
