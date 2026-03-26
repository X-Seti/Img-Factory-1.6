#this belongs in apps/components/Col_Editor/col_mesh_editor.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - COL Mesh Editor Dialog

"""
COL Mesh Editor — edit faces and vertices of a COL model.

Features:
  - Face list: select, delete, add faces
  - Vertex table: move vertices by editing X/Y/Z values
  - Mini viewport showing selected face highlighted
  - Full undo (each operation pushes state onto workshop undo stack)
  - Surface material picker per face

Layout:
  ┌─────────────────────────────────────────────────┐
  │  [Face List]    │  [Vertex Table]  │  [Preview]  │
  │  id  a  b  c  mat│  id  X   Y   Z  │             │
  │  ...            │  ...             │  (mini view)│
  ├─────────────────────────────────────────────────┤
  │  [Add Face] [Del Face] [Del Vertex] [Move Vert] │
  │  [Undo]  [Apply & Close]  [Close]               │
  └─────────────────────────────────────────────────┘
"""

import copy
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QSplitter,
    QTableWidget, QTableWidgetItem, QHeaderView,
    QPushButton, QLabel, QWidget, QGroupBox,
    QComboBox, QDoubleSpinBox, QMessageBox,
    QAbstractItemView, QFrame, QSpinBox, QTabWidget
)
from PyQt6.QtCore import Qt, QSize, QTimer
from PyQt6.QtGui import (
    QPainter, QColor, QPen, QBrush, QPixmap, QFont
)

# GTA surface material names (material byte 0–70 used)
SURFACE_MATERIALS = {
    0:  "Default",       1:  "Tarmac",        2:  "Gravel",
    3:  "Mud",           4:  "Pavement",       5:  "Grass",
    6:  "Sand",          7:  "Water",          8:  "Stone",
    9:  "Wood",          10: "Metal",          11: "Glass",
    12: "Dirt",          13: "Concrete",       14: "Hedge",
    15: "Leaves",        16: "Carpet",         17: "Fabric",
    18: "Rubber",        19: "Plastic",        20: "Cardboard",
    21: "Paper",         22: "Marble",         23: "Wood Floor",
    24: "Interior Floor",25: "Sky",            26: "Explosion",
    27: "Sand Beach",    28: "Steep Slope",    29: "Interior",
    30: "Puddle",        31: "Snow",           32: "Ice",
    33: "Wet",           34: "Flood Water",    35: "Office Carpet",
    70: "Unknown",
}

MATERIAL_COLORS = {
    0:  QColor(200, 200, 200),   # Default — grey
    1:  QColor(60,  60,  60),    # Tarmac — dark grey
    2:  QColor(140, 120, 80),    # Gravel — brownish
    3:  QColor(100, 80,  50),    # Mud — brown
    4:  QColor(180, 170, 150),   # Pavement
    5:  QColor(60,  150, 60),    # Grass — green
    6:  QColor(220, 200, 140),   # Sand — yellow
    7:  QColor(50,  100, 220),   # Water — blue
    8:  QColor(160, 160, 160),   # Stone
    9:  QColor(140, 100, 60),    # Wood — brown
    10: QColor(180, 180, 200),   # Metal
    11: QColor(180, 220, 240),   # Glass — light blue
    12: QColor(100, 80,  60),    # Dirt
    13: QColor(150, 150, 150),   # Concrete
}


##class COLMeshEditorViewport -
class COLMeshEditorViewport(QWidget): #vers 1
    """Mini 2D viewport for the mesh editor — shows faces with selection highlight."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumSize(180, 180)
        self._model        = None
        self._sel_faces    = set()   # selected face indices
        self._sel_verts    = set()   # selected vertex indices
        self._yaw          = 30.0
        self._pitch        = 20.0
        self._zoom         = 1.0
        self._pan_x        = 0.0
        self._pan_y        = 0.0
        self._drag         = None
        self.setStyleSheet("background-color: #14141e;")
        self.setCursor(Qt.CursorShape.OpenHandCursor)

    def set_model(self, model):
        self._model = model
        self._sel_faces.clear()
        self._sel_verts.clear()
        self.update()

    def set_selected_faces(self, indices):
        self._sel_faces = set(indices)
        self.update()

    def set_selected_verts(self, indices):
        self._sel_verts = set(indices)
        self.update()

    def mousePressEvent(self, event):
        self._drag = event.position()
        self.setCursor(Qt.CursorShape.ClosedHandCursor)

    def mouseMoveEvent(self, event):
        if self._drag:
            d = event.position() - self._drag
            if event.buttons() & Qt.MouseButton.RightButton:
                self._yaw   = (self._yaw + d.x() * 0.5) % 360
                self._pitch = max(-89, min(89, self._pitch + d.y() * 0.5))
            else:
                self._pan_x += d.x()
                self._pan_y += d.y()
            self._drag = event.position()
            self.update()

    def mouseReleaseEvent(self, event):
        self._drag = None
        self.setCursor(Qt.CursorShape.OpenHandCursor)

    def wheelEvent(self, event):
        f = 1.15 if event.angleDelta().y() > 0 else 1/1.15
        self._zoom = max(0.05, min(20.0, self._zoom * f))
        self.update()

    def paintEvent(self, event):
        import math
        from PyQt6.QtGui import QPolygonF
        from PyQt6.QtCore import QPointF
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        W, H = self.width(), self.height()
        p.fillRect(self.rect(), QColor(20, 20, 30))

        if not self._model:
            p.setPen(QColor(100, 100, 100))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No model")
            return

        verts = getattr(self._model, 'vertices', [])
        faces = getattr(self._model, 'faces', [])

        # ── projection helpers ────────────────────────────────────────────
        yr = math.radians(self._yaw);   cy, sy = math.cos(yr), math.sin(yr)
        pr = math.radians(self._pitch); cp, sp = math.cos(pr), math.sin(pr)

        def proj3(x, y, z):
            rx = x*cy - y*sy;  ry = x*sy + y*cy
            ry2 = ry*cp - z*sp
            return rx, ry2

        # Compute scale/origin from vertex bounds
        if verts:
            pts2d = [proj3(v.x, v.y, v.z) for v in verts]
            xs2 = [q[0] for q in pts2d]; ys2 = [q[1] for q in pts2d]
            mn_x, mx_x = min(xs2), max(xs2)
            mn_y, mx_y = min(ys2), max(ys2)
            rng = max(mx_x-mn_x, mx_y-mn_y, 0.001)
            pad = 20
            scale = ((min(W, H) - pad*2) / rng) * self._zoom
            cx2 = (mn_x+mx_x)/2;  cy3 = (mn_y+mx_y)/2
            ox = W/2 - cx2*scale + self._pan_x
            oy = H/2 - cy3*scale + self._pan_y

            def scx(i): return pts2d[i][0]*scale + ox
            def scy(i): return pts2d[i][1]*scale + oy

            extent = max(max(abs(v.x) for v in verts),
                         max(abs(v.y) for v in verts),
                         max(abs(v.z) for v in verts), 1.0)
        else:
            scale = 20 * self._zoom
            ox = W/2 + self._pan_x; oy = H/2 + self._pan_y
            extent = 5.0

        def to_screen(x, y, z):
            px, py = proj3(x, y, z)
            return px*scale + ox, py*scale + oy

        # ── reference grid ────────────────────────────────────────────────
        raw_step = extent / 4.0
        mag  = 10 ** math.floor(math.log10(max(raw_step, 0.001)))
        step = round(raw_step / mag) * mag;  step = max(step, 0.01)
        half = math.ceil(extent / step) * step
        n    = int(half / step)

        p.setRenderHint(p.renderHints().__class__.Antialiasing, False)
        for i in range(-n, n+1):
            v = i * step
            col = QColor(70, 75, 95) if i != 0 else QColor(90, 95, 120)
            p.setPen(QPen(col, 1))
            x0,y0 = to_screen(-half, v, 0);  x1,y1 = to_screen(half, v, 0)
            p.drawLine(int(x0), int(y0), int(x1), int(y1))
            x0,y0 = to_screen(v, -half, 0);  x1,y1 = to_screen(v, half, 0)
            p.drawLine(int(x0), int(y0), int(x1), int(y1))
        p.setRenderHint(p.renderHints().__class__.Antialiasing, True)

        # ── faces ─────────────────────────────────────────────────────────
        if not verts or not faces:
            p.setPen(QColor(100, 100, 100))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No mesh")
        else:
            for fi, face in enumerate(faces):
                a, b, c = face.a, face.b, face.c
                if a >= len(verts) or b >= len(verts) or c >= len(verts): continue
                mat_col = MATERIAL_COLORS.get(face.material, QColor(120, 120, 120))
                selected = fi in self._sel_faces
                if selected:
                    fill = QColor(255, 200, 50, 160); pen_col = QColor(255, 220, 0); pen_w = 2
                else:
                    fill = QColor(mat_col.red(), mat_col.green(), mat_col.blue(), 80)
                    pen_col = QColor(mat_col.red()//2+60, mat_col.green()//2+60, mat_col.blue()//2+60)
                    pen_w = 1
                poly = QPolygonF([QPointF(scx(a), scy(a)), QPointF(scx(b), scy(b)), QPointF(scx(c), scy(c))])
                p.setBrush(QBrush(fill)); p.setPen(QPen(pen_col, pen_w))
                p.drawPolygon(poly)

            # Selected vertices
            p.setPen(QPen(QColor(255, 80, 80), 1)); p.setBrush(QBrush(QColor(255, 80, 80)))
            for vi in self._sel_verts:
                if vi < len(verts):
                    p.drawEllipse(int(scx(vi))-4, int(scy(vi))-4, 8, 8)

        # ── XYZ gizmo (bottom-left) ───────────────────────────────────────
        gx, gy, arm = 42, H-42, 30
        axes = [((1,0,0), QColor(220,60,60), 'X'),
                ((0,1,0), QColor(60,200,60), 'Y'),
                ((0,0,1), QColor(60,120,220), 'Z')]
        for (dx,dy,dz), col, lbl in sorted(axes, key=lambda a: proj3(*a[0])[1], reverse=True):
            px,py = proj3(dx,dy,dz)
            tx, ty = int(gx+px*arm), int(gy+py*arm)
            p.setPen(QPen(col, 2)); p.drawLine(gx, gy, tx, ty)
            ang = math.atan2(ty-gy, tx-gx)
            aw, ah = 8, 5
            tip  = QPointF(tx, ty)
            lpt  = QPointF(tx-aw*math.cos(ang)+ah*math.sin(ang), ty-aw*math.sin(ang)-ah*math.cos(ang))
            rpt  = QPointF(tx-aw*math.cos(ang)-ah*math.sin(ang), ty-aw*math.sin(ang)+ah*math.cos(ang))
            p.setBrush(QBrush(col)); p.setPen(QPen(col,1))
            p.drawPolygon(QPolygonF([tip, lpt, rpt]))
            p.setFont(QFont('Arial', 7, QFont.Weight.Bold)); p.setPen(col)
            p.drawText(tx+(7 if tx>=gx else -12), ty+(5 if ty>=gy else -2), lbl)
        p.setBrush(QBrush(QColor(220,220,220))); p.setPen(QPen(QColor(180,180,180),1))
        p.drawEllipse(gx-3, gy-3, 6, 6)

        # ── HUD ───────────────────────────────────────────────────────────
        p.setPen(QColor(180, 180, 180))
        p.setFont(QFont('Arial', 7))
        p.drawText(4, 12, f"F:{len(faces)} V:{len(verts)}")
        p.drawText(4, H-4, f"Y:{self._yaw:.0f}° P:{self._pitch:.0f}°")


##class COLMeshEditor -
class COLMeshEditor(QDialog): #vers 1
    """Dialog for editing COL mesh faces and vertices with undo support."""

    def __init__(self, workshop, model_index, parent=None):
        super().__init__(parent)
        self.workshop    = workshop
        self.model_index = model_index
        # Deep-copy so edits don't affect original until Apply
        self._model = copy.deepcopy(
            workshop.current_col_file.models[model_index])
        self._undo_stack = []   # local undo stack (list of deep-copied models)
        self._dirty = False

        self.setWindowTitle(f"Mesh Editor — {getattr(self._model.header, 'name', 'Model')}")
        self.setMinimumSize(900, 560)
        self.resize(1000, 620)
        self._build_ui()
        self._populate_all()

    # ── UI construction ───────────────────────────────────────────────────

    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setSpacing(4)

        # ── Main splitter: tabs left, viewport right ──────────────────────
        splitter = QSplitter(Qt.Orientation.Horizontal)

        # ── Tab widget ────────────────────────────────────────────────────
        self.tabs = QTabWidget()
        self.tabs.setMinimumWidth(420)

        # Tab 1: Mesh (faces + vertices)
        self.tabs.addTab(self._build_mesh_tab(),    "Mesh")
        # Tab 2: Boxes
        self.tabs.addTab(self._build_boxes_tab(),   "Boxes")
        # Tab 3: Spheres
        self.tabs.addTab(self._build_spheres_tab(), "Spheres")
        # Tab 4: Bounds
        self.tabs.addTab(self._build_bounds_tab(),  "Bounds")

        splitter.addWidget(self.tabs)

        # ── Viewport ──────────────────────────────────────────────────────
        vp_grp = QGroupBox("Preview")
        vpl = QVBoxLayout(vp_grp)
        self.viewport = COLMeshEditorViewport()
        vpl.addWidget(self.viewport)
        vpl.addWidget(QLabel("Left: pan  Right: rotate  Scroll: zoom",
                             alignment=Qt.AlignmentFlag.AlignCenter))
        splitter.addWidget(vp_grp)
        splitter.setSizes([480, 260])
        root.addWidget(splitter, 1)

        # ── Status ────────────────────────────────────────────────────────
        self._status = QLabel("Ready")
        self._status.setStyleSheet("color:#aaa;font-size:10px;")
        root.addWidget(self._status)

        # ── Bottom buttons ────────────────────────────────────────────────
        bot = QHBoxLayout()
        self._undo_btn = self._btn(bot, "↩ Undo", self._undo)
        self._undo_btn.setEnabled(False)
        bot.addStretch()
        self._btn(bot, "Apply & Close", self._apply_and_close)
        self._btn(bot, "Close",         self.reject)
        root.addLayout(bot)

    def _build_mesh_tab(self):
        """Faces + vertices sub-panel."""
        w = QWidget(); lay = QVBoxLayout(w); lay.setSpacing(4)
        inner = QSplitter(Qt.Orientation.Horizontal)

        # Face table
        face_grp = QGroupBox("Faces")
        fl = QVBoxLayout(face_grp)
        self.face_table = QTableWidget(0, 5)
        self.face_table.setHorizontalHeaderLabels(["#","A","B","C","Material"])
        self.face_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self.face_table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.face_table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self.face_table.setEditTriggers(QAbstractItemView.EditTrigger.DoubleClicked)
        self.face_table.itemSelectionChanged.connect(self._on_face_selection)
        self.face_table.itemChanged.connect(self._on_face_cell_changed)
        fl.addWidget(self.face_table)
        fb = QHBoxLayout()
        self._btn(fb, "Add Face",    self._add_face)
        self._btn(fb, "Delete Face", self._delete_faces)
        fl.addLayout(fb)
        inner.addWidget(face_grp)

        # Vertex table
        vert_grp = QGroupBox("Vertices")
        vl = QVBoxLayout(vert_grp)
        self.vert_table = QTableWidget(0, 4)
        self.vert_table.setHorizontalHeaderLabels(["#","X","Y","Z"])
        self.vert_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self.vert_table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.vert_table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self.vert_table.setEditTriggers(QAbstractItemView.EditTrigger.DoubleClicked)
        self.vert_table.itemSelectionChanged.connect(self._on_vert_selection)
        self.vert_table.itemChanged.connect(self._on_vert_cell_changed)
        vl.addWidget(self.vert_table)
        vb = QHBoxLayout()
        self._btn(vb, "Delete Vertex",  self._delete_verts)
        self._btn(vb, "Remove Orphans", self._remove_orphan_verts)
        vl.addLayout(vb)
        inner.addWidget(vert_grp)
        inner.setSizes([240,200])
        lay.addWidget(inner, 1)

        # Add face form
        add_grp = QGroupBox("Add Face")
        al = QHBoxLayout(add_grp)
        al.addWidget(QLabel("A:")); self._af_a = QSpinBox(); self._af_a.setRange(0,99999); al.addWidget(self._af_a)
        al.addWidget(QLabel("B:")); self._af_b = QSpinBox(); self._af_b.setRange(0,99999); al.addWidget(self._af_b)
        al.addWidget(QLabel("C:")); self._af_c = QSpinBox(); self._af_c.setRange(0,99999); al.addWidget(self._af_c)
        al.addWidget(QLabel("Material:"))
        self._af_mat = QComboBox()
        for idx2, name in sorted(SURFACE_MATERIALS.items()):
            self._af_mat.addItem(f"{idx2} — {name}", idx2)
        al.addWidget(self._af_mat)
        self._btn(al, "➕ Add", self._commit_add_face)
        lay.addWidget(add_grp)
        return w

    def _build_boxes_tab(self):
        """Box editor tab."""
        w = QWidget(); lay = QVBoxLayout(w); lay.setSpacing(4)

        self.box_table = QTableWidget(0, 9)
        self.box_table.setHorizontalHeaderLabels(
            ["#","Min X","Min Y","Min Z","Max X","Max Y","Max Z","Mat","Flag"])
        self.box_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self.box_table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.box_table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self.box_table.setEditTriggers(QAbstractItemView.EditTrigger.DoubleClicked)
        self.box_table.itemChanged.connect(self._on_box_cell_changed)
        lay.addWidget(self.box_table, 1)

        btns = QHBoxLayout()
        self._btn(btns, "Add Box",    self._add_box)
        self._btn(btns, "Delete Box", self._delete_boxes)
        self._btn(btns, "Duplicate",  self._duplicate_boxes)
        lay.addLayout(btns)

        # Quick-add form
        add_grp = QGroupBox("Add Box")
        al = QHBoxLayout(add_grp)
        for lbl, attr in [("Min X","_bx1"),("Min Y","_by1"),("Min Z","_bz1"),
                          ("Max X","_bx2"),("Max Y","_by2"),("Max Z","_bz2")]:
            al.addWidget(QLabel(lbl))
            sp = QDoubleSpinBox(); sp.setRange(-9999,9999); sp.setDecimals(3)
            sp.setMaximumWidth(72); setattr(self, attr, sp); al.addWidget(sp)
        al.addWidget(QLabel("Mat:"))
        self._b_mat = QSpinBox(); self._b_mat.setRange(0,70); self._b_mat.setMaximumWidth(48)
        al.addWidget(self._b_mat)
        self._btn(al, "➕ Add", self._commit_add_box)
        lay.addWidget(add_grp)
        return w

    def _build_spheres_tab(self):
        """Sphere editor tab."""
        w = QWidget(); lay = QVBoxLayout(w); lay.setSpacing(4)

        self.sphere_table = QTableWidget(0, 6)
        self.sphere_table.setHorizontalHeaderLabels(
            ["#","Centre X","Centre Y","Centre Z","Radius","Material"])
        self.sphere_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self.sphere_table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.sphere_table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self.sphere_table.setEditTriggers(QAbstractItemView.EditTrigger.DoubleClicked)
        self.sphere_table.itemChanged.connect(self._on_sphere_cell_changed)
        lay.addWidget(self.sphere_table, 1)

        btns = QHBoxLayout()
        self._btn(btns, "Add Sphere",    self._add_sphere)
        self._btn(btns, "Delete Sphere", self._delete_spheres)
        self._btn(btns, "Duplicate",     self._duplicate_spheres)
        lay.addLayout(btns)

        add_grp = QGroupBox("Add Sphere")
        al = QHBoxLayout(add_grp)
        for lbl, attr in [("X","_sx"),("Y","_sy"),("Z","_sz"),("Radius","_sr")]:
            al.addWidget(QLabel(lbl))
            sp = QDoubleSpinBox(); sp.setRange(-9999,9999); sp.setDecimals(3)
            if attr == "_sr": sp.setRange(0.001, 9999)
            sp.setMaximumWidth(72); setattr(self, attr, sp); al.addWidget(sp)
            if attr == "_sr": sp.setValue(1.0)
        al.addWidget(QLabel("Mat:"))
        self._s_mat = QSpinBox(); self._s_mat.setRange(0,70); self._s_mat.setMaximumWidth(48)
        al.addWidget(self._s_mat)
        self._btn(al, "➕ Add", self._commit_add_sphere)
        lay.addWidget(add_grp)
        return w

    def _build_bounds_tab(self):
        """Bounds editor — radius, centre, min, max."""
        w = QWidget(); lay = QVBoxLayout(w); lay.setSpacing(8)
        lay.addWidget(QLabel("Bounding volume for the entire model."))

        def row(label, attrs):
            grp = QGroupBox(label); gl = QHBoxLayout(grp)
            spins = []
            for lbl in attrs:
                gl.addWidget(QLabel(lbl))
                sp = QDoubleSpinBox(); sp.setRange(-9999,9999); sp.setDecimals(4)
                sp.setMaximumWidth(90); gl.addWidget(sp); spins.append(sp)
            return grp, spins

        grp_r = QGroupBox("Radius"); rl = QHBoxLayout(grp_r)
        self._bd_r = QDoubleSpinBox(); self._bd_r.setRange(0,9999); self._bd_r.setDecimals(4)
        rl.addWidget(self._bd_r); lay.addWidget(grp_r)

        grp_c, self._bd_c = row("Centre (X, Y, Z)", ["X","Y","Z"]); lay.addWidget(grp_c)
        grp_mn, self._bd_mn = row("Min (X, Y, Z)",   ["X","Y","Z"]); lay.addWidget(grp_mn)
        grp_mx, self._bd_mx = row("Max (X, Y, Z)",   ["X","Y","Z"]); lay.addWidget(grp_mx)

        btn_row = QHBoxLayout()
        self._btn(btn_row, "Recalculate from Geometry", self._recalc_bounds)
        self._btn(btn_row, "Apply Bounds",              self._apply_bounds)
        lay.addLayout(btn_row)
        lay.addStretch()
        return w


    def _populate_all(self):
        self._populate_faces()
        self._populate_verts()
        self._populate_boxes()
        self._populate_spheres()
        self._populate_bounds()
        self.viewport.set_model(self._model)

    def _populate_faces(self):
        self.face_table.blockSignals(True)
        faces = getattr(self._model, 'faces', [])
        self.face_table.setRowCount(len(faces))
        for i, f in enumerate(faces):
            mat_name = SURFACE_MATERIALS.get(f.material, str(f.material))
            for col, val in enumerate([i, f.a, f.b, f.c, f"{f.material} {mat_name}"]):
                item = QTableWidgetItem(str(val))
                if col == 0:
                    item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.face_table.setItem(i, col, item)
        self.face_table.blockSignals(False)

    def _populate_verts(self):
        self.vert_table.blockSignals(True)
        verts = getattr(self._model, 'vertices', [])
        self.vert_table.setRowCount(len(verts))
        for i, v in enumerate(verts):
            for col, val in enumerate([i, v.x, v.y, v.z]):
                item = QTableWidgetItem(f"{val:.4f}" if col > 0 else str(val))
                if col == 0:
                    item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.vert_table.setItem(i, col, item)
        self.vert_table.blockSignals(False)

    # ── Selection sync ────────────────────────────────────────────────────

    def _on_face_selection(self):
        rows = {idx.row() for idx in self.face_table.selectedIndexes()}
        self.viewport.set_selected_faces(rows)
        # Also highlight referenced vertices
        verts = set()
        faces = getattr(self._model, 'faces', [])
        for r in rows:
            if r < len(faces):
                f = faces[r]
                verts.update([f.a, f.b, f.c])
        self.viewport.set_selected_verts(verts)

    def _on_vert_selection(self):
        rows = {idx.row() for idx in self.vert_table.selectedIndexes()}
        self.viewport.set_selected_verts(rows)

    # ── Inline cell editing ───────────────────────────────────────────────

    def _on_face_cell_changed(self, item):
        row = item.row()
        col = item.column()
        faces = getattr(self._model, 'faces', [])
        if row >= len(faces) or col == 0:
            return
        self._push_undo("Edit face")
        try:
            val = int(item.text().split()[0])   # handles "5 Tarmac" format
            face = faces[row]
            if col == 1: face.a = val
            elif col == 2: face.b = val
            elif col == 3: face.c = val
            elif col == 4:
                face.material = val
                # Update display
                mat_name = SURFACE_MATERIALS.get(val, str(val))
                self.face_table.blockSignals(True)
                self.face_table.item(row, 4).setText(f"{val} {mat_name}")
                self.face_table.blockSignals(False)
            self.viewport.update()
            self._set_dirty()
        except (ValueError, IndexError):
            self._populate_faces()   # restore on bad input

    def _on_vert_cell_changed(self, item):
        row = item.row()
        col = item.column()
        verts = getattr(self._model, 'vertices', [])
        if row >= len(verts) or col == 0:
            return
        self._push_undo("Move vertex")
        try:
            val = float(item.text())
            v = verts[row]
            if col == 1: v.x = val
            elif col == 2: v.y = val
            elif col == 3: v.z = val
            self.viewport.update()
            self._set_dirty()
        except ValueError:
            self._populate_verts()

    # ── Operations ────────────────────────────────────────────────────────

    def _add_face(self):
        """Open add-face form (just scrolls to it — it's already visible)."""
        n = len(getattr(self._model, 'vertices', []))
        self._af_a.setMaximum(max(0, n-1))
        self._af_b.setMaximum(max(0, n-1))
        self._af_c.setMaximum(max(0, n-1))
        self._status.setText(f"Set vertex indices (0–{n-1}) then click Add")

    def _commit_add_face(self):
        verts = getattr(self._model, 'vertices', [])
        a = self._af_a.value()
        b = self._af_b.value()
        c = self._af_c.value()
        n = len(verts)
        if a >= n or b >= n or c >= n:
            QMessageBox.warning(self, "Invalid",
                f"Vertex indices must be 0–{n-1}. Model has {n} vertices.")
            return
        if a == b or b == c or a == c:
            QMessageBox.warning(self, "Invalid", "Face vertices must all be different.")
            return
        self._push_undo("Add face")
        from apps.methods.col_workshop_classes import COLFace
        mat = self._af_mat.currentData()
        new_face = COLFace(a=a, b=b, c=c, material=mat, flag=0, brightness=0, light=0)
        self._model.faces.append(new_face)
        self._populate_faces()
        self.viewport.update()
        # Select the new face
        last = self.face_table.rowCount() - 1
        self.face_table.selectRow(last)
        self._set_dirty()
        self._status.setText(f"Added face {last} ({a},{b},{c}) mat={mat}")

    def _delete_faces(self):
        rows = sorted({idx.row() for idx in self.face_table.selectedIndexes()}, reverse=True)
        if not rows:
            self._status.setText("Select faces to delete first.")
            return
        self._push_undo(f"Delete {len(rows)} face(s)")
        faces = self._model.faces
        for r in rows:
            if r < len(faces):
                faces.pop(r)
        self._populate_faces()
        self.viewport.update()
        self._set_dirty()
        self._status.setText(f"Deleted {len(rows)} face(s). {len(faces)} remaining.")

    def _delete_verts(self):
        rows = sorted({idx.row() for idx in self.vert_table.selectedIndexes()}, reverse=True)
        if not rows:
            self._status.setText("Select vertices to delete first.")
            return
        # Check if any selected vertex is in use
        faces = getattr(self._model, 'faces', [])
        used = {f.a for f in faces} | {f.b for f in faces} | {f.c for f in faces}
        blocked = [r for r in rows if r in used]
        if blocked:
            res = QMessageBox.question(
                self, "Vertex in use",
                f"{len(blocked)} vertex/vertices are referenced by faces.\n"
                "Delete those faces first, or proceed and leave dangling refs?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.Cancel)
            if res != QMessageBox.StandardButton.Yes:
                return
        self._push_undo(f"Delete {len(rows)} vertex/vertices")
        verts = self._model.vertices
        # Remap face indices
        remap = {}
        new_idx = 0
        for old_idx in range(len(verts)):
            if old_idx not in rows:
                remap[old_idx] = new_idx
                new_idx += 1
        for face in faces:
            face.a = remap.get(face.a, face.a)
            face.b = remap.get(face.b, face.b)
            face.c = remap.get(face.c, face.c)
        for r in rows:
            if r < len(verts):
                verts.pop(r)
        self._populate_all()
        self._set_dirty()
        self._status.setText(f"Deleted {len(rows)} vert(s). {len(verts)} remaining.")

    def _remove_orphan_verts(self):
        faces  = getattr(self._model, 'faces', [])
        verts  = getattr(self._model, 'vertices', [])
        used   = {f.a for f in faces} | {f.b for f in faces} | {f.c for f in faces}
        orphans = [i for i in range(len(verts)) if i not in used]
        if not orphans:
            self._status.setText("No orphan vertices found.")
            return
        self._push_undo(f"Remove {len(orphans)} orphan vertex/vertices")
        for i in sorted(orphans, reverse=True):
            verts.pop(i)
        # Rebuild face index map
        old_to_new = {}
        new_i = 0
        for old_i in range(len(verts) + len(orphans)):
            if old_i not in orphans:
                old_to_new[old_i] = new_i
                new_i += 1
        for face in faces:
            face.a = old_to_new.get(face.a, face.a)
            face.b = old_to_new.get(face.b, face.b)
            face.c = old_to_new.get(face.c, face.c)
        self._populate_all()
        self._set_dirty()
        self._status.setText(f"Removed {len(orphans)} orphan vert(s).")

    # ── Undo ──────────────────────────────────────────────────────────────

    def _push_undo(self, description=""):
        self._undo_stack.append((description, copy.deepcopy(self._model)))
        self._undo_btn.setEnabled(True)
        if len(self._undo_stack) > 50:
            self._undo_stack.pop(0)

    def _undo(self):
        if not self._undo_stack:
            return
        desc, saved = self._undo_stack.pop()
        self._model = saved
        self._populate_all()
        self._undo_btn.setEnabled(bool(self._undo_stack))
        self._status.setText(f"Undone: {desc}")

    # ── Dirty tracking + apply ────────────────────────────────────────────

    def _set_dirty(self):
        self._dirty = True
        n_f = len(getattr(self._model, 'faces', []))
        n_v = len(getattr(self._model, 'vertices', []))
        self.setWindowTitle(
            f"Mesh Editor* — {getattr(self._model.header, 'name', 'Model')}  "
            f"F:{n_f} V:{n_v}")

    # ── Box helpers ──────────────────────────────────────────────────────

    def _pt(self, obj):
        """Return (x,y,z) from Vector3, tuple, or list."""
        if hasattr(obj,'x'):  return obj.x, obj.y, obj.z
        if obj is None:       return 0.0, 0.0, 0.0
        return float(obj[0]), float(obj[1]), float(obj[2])

    def _box_pts(self, box):
        mn = getattr(box,'min_point', getattr(box,'min', None))
        mx = getattr(box,'max_point', getattr(box,'max', None))
        return mn, mx

    # ── Box populate / edit ───────────────────────────────────────────────

    def _populate_boxes(self):
        self.box_table.blockSignals(True)
        boxes = getattr(self._model,'boxes',[])
        self.box_table.setRowCount(len(boxes))
        for i, box in enumerate(boxes):
            mn, mx = self._box_pts(box)
            mnx,mny,mnz = self._pt(mn)
            mxx,mxy,mxz = self._pt(mx)
            mat = getattr(box,'material',0)
            flag = getattr(box,'flag',0)
            for col, val in enumerate([i, mnx,mny,mnz, mxx,mxy,mxz, mat, flag]):
                item = QTableWidgetItem(f"{val:.4f}" if isinstance(val,float) else str(val))
                if col == 0: item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.box_table.setItem(i, col, item)
        self.box_table.blockSignals(False)

    def _on_box_cell_changed(self, item):
        row, col = item.row(), item.column()
        boxes = getattr(self._model,'boxes',[])
        if row >= len(boxes) or col == 0: return
        self._push_undo("Edit box")
        try:
            val = float(item.text())
            box = boxes[row]
            mn, mx = self._box_pts(box)
            if col==1 and mn: mn.x=val
            elif col==2 and mn: mn.y=val
            elif col==3 and mn: mn.z=val
            elif col==4 and mx: mx.x=val
            elif col==5 and mx: mx.y=val
            elif col==6 and mx: mx.z=val
            elif col==7: box.material=int(val)
            elif col==8: box.flag=int(val)
            self.viewport.update(); self._set_dirty()
        except ValueError:
            self._populate_boxes()

    def _add_box(self):
        self._status.setText("Fill in coordinates below and click ➕ Add")

    def _commit_add_box(self):
        self._push_undo("Add box")
        from apps.methods.col_workshop_classes import COLBox
        from apps.methods.col_core_classes import Vector3
        mn = Vector3(self._bx1.value(), self._by1.value(), self._bz1.value())
        mx = Vector3(self._bx2.value(), self._by2.value(), self._bz2.value())
        mat = self._b_mat.value()
        box = COLBox(min=mn, max=mx, material=mat, flag=0, brightness=0, light=0)
        self._model.boxes.append(box)
        self._populate_boxes()
        self.box_table.selectRow(self.box_table.rowCount()-1)
        self.viewport.update(); self._set_dirty()
        self._status.setText(f"Added box {len(self._model.boxes)-1}")

    def _delete_boxes(self):
        rows = sorted({idx.row() for idx in self.box_table.selectedIndexes()}, reverse=True)
        if not rows: self._status.setText("Select boxes to delete first."); return
        self._push_undo(f"Delete {len(rows)} box(es)")
        boxes = self._model.boxes
        for r in rows:
            if r < len(boxes): boxes.pop(r)
        self._populate_boxes(); self.viewport.update(); self._set_dirty()
        self._status.setText(f"Deleted {len(rows)} box(es). {len(boxes)} remaining.")

    def _duplicate_boxes(self):
        import copy
        rows = sorted({idx.row() for idx in self.box_table.selectedIndexes()})
        if not rows: self._status.setText("Select boxes to duplicate."); return
        self._push_undo(f"Duplicate {len(rows)} box(es)")
        boxes = self._model.boxes
        for r in rows:
            if r < len(boxes): boxes.append(copy.deepcopy(boxes[r]))
        self._populate_boxes(); self._set_dirty()

    # ── Sphere populate / edit ────────────────────────────────────────────

    def _populate_spheres(self):
        self.sphere_table.blockSignals(True)
        spheres = getattr(self._model,'spheres',[])
        self.sphere_table.setRowCount(len(spheres))
        for i, sph in enumerate(spheres):
            cx,cy,cz = self._pt(sph.center if hasattr(sph,'center') else None)
            r = getattr(sph,'radius',1.0)
            mat = getattr(sph,'material',0)
            for col, val in enumerate([i, cx,cy,cz, r, mat]):
                item = QTableWidgetItem(f"{val:.4f}" if isinstance(val,float) else str(val))
                if col==0: item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.sphere_table.setItem(i, col, item)
        self.sphere_table.blockSignals(False)

    def _on_sphere_cell_changed(self, item):
        row, col = item.row(), item.column()
        spheres = getattr(self._model,'spheres',[])
        if row >= len(spheres) or col==0: return
        self._push_undo("Edit sphere")
        try:
            val = float(item.text())
            sph = spheres[row]
            c = sph.center if hasattr(sph,'center') else None
            if col==1 and c: c.x=val
            elif col==2 and c: c.y=val
            elif col==3 and c: c.z=val
            elif col==4: sph.radius=max(0.001,val)
            elif col==5: sph.material=int(val)
            self.viewport.update(); self._set_dirty()
        except ValueError:
            self._populate_spheres()

    def _add_sphere(self):
        self._status.setText("Fill in coordinates below and click ➕ Add")

    def _commit_add_sphere(self):
        self._push_undo("Add sphere")
        from apps.methods.col_workshop_classes import COLSphere
        from apps.methods.col_core_classes import Vector3
        centre = Vector3(self._sx.value(), self._sy.value(), self._sz.value())
        sph = COLSphere(radius=self._sr.value(), center=centre,
                        material=self._s_mat.value(), flag=0, brightness=0, light=0)
        self._model.spheres.append(sph)
        self._populate_spheres()
        self.sphere_table.selectRow(self.sphere_table.rowCount()-1)
        self.viewport.update(); self._set_dirty()
        self._status.setText(f"Added sphere {len(self._model.spheres)-1}")

    def _delete_spheres(self):
        rows = sorted({idx.row() for idx in self.sphere_table.selectedIndexes()}, reverse=True)
        if not rows: self._status.setText("Select spheres to delete first."); return
        self._push_undo(f"Delete {len(rows)} sphere(s)")
        spheres = self._model.spheres
        for r in rows:
            if r < len(spheres): spheres.pop(r)
        self._populate_spheres(); self.viewport.update(); self._set_dirty()
        self._status.setText(f"Deleted {len(rows)} sphere(s).")

    def _duplicate_spheres(self):
        import copy
        rows = sorted({idx.row() for idx in self.sphere_table.selectedIndexes()})
        if not rows: self._status.setText("Select spheres to duplicate."); return
        self._push_undo(f"Duplicate {len(rows)} sphere(s)")
        spheres = self._model.spheres
        for r in rows:
            if r < len(spheres): spheres.append(copy.deepcopy(spheres[r]))
        self._populate_spheres(); self._set_dirty()

    # ── Bounds populate / edit ────────────────────────────────────────────

    def _populate_bounds(self):
        bounds = getattr(self._model,'bounds',None)
        if not bounds: return
        self._bd_r.setValue(getattr(bounds,'radius',0.0))
        cx,cy,cz   = self._pt(getattr(bounds,'center',None))
        mnx,mny,mnz = self._pt(getattr(bounds,'min',None))
        mxx,mxy,mxz = self._pt(getattr(bounds,'max',None))
        for sp,v in zip(self._bd_c,  [cx,cy,cz]):   sp.setValue(v)
        for sp,v in zip(self._bd_mn, [mnx,mny,mnz]): sp.setValue(v)
        for sp,v in zip(self._bd_mx, [mxx,mxy,mxz]): sp.setValue(v)

    def _apply_bounds(self):
        bounds = getattr(self._model,'bounds',None)
        if not bounds: return
        self._push_undo("Edit bounds")
        from apps.methods.col_core_classes import Vector3
        bounds.radius = self._bd_r.value()
        cx,cy,cz   = [s.value() for s in self._bd_c]
        mnx,mny,mnz = [s.value() for s in self._bd_mn]
        mxx,mxy,mxz = [s.value() for s in self._bd_mx]
        if hasattr(bounds,'center') and hasattr(bounds.center,'x'):
            bounds.center.x=cx; bounds.center.y=cy; bounds.center.z=cz
        else: bounds.center=Vector3(cx,cy,cz)
        if hasattr(bounds,'min') and hasattr(bounds.min,'x'):
            bounds.min.x=mnx; bounds.min.y=mny; bounds.min.z=mnz
        else: bounds.min=Vector3(mnx,mny,mnz)
        if hasattr(bounds,'max') and hasattr(bounds.max,'x'):
            bounds.max.x=mxx; bounds.max.y=mxy; bounds.max.z=mxz
        else: bounds.max=Vector3(mxx,mxy,mxz)
        self._set_dirty(); self._status.setText("Bounds applied.")

    def _recalc_bounds(self):
        """Recalculate bounding volume from all geometry."""
        self._push_undo("Recalculate bounds")
        import math
        verts   = getattr(self._model,'vertices',[])
        boxes   = getattr(self._model,'boxes',[])
        spheres = getattr(self._model,'spheres',[])
        pts = [(v.x,v.y,v.z) for v in verts]
        for box in boxes:
            mn,mx = self._box_pts(box)
            if mn: pts.append(self._pt(mn))
            if mx: pts.append(self._pt(mx))
        for sph in spheres:
            cx,cy,cz = self._pt(getattr(sph,'center',None))
            r = getattr(sph,'radius',0)
            for dx,dy,dz in [(r,0,0),(-r,0,0),(0,r,0),(0,-r,0),(0,0,r),(0,0,-r)]:
                pts.append((cx+dx,cy+dy,cz+dz))
        if not pts:
            self._status.setText("No geometry to calculate bounds from."); return
        min_x=min(p[0] for p in pts); max_x=max(p[0] for p in pts)
        min_y=min(p[1] for p in pts); max_y=max(p[1] for p in pts)
        min_z=min(p[2] for p in pts); max_z=max(p[2] for p in pts)
        cx=(min_x+max_x)/2; cy=(min_y+max_y)/2; cz=(min_z+max_z)/2
        r=math.sqrt(max((x-cx)**2+(y-cy)**2+(z-cz)**2 for x,y,z in pts))
        bounds = getattr(self._model,'bounds',None)
        if bounds:
            from apps.methods.col_core_classes import Vector3
            bounds.radius=r
            bounds.center=Vector3(cx,cy,cz)
            bounds.min=Vector3(min_x,min_y,min_z)
            bounds.max=Vector3(max_x,max_y,max_z)
        self._populate_bounds(); self._set_dirty()
        self._status.setText(f"Bounds recalculated: r={r:.3f} centre=({cx:.2f},{cy:.2f},{cz:.2f})")

    def _apply_and_close(self):
        """Write edited model back to the COL file and push to workshop undo."""
        ws = self.workshop
        # Push to workshop-level undo stack
        ws._push_undo(self.model_index, "Mesh edit")
        ws.current_col_file.models[self.model_index] = copy.deepcopy(self._model)
        # Refresh workshop UI
        ws._populate_collision_list()
        ws._populate_compact_col_list()
        if hasattr(ws, 'preview_widget'):
            ws.preview_widget.set_current_model(
                ws.current_col_file.models[self.model_index], self.model_index)
        n_f = len(getattr(self._model, 'faces', []))
        n_v = len(getattr(self._model, 'vertices', []))
        if hasattr(ws, 'main_window') and ws.main_window:
            ws.main_window.log_message(
                f"Mesh edit applied: {getattr(self._model.header,'name','')} "
                f"F:{n_f} V:{n_v}")
        self.accept()


##Functions -
def open_col_mesh_editor(workshop, parent=None): #vers 1
    """Open the COL Mesh Editor for the currently selected model."""
    if not getattr(workshop, 'current_col_file', None):
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.warning(parent or workshop, "No file", "No COL file loaded.")
        return

    # Find selected model index
    model_index = None
    active = (workshop.col_compact_list
              if getattr(workshop, '_col_view_mode', 'list') == 'detail'
              else workshop.collision_list)
    rows = active.selectionModel().selectedRows()
    if rows:
        row = rows[0].row()
        item = active.item(row, 1) or active.item(row, 0)
        if item:
            from PyQt6.QtCore import Qt
            model_index = item.data(Qt.ItemDataRole.UserRole)

    if model_index is None:
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.warning(parent or workshop, "No model selected",
                            "Select a model in the list first.")
        return

    models = getattr(workshop.current_col_file, 'models', [])
    if model_index >= len(models):
        return

    dlg = COLMeshEditor(workshop, model_index, parent=parent or workshop)
    dlg.exec()
