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
    QAbstractItemView, QFrame, QSpinBox
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
        if not verts or not faces:
            p.setPen(QColor(100, 100, 100))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No mesh")
            return

        # Project vertices
        yr = math.radians(self._yaw);  cy, sy = math.cos(yr), math.sin(yr)
        pr = math.radians(self._pitch); cp, sp = math.cos(pr), math.sin(pr)

        def proj(v):
            x, y, z = v.x, v.y, v.z
            rx = x*cy - y*sy;  ry = x*sy + y*cy;  rz = z
            rx2 = rx;           ry2 = ry*cp - rz*sp
            return rx2, ry2

        pts2d = [proj(v) for v in verts]
        if not pts2d:
            return

        xs = [p2[0] for p2 in pts2d]
        ys = [p2[1] for p2 in pts2d]
        mn_x, mx_x = min(xs), max(xs)
        mn_y, mx_y = min(ys), max(ys)
        rng = max(mx_x - mn_x, mx_y - mn_y, 0.001)
        pad = 20
        scale = ((min(W, H) - pad*2) / rng) * self._zoom
        cx = (mn_x + mx_x) / 2
        cy2 = (mn_y + mx_y) / 2
        ox = W/2 - cx*scale + self._pan_x
        oy = H/2 - cy2*scale + self._pan_y

        def sx(i): return pts2d[i][0] * scale + ox
        def sy(i): return pts2d[i][1] * scale + oy

        # Draw faces
        for fi, face in enumerate(faces):
            a, b, c = face.a, face.b, face.c
            if a >= len(verts) or b >= len(verts) or c >= len(verts):
                continue
            mat_col = MATERIAL_COLORS.get(face.material, QColor(120, 120, 120))
            selected = fi in self._sel_faces
            if selected:
                fill = QColor(255, 200, 50, 160)
                pen_col = QColor(255, 220, 0)
                pen_w = 2
            else:
                fill = QColor(mat_col.red(), mat_col.green(), mat_col.blue(), 80)
                pen_col = QColor(mat_col.red()//2+60, mat_col.green()//2+60, mat_col.blue()//2+60)
                pen_w = 1
            from PyQt6.QtGui import QPolygonF
            from PyQt6.QtCore import QPointF
            poly = QPolygonF([QPointF(sx(a), sy(a)), QPointF(sx(b), sy(b)), QPointF(sx(c), sy(c))])
            p.setBrush(QBrush(fill))
            p.setPen(QPen(pen_col, pen_w))
            p.drawPolygon(poly)

        # Draw selected vertices
        p.setPen(QPen(QColor(255, 80, 80), 1))
        p.setBrush(QBrush(QColor(255, 80, 80)))
        for vi in self._sel_verts:
            if vi < len(verts):
                p.drawEllipse(int(sx(vi))-4, int(sy(vi))-4, 8, 8)

        # HUD
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

        splitter = QSplitter(Qt.Orientation.Horizontal)

        # ── Face panel ────────────────────────────────────────────────────
        face_grp = QGroupBox("Faces")
        fl = QVBoxLayout(face_grp)
        self.face_table = QTableWidget(0, 5)
        self.face_table.setHorizontalHeaderLabels(["#", "A", "B", "C", "Material"])
        self.face_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self.face_table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.face_table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self.face_table.setEditTriggers(QAbstractItemView.EditTrigger.DoubleClicked)
        self.face_table.itemSelectionChanged.connect(self._on_face_selection)
        self.face_table.itemChanged.connect(self._on_face_cell_changed)
        fl.addWidget(self.face_table)

        face_btns = QHBoxLayout()
        self._btn(face_btns, "Add Face",    self._add_face)
        self._btn(face_btns, "Delete Face", self._delete_faces)
        fl.addLayout(face_btns)
        splitter.addWidget(face_grp)

        # ── Vertex panel ──────────────────────────────────────────────────
        vert_grp = QGroupBox("Vertices")
        vl = QVBoxLayout(vert_grp)
        self.vert_table = QTableWidget(0, 4)
        self.vert_table.setHorizontalHeaderLabels(["#", "X", "Y", "Z"])
        self.vert_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self.vert_table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.vert_table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self.vert_table.setEditTriggers(QAbstractItemView.EditTrigger.DoubleClicked)
        self.vert_table.itemSelectionChanged.connect(self._on_vert_selection)
        self.vert_table.itemChanged.connect(self._on_vert_cell_changed)
        vl.addWidget(self.vert_table)

        vert_btns = QHBoxLayout()
        self._btn(vert_btns, "Delete Vertex",      self._delete_verts)
        self._btn(vert_btns, "Remove Orphans",     self._remove_orphan_verts)
        vl.addLayout(vert_btns)
        splitter.addWidget(vert_grp)

        # ── Viewport ──────────────────────────────────────────────────────
        vp_grp = QGroupBox("Preview")
        vpl = QVBoxLayout(vp_grp)
        self.viewport = COLMeshEditorViewport()
        vpl.addWidget(self.viewport)
        vpl.addWidget(QLabel("Left-drag: pan  Right-drag: rotate  Scroll: zoom",
                             alignment=Qt.AlignmentFlag.AlignCenter))
        splitter.addWidget(vp_grp)

        splitter.setSizes([340, 300, 220])
        root.addWidget(splitter, 1)

        # ── Status bar ────────────────────────────────────────────────────
        self._status = QLabel("Ready")
        self._status.setStyleSheet("color: #aaa; font-size: 10px;")
        root.addWidget(self._status)

        # ── Add-face form ─────────────────────────────────────────────────
        add_grp = QGroupBox("Add Face — vertex indices (must already exist)")
        add_lay = QHBoxLayout(add_grp)
        add_lay.addWidget(QLabel("A:"))
        self._af_a = QSpinBox(); self._af_a.setRange(0, 99999); add_lay.addWidget(self._af_a)
        add_lay.addWidget(QLabel("B:"))
        self._af_b = QSpinBox(); self._af_b.setRange(0, 99999); add_lay.addWidget(self._af_b)
        add_lay.addWidget(QLabel("C:"))
        self._af_c = QSpinBox(); self._af_c.setRange(0, 99999); add_lay.addWidget(self._af_c)
        add_lay.addWidget(QLabel("Material:"))
        self._af_mat = QComboBox()
        for idx, name in sorted(SURFACE_MATERIALS.items()):
            self._af_mat.addItem(f"{idx} — {name}", idx)
        add_lay.addWidget(self._af_mat)
        self._btn(add_lay, "➕ Add", self._commit_add_face)
        root.addWidget(add_grp)

        # ── Bottom buttons ────────────────────────────────────────────────
        bot = QHBoxLayout()
        self._undo_btn = self._btn(bot, "↩ Undo", self._undo)
        self._undo_btn.setEnabled(False)
        bot.addStretch()
        self._btn(bot, "Apply & Close", self._apply_and_close)
        self._btn(bot, "Close",         self.reject)
        root.addLayout(bot)

    def _btn(self, layout, label, slot, enabled=True):
        b = QPushButton(label)
        b.clicked.connect(slot)
        b.setEnabled(enabled)
        layout.addWidget(b)
        return b

    # ── Population ────────────────────────────────────────────────────────

    def _populate_all(self):
        self._populate_faces()
        self._populate_verts()
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
