#!/usr/bin/env python3
#this belongs in components.Col_Editor.col_workshop.py - Version: 74
# X-Seti - August10 2025 - Converted col editor using gui base template.

"""
components/Col_Editor/col_workshop.py
COL Editor - Main collision editor interface
"""

import os
# Force X11/GLX backend for NVIDIA on Wayland
os.environ['QT_QPA_PLATFORM'] = 'xcb'
os.environ['QSG_RHI_BACKEND'] = 'opengl'
os.environ['LIBGL_ALWAYS_SOFTWARE'] = '0'  # Use hardware acceleration

import tempfile
import subprocess
import shutil
import struct
import sys
import io
import numpy as np
from pathlib import Path
from typing import Optional, List, Dict, Tuple


# Add project root to path for standalone mode
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

# Import PyQt6
from PyQt6.QtWidgets import (QApplication, QSlider, QCheckBox,
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QListWidget, QDialog, QFormLayout, QSpinBox,  QListWidgetItem, QLabel, QPushButton, QFrame, QFileDialog, QLineEdit, QTextEdit, QMessageBox, QScrollArea, QGroupBox, QTableWidget, QTableWidgetItem, QColorDialog, QHeaderView, QAbstractItemView, QMenu, QComboBox, QInputDialog, QTabWidget, QDoubleSpinBox, QRadioButton, QStyledItemDelegate
)
from PyQt6.QtCore import Qt, pyqtSignal, QSize, QPoint, QRect, QByteArray
from PyQt6.QtGui import QFont, QIcon, QPixmap, QImage, QPainter, QPen, QBrush, QColor, QCursor
# QAction location varies by PyQt6 version — try bothQStyledItemDelegate
try:
    from PyQt6.QtGui import QAction
except ImportError:
    from PyQt6.QtWidgets import QAction
from PyQt6.QtSvg import QSvgRenderer

# Import project modules AFTER path setup
from apps.methods.imgfactory_svg_icons import SVGIconFactory

# COL Workshop parser system
from apps.methods.col_workshop_classes import (
    COLModel, COLVersion, COLHeader, COLBounds,
    COLSphere, COLBox, COLVertex, COLFace
)

from apps.methods.col_workshop_structures import setup_col_table_structure, populate_col_table
from apps.methods.col_workshop_parser import COLParser
from apps.methods.col_workshop_loader import COLFile
from apps.gui.tool_menu_mixin import ToolMenuMixin


# Temporary 3D viewport placeholder
class COL3DViewport(QWidget): #vers 2
    """COL preview viewport.
    Left-drag = pan, Right-drag = free rotate, Scroll = zoom, Middle = pan.
    G key / button = translate gizmo, R key / button = rotate gizmo.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumSize(200, 200)
        self._model        = None
        self._yaw          = 30.0
        self._pitch        = 20.0
        self._zoom         = 1.0
        self._pan_x        = 0.0
        self._pan_y        = 0.0
        self._flip_h       = False
        self._flip_v       = False
        self._show_spheres = True
        self._show_boxes   = True
        self._show_mesh    = True
        self._backface     = False
        self._render_style = 'semi'
        self._bg_color     = (25, 25, 35)
        # drag state
        self._left_drag    = None
        self._right_drag   = None
        self._mid_drag     = None
        # gizmo
        self._gizmo_mode   = 'translate'  # 'translate' | 'rotate'
        self._gizmo_drag   = None         # 'X'|'Y'|'Z' while dragging
        self._gizmo_start  = None
        # face selection / paint state
        self._selected_faces  = set()    # set of face indices currently selected
        self._paint_mode      = False    # True = click face to paint material
        self._paint_material  = 0
        self._tool_mode       = 'select'  # 'select' | 'paint' | 'dropper' | 'select_all_mat'
        self._dropper_active  = False        # material id to apply in paint mode
        self.on_face_selected = None     # callback(face_index, face) when face clicked
        self._drag_selecting  = False    # True while LMB held after face click
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.DefaultContextMenu)
        self.setFocusPolicy(Qt.FocusPolicy.ClickFocus)
        self.setCursor(Qt.CursorShape.ArrowCursor)


    # ── public API ────────────────────────────────────────────────────────
    def set_current_file(self, col_file): pass
    def set_view_options(self, **kw):     pass

    def set_current_model(self, model, index=0):
        self._model = model
        # Reset view so the new model is centred and visible
        self._pan_x = 0.0
        self._pan_y = 0.0
        self._zoom  = 1.0
        self.update()

    def zoom_in(self):
        self._zoom = min(20.0, self._zoom * 1.25); self.update()

    def zoom_out(self):
        self._zoom = max(0.05, self._zoom / 1.25); self.update()

    def reset_view(self):
        self._yaw = 30.0; self._pitch = 20.0
        self._zoom = 1.0; self._pan_x = self._pan_y = 0.0
        self._flip_h = self._flip_v = False
        self.update()

    def fit_to_window(self):
        self._pan_x = self._pan_y = 0.0; self._zoom = 1.0; self.update()

    def pan(self, dx, dy):
        self._pan_x += dx; self._pan_y += dy; self.update()

    def rotate_cw(self):
        self._yaw = (self._yaw + 90) % 360; self.update()

    def rotate_ccw(self):
        self._yaw = (self._yaw - 90) % 360; self.update()

    def flip_horizontal(self):
        self._flip_h = not self._flip_h; self.update()

    def flip_vertical(self):
        self._flip_v = not self._flip_v; self.update()

    def set_background_color(self, rgb):
        self._bg_color = rgb; self.update()

    def set_show_spheres(self, v): self._show_spheres = v; self.update()
    def set_show_boxes(self,   v): self._show_boxes   = v; self.update()
    def set_show_mesh(self,    v): self._show_mesh     = v; self.update()
    def set_backface(self,     v): self._backface      = v; self.update()
    def set_render_style(self, s): self._render_style  = s; self.update()

    def toggle_gizmo_mode(self):
        self._gizmo_mode = 'rotate' if self._gizmo_mode == 'translate' else 'translate'
        self._gizmo_drag = None
        self.update()

    def _set_gizmo(self, mode):
        self._gizmo_mode = mode; self._gizmo_drag = None; self.update()


    # ── projection (self-contained, no workshop needed) ──────────────────
    def _proj(self, x, y, z):
        """Project 3D world point → 2D screen pixel using current view state."""
        import math
        yr = math.radians(self._yaw);   cy, sy = math.cos(yr), math.sin(yr)
        pr = math.radians(self._pitch); cp, sp = math.cos(pr), math.sin(pr)
        rx = x*cy - y*sy
        ry = x*sy + y*cy
        ry2 = ry*cp - z*sp
        return rx, ry2

    def _get_scale_origin(self):
        """Return (scale, ox, oy) mapping 3D projected coords to screen pixels."""
        W, H = self.width(), self.height()
        model = self._model
        if not model:
            return 50.0 * self._zoom, W/2 + self._pan_x, H/2 + self._pan_y

        verts = getattr(model, 'vertices', [])
        spheres = getattr(model, 'spheres', [])
        boxes   = getattr(model, 'boxes',   [])

        pts3 = [(v.x, v.y, v.z) for v in verts]
        for s in spheres:
            c = s.center; r = s.radius
            cx,cy,cz = (c.x,c.y,c.z) if hasattr(c,'x') else (c[0],c[1],c[2])
            pts3 += [(cx-r,cy,cz),(cx+r,cy,cz),(cx,cy-r,cz),(cx,cy+r,cz),(cx,cy,cz-r),(cx,cy,cz+r)]
        for b in boxes:
            mn = b.min if not hasattr(b,'min_point') else b.min_point
            mx = b.max if not hasattr(b,'max_point') else b.max_point
            for p in [mn, mx]:
                pts3.append((p.x,p.y,p.z) if hasattr(p,'x') else (p[0],p[1],p[2]))

        if not pts3:
            return 50.0 * self._zoom, W/2 + self._pan_x, H/2 + self._pan_y

        pts2 = [self._proj(x,y,z) for x,y,z in pts3]
        xs = [p[0] for p in pts2]; ys = [p[1] for p in pts2]
        rng = max(max(xs)-min(xs), max(ys)-min(ys), 0.001)
        pad = 40
        base_scale = (min(W,H) - pad*2) / rng
        scale = base_scale * self._zoom
        cx2 = (min(xs)+max(xs))/2; cy2 = (min(ys)+max(ys))/2
        ox = W/2 - cx2*scale + self._pan_x
        oy = H/2 - cy2*scale + self._pan_y
        return scale, ox, oy

    def _to_screen(self, x, y, z):
        scale, ox, oy = self._get_scale_origin()
        px, py = self._proj(x, y, z)
        return px*scale + ox, py*scale + oy


    # ── gizmo hit test ───────────────────────────────────────────────────
    def _gizmo_centre(self):
        """Screen coords of gizmo origin (model centroid)."""
        model = self._model
        if not model: return None
        verts = getattr(model, 'vertices', [])
        if verts:
            cx = sum(v.x for v in verts)/len(verts)
            cy = sum(v.y for v in verts)/len(verts)
            cz = sum(v.z for v in verts)/len(verts)
        else:
            cx = cy = cz = 0.0
        return self._to_screen(cx, cy, cz)

    def _gizmo_arm(self):
        return max(45, min(self.width(), self.height()) * 0.15)

    def _hit_gizmo(self, mx, my):
        """Return axis 'X'/'Y'/'Z' if click near a gizmo handle, else None."""
        import math
        ctr = self._gizmo_centre()
        if not ctr: return None
        gx, gy = ctr
        arm = self._gizmo_arm()
        best, best_d = None, 16
        for (dx,dy,dz), name in [((1,0,0),'X'),((0,1,0),'Y'),((0,0,1),'Z')]:
            px, py = self._proj(dx, dy, dz)
            tx, ty = gx+px*arm, gy+py*arm
            if math.hypot(mx-tx, my-ty) < best_d:
                best_d, best = math.hypot(mx-tx, my-ty), name
        return best

    def set_paint_mode(self, enabled: bool, material_id: int = 0):
        """Enable/disable paint mode. In paint mode LMB click paints a face."""
        self._paint_mode     = enabled
        self._paint_material = material_id
        self._selected_faces = set()
        self.setCursor(Qt.CursorShape.CrossCursor if enabled else Qt.CursorShape.OpenHandCursor)
        self.update()

    def _pick_face(self, mx, my):
        """Return (face_index, face) of the closest face whose projected centroid
        is within 20px of click, or (None, None)."""
        import math
        model = self._model
        if not model: return None, None
        verts = getattr(model, 'vertices', [])
        faces = getattr(model, 'faces',   [])
        if not verts or not faces: return None, None

        scale, ox, oy = self._get_scale_origin()

        def ts(x, y, z):
            px, py = self._proj(x, y, z)
            return px * scale + ox, py * scale + oy

        def g3(obj):
            if hasattr(obj, 'x'): return obj.x, obj.y, obj.z
            return float(obj[0]), float(obj[1]), float(obj[2])

        best_i, best_d = None, 20.0   # 20px pick radius
        for i, face in enumerate(faces):
            fa = getattr(face, 'a', None)
            if fa is None: continue
            try:
                ax, ay, az = g3(verts[face.a])
                bx, by, bz = g3(verts[face.b])
                cx, cy, cz = g3(verts[face.c])
            except IndexError: continue
            # centroid in screen space
            sx, sy = ts((ax+bx+cx)/3, (ay+by+cy)/3, (az+bz+cz)/3)
            d = math.hypot(mx - sx, my - sy)
            if d < best_d:
                best_d, best_i = d, i

        if best_i is not None:
            return best_i, faces[best_i]
        return None, None


    # ── mouse ────────────────────────────────────────────────────────────
    def mousePressEvent(self, event):
        mx, my = event.position().x(), event.position().y()
        W, H = self.width(), self.height()
        if event.button() == Qt.MouseButton.LeftButton:
            # Top-right chips
            if self._paint_mode:
                # Hit-test using same constants as paintEvent
                _CHIP_H  = 22; _BTN_W = 24; _BTN_GAP = 4
                _MAT_W   = 180; _MARGIN = 8
                _ROW1_Y  = 4;  _ROW2_Y = _ROW1_Y + _CHIP_H + 2
                rx = W - _MAT_W - _MARGIN   # left edge of chip area
                # Row 1: material chip click (future: open mat picker)
                if _ROW1_Y <= my <= _ROW1_Y + _CHIP_H and rx <= mx <= rx + _MAT_W:
                    _ARW = 22
                    ws = self._find_workshop()
                    if ws:
                        if mx <= rx + _ARW:           # ◀ prev
                            ws._paint_cycle_mat(-1)
                        elif mx >= rx + _MAT_W - _ARW: # ▶ next
                            ws._paint_cycle_mat(+1)
                        else:                           # mat name chip — open list popup
                            ws._open_paint_mat_popup()
                    return
                # Row 2: tool buttons
                elif _ROW2_Y <= my <= _ROW2_Y + _CHIP_H:
                    ws = self._find_workshop()
                    # Each button occupies _BTN_W + _BTN_GAP
                    btn_idx = (mx - rx) // (_BTN_W + _BTN_GAP)
                    if   btn_idx == 0 and ws: ws._set_paint_tool('paint')
                    elif btn_idx == 1 and ws: ws._set_paint_tool('dropper')
                    elif btn_idx == 2 and ws: ws._set_paint_tool('fill')
                    elif btn_idx == 3 and ws: ws._undo_last_action()
                    elif btn_idx == 4 and ws: ws._save_file()
                    elif btn_idx == 5 and ws: ws._exit_paint_mode()
                    self.update(); return
            else:
                # Normal: Move [G] toggle
                if W-70 <= mx <= W-4 and 4 <= my <= 26:
                    self.toggle_gizmo_mode(); return

            # Paint mode — pick face, apply current tool
            if self._paint_mode:
                fi, face = self._pick_face(mx, my)
                if fi is not None and face is not None:
                    tool = getattr(self, '_tool_mode', 'paint')

                    if tool == 'dropper':
                        # Pick material from face → update paint colour + overlay
                        mat = face.material
                        picked = mat.material_id if hasattr(mat, 'material_id') else int(mat)
                        self._paint_material = picked
                        self._paint_material = picked   # update viewport attr
                        ws = self._find_workshop()
                        if ws:
                            combo = getattr(ws, 'paint_mat_combo', None)
                            if combo:
                                for i in range(combo.count()):
                                    if combo.itemData(i) == picked:
                                        combo.setCurrentIndex(i)
                                        break
                            ws._paint_active_mat = picked
                            # Sync _paint_mat_idx so ◀▶ arrows start from picked mat
                            lst = getattr(ws, '_paint_mat_list', [])
                            mat_ids = [m[0] for m in lst]
                            if picked in mat_ids:
                                ws._paint_mat_idx = mat_ids.index(picked)
                        # Auto-switch back to paint tool after dropper pick
                        self._tool_mode = 'paint'
                        self.update()   # refresh overlay with new colour
                        return

                    elif tool == 'fill':
                        # Fill all faces that share the same material as clicked face
                        mat = face.material
                        src_id = mat.material_id if hasattr(mat, 'material_id') else int(mat)
                        model = self._model
                        if model:
                            ws = self._find_workshop()
                            if ws:
                                models = getattr(getattr(ws, 'current_col_file', None), 'models', [])
                                mi = models.index(model) if model in models else -1
                                if mi >= 0:
                                    ws._push_undo(mi, f"Fill material {self._paint_material} from {src_id}")
                            count = 0
                            for f2 in model.faces:
                                m2 = f2.material
                                cur = m2.material_id if hasattr(m2, 'material_id') else int(m2)
                                if cur == src_id:
                                    if hasattr(m2, 'material_id'):
                                        m2.material_id = self._paint_material
                                    else:
                                        f2.material = self._paint_material
                                    count += 1
                            if ws:
                                ws._set_status(f"Filled {count} faces (mat {src_id} → {self._paint_material})")

                    else:  # paint
                        if hasattr(face, 'material'):
                            if hasattr(face.material, 'material_id'):
                                face.material.material_id = self._paint_material
                            else:
                                face.material = self._paint_material

                    self._selected_faces = {fi}
                    if self.on_face_selected:
                        self.on_face_selected(fi, face)
                    self.update()
                return

            # Normal mode — face select on click; start drag-select
            fi, face = self._pick_face(mx, my)
            if fi is not None:
                mods = event.modifiers()
                if mods & Qt.KeyboardModifier.ControlModifier:
                    # Ctrl+click: toggle individual face
                    if fi in self._selected_faces:
                        self._selected_faces.discard(fi)
                    else:
                        self._selected_faces.add(fi)
                else:
                    self._selected_faces = {fi}
                self._drag_selecting = True   # enable brush drag
                self.setCursor(Qt.CursorShape.CrossCursor)
                if self.on_face_selected:
                    self.on_face_selected(fi, face)
                self.update()
                return

            # Gizmo axis
            axis = self._hit_gizmo(mx, my)
            if axis:
                self._gizmo_drag  = axis
                self._gizmo_start = event.position()
                self.setCursor(Qt.CursorShape.SizeAllCursor)
            else:
                self._left_drag = event.position()
                self.setCursor(Qt.CursorShape.ClosedHandCursor)
        elif event.button() == Qt.MouseButton.RightButton:
            # Try to pick a face at click pos; if hit → context menu, else → rotate drag
            mx2, my2 = event.position().x(), event.position().y()
            fi2, face2 = self._pick_face(mx2, my2)
            if fi2 is not None and face2 is not None:
                # Select the face
                if not (event.modifiers() & Qt.KeyboardModifier.ControlModifier):
                    if fi2 not in self._selected_faces:
                        self._selected_faces = {fi2}
                else:
                    self._selected_faces.add(fi2)
                self.update()
                self._show_face_context_menu(event.globalPosition().toPoint(), fi2, face2)
            else:
                self._right_drag = event.position()
                self.setCursor(Qt.CursorShape.SizeAllCursor)
        elif event.button() == Qt.MouseButton.MiddleButton:
            self._mid_drag = event.position()
            self.setCursor(Qt.CursorShape.SizeAllCursor)  # rotate

    def mouseMoveEvent(self, event):
        import math

        # ── Gizmo drag ───────────────────────────────────────────────────
        if self._gizmo_drag and (event.buttons() & Qt.MouseButton.LeftButton):
            d  = event.position() - self._gizmo_start
            self._gizmo_start = event.position()
            axis = self._gizmo_drag
            scale, _, _ = self._get_scale_origin()
            ax3 = {'X':(1,0,0),'Y':(0,1,0),'Z':(0,0,1)}[axis]
            px, py = self._proj(*ax3)
            screen_len = math.hypot(px, py) or 1.0

            def _vec(obj):
                """Return the Vector3-like object itself if it has .x/.y/.z"""
                return obj if (obj and hasattr(obj,'x')) else None

            def _box_pts(box):
                """Yield the min and max Vector3 points of a box."""
                mn = getattr(box,'min_point', getattr(box,'min', None))
                mx = getattr(box,'max_point', getattr(box,'max', None))
                for pt in [mn, mx]:
                    if pt and hasattr(pt,'x'): yield pt

            if self._gizmo_mode == 'translate':
                dot = (d.x()*px + d.y()*py) / screen_len
                delta = dot / scale
                if self._model:
                    # Move vertices
                    for v in getattr(self._model, 'vertices', []):
                        if   axis=='X': v.x += delta
                        elif axis=='Y': v.y += delta
                        else:           v.z += delta
                    # Move box min/max points
                    for box in getattr(self._model, 'boxes', []):
                        for pt in _box_pts(box):
                            if   axis=='X': pt.x += delta
                            elif axis=='Y': pt.y += delta
                            else:           pt.z += delta
                    # Move sphere centres
                    for sph in getattr(self._model, 'spheres', []):
                        c = _vec(sph.center)
                        if c:
                            if   axis=='X': c.x += delta
                            elif axis=='Y': c.y += delta
                            else:           c.z += delta
                    # Move bounds centre
                    bounds = getattr(self._model, 'bounds', None)
                    if bounds:
                        for pt in [getattr(bounds,'center',None),
                                   getattr(bounds,'min',None),
                                   getattr(bounds,'max',None)]:
                            if pt and hasattr(pt,'x'):
                                if   axis=='X': pt.x += delta
                                elif axis=='Y': pt.y += delta
                                else:           pt.z += delta
            else:  # rotate
                perp_x, perp_y = -py, px
                deg = (d.x()*perp_x + d.y()*perp_y) / screen_len * 0.8
                r = math.radians(deg)
                cos_r, sin_r = math.cos(r), math.sin(r)
                if self._model:
                    def _rot_pt(pt):
                        if not (pt and hasattr(pt,'x')): return
                        x2,y2,z2 = pt.x, pt.y, pt.z
                        if axis=='X':
                            pt.y = y2*cos_r - z2*sin_r
                            pt.z = y2*sin_r + z2*cos_r
                        elif axis=='Y':
                            pt.x = x2*cos_r + z2*sin_r
                            pt.z = -x2*sin_r + z2*cos_r
                        else:
                            pt.x = x2*cos_r - y2*sin_r
                            pt.y = x2*sin_r + y2*cos_r
                    # Rotate vertices
                    for v in getattr(self._model, 'vertices', []):
                        _rot_pt(v)
                    # Rotate box min/max
                    for box in getattr(self._model, 'boxes', []):
                        for pt in _box_pts(box):
                            _rot_pt(pt)
                    # Rotate sphere centres
                    for sph in getattr(self._model, 'spheres', []):
                        _rot_pt(_vec(sph.center))
                    # Rotate bounds
                    bounds = getattr(self._model, 'bounds', None)
                    if bounds:
                        for attr in ('center','min','max'):
                            _rot_pt(getattr(bounds, attr, None))
            self.update()
            return

        # ── Pan (left drag on background) ────────────────────────────────
        # ── Drag-select (LMB held after face click — paint-brush selection) ──
        if self._drag_selecting and (event.buttons() & Qt.MouseButton.LeftButton):
            mx2, my2 = event.position().x(), event.position().y()
            fi, face = self._pick_face(mx2, my2)
            shift_held = bool(event.modifiers() & Qt.KeyboardModifier.ShiftModifier)
            if fi is not None and fi not in self._selected_faces:
                if self._paint_mode and not shift_held:
                    # Paint mode drag without shift: paint face
                    if hasattr(face, 'material'):
                        if hasattr(face.material, 'material_id'):
                            face.material.material_id = self._paint_material
                        else:
                            face.material = self._paint_material
                # Shift held OR not in paint mode: just add to selection
                self._selected_faces.add(fi)
                if self.on_face_selected and not shift_held:
                    self.on_face_selected(fi, face)
                self.update()

        elif self._left_drag and (event.buttons() & Qt.MouseButton.LeftButton):
            d = event.position() - self._left_drag
            self._pan_x += d.x(); self._pan_y += d.y()
            self._left_drag = event.position(); self.update()

        # ── Free rotate (right drag) ──────────────────────────────────────
        if self._right_drag and (event.buttons() & Qt.MouseButton.RightButton):
            d = event.position() - self._right_drag
            self._yaw   = (self._yaw + d.x() * 0.4) % 360
            self._pitch = max(-89.0, min(89.0, self._pitch + d.y() * 0.4))
            self._right_drag = event.position(); self.update()

        # ── Free rotate (middle drag) ─────────────────────────────────────
        if self._mid_drag and (event.buttons() & Qt.MouseButton.MiddleButton):
            d = event.position() - self._mid_drag
            self._yaw   = (self._yaw + d.x() * 0.4) % 360
            self._pitch = max(-89.0, min(89.0, self._pitch + d.y() * 0.4))
            self._mid_drag = event.position(); self.update()

    def mouseReleaseEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton:
            self._left_drag = None
            self._gizmo_drag = None
            self._drag_selecting = False
        elif event.button() == Qt.MouseButton.RightButton:
            self._right_drag = None
        elif event.button() == Qt.MouseButton.MiddleButton:
            self._mid_drag = None
        # Restore cursor — cross if still in paint mode, else arrow
        self.setCursor(
            Qt.CursorShape.CrossCursor if self._paint_mode
            else Qt.CursorShape.ArrowCursor
        )

    def resizeEvent(self, event):
        super().resizeEvent(event)
        ws = self._find_workshop()
        if ws and hasattr(ws, 'paint_toolbar') and ws.paint_toolbar and ws.paint_toolbar.isVisible():
            vp = getattr(ws, 'preview_widget', None)
            if vp: ws.paint_toolbar.setGeometry(0, 0, vp.width(), 34)

    def wheelEvent(self, event):
        factor = 1.18 if event.angleDelta().y() > 0 else 1/1.18
        self._zoom = max(0.02, min(40.0, self._zoom * factor))
        self.update()

    def keyPressEvent(self, event):
        if event.key() == Qt.Key.Key_Escape:
            if self._paint_mode:
                self.set_paint_mode(False)
                # notify workshop via _workshop_ref (reliable; parent() may be a QFrame)
                ws = self._find_workshop()
                if ws and hasattr(ws, '_on_paint_mode_exited'):
                    ws._on_paint_mode_exited()
            self._selected_faces = set()
            self.update()
        elif event.key() == Qt.Key.Key_G: self._set_gizmo('translate')
        elif event.key() == Qt.Key.Key_F and self._paint_mode:
            # F = fill selected faces with current paint material
            ws = self._find_workshop()
            if ws: ws._apply_to_selected_faces_paint()
        elif event.key() == Qt.Key.Key_R: self._set_gizmo('rotate')
        elif event.key() == Qt.Key.Key_F: self.fit_to_window()
        elif event.key() == Qt.Key.Key_V: self._cycle_render_style()
        else: super().keyPressEvent(event)

    def _cycle_render_style(self):
        modes = ['wireframe','semi','solid']
        self._render_style = modes[(modes.index(self._render_style)+1) % 3]                              if self._render_style in modes else 'semi'
        self.update()

    def contextMenuEvent(self, event):
        from PyQt6.QtWidgets import QMenu
        m = QMenu(self)
        m.addAction("Top",       lambda: self._set_angles(0,   0))
        m.addAction("Front",     lambda: self._set_angles(0,  90))
        m.addAction("Side",      lambda: self._set_angles(90,  0))
        m.addAction("Isometric", lambda: self._set_angles(45, 35))
        m.addSeparator()
        m.addAction("Reset View",    self.reset_view)
        m.addAction("Fit to Window", self.fit_to_window)
        m.addSeparator()
        m.addAction("Move Gizmo  [G]",   lambda: self._set_gizmo('translate'))
        m.addAction("Rotate Gizmo [R]",  lambda: self._set_gizmo('rotate'))
        m.addSeparator()
        for style,label in [('wireframe','Wireframe [V]'),
                             ('semi',     'Semi-transparent [V]'),
                             ('solid',    'Solid [V]')]:
            tick = '✓ ' if self._render_style == style else '    '
            m.addAction(tick+label, lambda s=style: self.set_render_style(s))
        m.exec(event.globalPos())

    def _set_angles(self, yaw, pitch):
        self._yaw, self._pitch = float(yaw), float(pitch); self.update()


    # ── paint ─────────────────────────────────────────────────────────────
    def paintEvent(self, event):
        """Fully self-contained paint — grid, mesh, boxes, spheres, bounds, gizmo, HUD."""
        from PyQt6.QtGui import (QPainter, QColor, QFont, QPen, QBrush,
                                  QPolygonF, QLinearGradient)
        from PyQt6.QtCore import QPointF, QRectF
        import math

        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        W, H = self.width(), self.height()
        r2, g2, b2 = self._bg_color
        p.fillRect(self.rect(), QColor(r2, g2, b2))

        if not self._model:
            p.setPen(QColor(120, 120, 120))
            p.setFont(QFont('Arial', 11))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No model selected")
            return

        scale, ox, oy = self._get_scale_origin()

        def to_screen(x, y, z):
            px, py = self._proj(x, y, z)
            return px * scale + ox, py * scale + oy

        def g3(obj):
            if hasattr(obj, 'x'):        return obj.x, obj.y, obj.z
            if hasattr(obj, 'position'): return obj.position.x, obj.position.y, obj.position.z
            if obj is None:              return 0.0, 0.0, 0.0
            return float(obj[0]), float(obj[1]), float(obj[2])

        model   = self._model
        verts   = getattr(model, 'vertices', [])
        faces   = getattr(model, 'faces',   [])
        boxes   = getattr(model, 'boxes',   [])
        spheres = getattr(model, 'spheres', [])
        bounds  = getattr(model, 'bounds',  None)

        # ── Extent from ALL geometry (verts + boxes + spheres) ───────────
        all_pts = [(v.x, v.y, v.z) for v in verts]
        for box in boxes:
            mn = getattr(box,'min_point', getattr(box,'min', None))
            mx = getattr(box,'max_point', getattr(box,'max', None))
            if mn: all_pts.append(g3(mn))
            if mx: all_pts.append(g3(mx))
        for sph in spheres:
            cx,cy3,cz = g3(getattr(sph,'center',None))
            r = getattr(sph,'radius',1.0)
            all_pts += [(cx+r,cy3,cz),(cx-r,cy3,cz),(cx,cy3+r,cz),(cx,cy3-r,cz)]
        if bounds:
            for attr in ('min','max'):
                pt = getattr(bounds, attr, None)
                if pt: all_pts.append(g3(pt))

        if all_pts:
            extent = max(max(abs(c) for pt in all_pts for c in pt), 1.0)
        else:
            extent = 5.0

        # ── Reference grid (XY plane, Z=0) ───────────────────────────────
        raw_step = extent / 4.0
        mag  = 10 ** math.floor(math.log10(max(raw_step, 0.001)))
        step = round(raw_step / mag) * mag; step = max(step, 0.01)
        half = math.ceil(extent / step + 1) * step
        n    = int(half / step)
        p.setRenderHint(QPainter.RenderHint.Antialiasing, False)
        for i in range(-n, n + 1):
            v2 = i * step
            col = QColor(75, 80, 105) if i == 0 else QColor(50, 55, 72)
            p.setPen(QPen(col, 1))
            x0,y0 = to_screen(-half, v2, 0); x1,y1 = to_screen(half, v2, 0)
            p.drawLine(int(x0), int(y0), int(x1), int(y1))
            x0,y0 = to_screen(v2, -half, 0); x1,y1 = to_screen(v2, half, 0)
            p.drawLine(int(x0), int(y0), int(x1), int(y1))
        p.setRenderHint(QPainter.RenderHint.Antialiasing, True)

        # ── Material colours — from col_materials group palette ───────────
        try:
            from apps.methods.col_materials import get_material_qcolor, COLGame
            _game = COLGame.VC if getattr(getattr(model,'version',None),'value',3)==1 else COLGame.SA
            def mat_col(mat_id):
                c = get_material_qcolor(mat_id, _game)
                return c if c else QColor(120,120,120)
        except Exception:
            def mat_col(mat_id):
                return QColor(120,120,120)

        # ── Mesh faces ────────────────────────────────────────────────────
        rs = self._render_style  # 'wireframe' | 'semi' | 'solid'
        if self._show_mesh and verts and faces:
            for face_idx, face in enumerate(faces):
                idx = getattr(face,'vertex_indices',None)
                if idx is None:
                    fa = getattr(face,'a',None)
                    if fa is not None: idx=(fa,face.b,face.c)
                if not idx or len(idx)!=3: continue
                try:
                    pts=[QPointF(*to_screen(*g3(verts[i]))) for i in idx]
                except (IndexError,AttributeError): continue
                _mat = getattr(face,'material',0)
                _mat_id = getattr(_mat,'material_id',_mat) if not isinstance(_mat,int) else _mat
                mc = mat_col(_mat_id)
                is_selected = (face_idx in self._selected_faces)
                if is_selected:
                    p.setBrush(QBrush(QColor(255, 200, 50, 200)))
                    p.setPen(QPen(QColor(255, 230, 80), 2))
                elif rs == 'solid':
                    p.setBrush(QBrush(mc))
                    p.setPen(QPen(mc.darker(130),0.5))
                elif rs == 'semi':
                    fill=QColor(mc.red(),mc.green(),mc.blue(),90)
                    p.setBrush(QBrush(fill))
                    p.setPen(QPen(QColor(mc.red()//2+60,mc.green()//2+60,mc.blue()//2+60),0.5))
                else:  # wireframe
                    p.setBrush(Qt.BrushStyle.NoBrush)
                    p.setPen(QPen(QColor(100,180,100),1))
                p.drawPolygon(QPolygonF(pts))

        # ── Boxes — draw all 12 edges of AABB ─────────────────────────────
        if self._show_boxes:
            p.setPen(QPen(QColor(220,180,50),1.5))
            p.setBrush(QBrush(QColor(220,180,50,30)) if rs!='wireframe' else Qt.BrushStyle.NoBrush)
            for box in boxes:
                mn_obj = getattr(box,'min_point',getattr(box,'min',None))
                mx_obj = getattr(box,'max_point',getattr(box,'max',None))
                if mn_obj is None or mx_obj is None: continue
                x0,y0,z0 = g3(mn_obj)
                x1,y1,z1 = g3(mx_obj)
                # 8 corners
                corners=[(xa,ya,za) for xa in(x0,x1) for ya in(y0,y1) for za in(z0,z1)]
                sc=[to_screen(*c) for c in corners]
                # 12 edges of the cube
                edges=[(0,1),(0,2),(0,4),(1,3),(1,5),(2,3),(2,6),(3,7),(4,5),(4,6),(5,7),(6,7)]
                for a2,b2 in edges:
                    ax,ay=sc[a2]; bx,by=sc[b2]
                    p.drawLine(int(ax),int(ay),int(bx),int(by))

        # ── Spheres — draw 3 projected rings (equator + 2 meridians) ──────
        if self._show_spheres:
            p.setPen(QPen(QColor(80,200,220),1.5))
            p.setBrush(QBrush(QColor(80,200,220,25)) if rs!='wireframe' else Qt.BrushStyle.NoBrush)
            N = 48
            for sph in spheres:
                cx,cy3,cz = g3(getattr(sph,'center',sph))
                r = getattr(sph,'radius',1.0)
                # 3 rings in different planes
                for t1,t2,t3 in [(1,0,0,),(0,1,0),(0,0,1)]:
                    # tangent vectors from axis (t1,t2,t3)
                    if t3: ta,tb = (1,0,0),(0,1,0)
                    elif t2: ta,tb = (1,0,0),(0,0,1)
                    else: ta,tb = (0,1,0),(0,0,1)
                    pts=[]
                    for i in range(N+1):
                        a2=2*math.pi*i/N
                        wx=cx+r*(math.cos(a2)*ta[0]+math.sin(a2)*tb[0])
                        wy=cy3+r*(math.cos(a2)*ta[1]+math.sin(a2)*tb[1])
                        wz=cz+r*(math.cos(a2)*ta[2]+math.sin(a2)*tb[2])
                        pts.append(QPointF(*to_screen(wx,wy,wz)))
                    for i in range(len(pts)-1):
                        p.drawLine(pts[i],pts[i+1])

        # ── Bounding box (model.bounds) ───────────────────────────────────
        if bounds:
            mn_obj=getattr(bounds,'min',None); mx_obj=getattr(bounds,'max',None)
            if mn_obj and mx_obj:
                x0,y0,z0=g3(mn_obj); x1,y1,z1=g3(mx_obj)
                corners=[(xa,ya,za) for xa in(x0,x1) for ya in(y0,y1) for za in(z0,z1)]
                sc=[to_screen(*c) for c in corners]
                edges=[(0,1),(0,2),(0,4),(1,3),(1,5),(2,3),(2,6),(3,7),(4,5),(4,6),(5,7),(6,7)]
                p.setPen(QPen(QColor(180,100,220,160),1,Qt.PenStyle.DashLine))
                p.setBrush(Qt.BrushStyle.NoBrush)
                for a2,b2 in edges:
                    ax,ay=sc[a2]; bx,by=sc[b2]
                    p.drawLine(int(ax),int(ay),int(bx),int(by))
            # Bounding sphere
            bc=getattr(bounds,'center',None); br=getattr(bounds,'radius',0)
            if bc and br>0:
                cx,cy3,cz=g3(bc)
                ta,tb=(1,0,0),(0,1,0)
                pts=[]
                for i in range(49):
                    a2=2*math.pi*i/48
                    wx=cx+br*(math.cos(a2)*ta[0]+math.sin(a2)*tb[0])
                    wy=cy3+br*(math.cos(a2)*ta[1]+math.sin(a2)*tb[1])
                    wz=cz+br*(math.cos(a2)*ta[2]+math.sin(a2)*tb[2])
                    pts.append(QPointF(*to_screen(wx,wy,wz)))
                p.setPen(QPen(QColor(180,100,220,120),1,Qt.PenStyle.DotLine))
                for i in range(len(pts)-1): p.drawLine(pts[i],pts[i+1])

        # ── Gizmo at model centroid ───────────────────────────────────────
        if all_pts:
            cx3=sum(pt[0] for pt in all_pts)/len(all_pts)
            cy3=sum(pt[1] for pt in all_pts)/len(all_pts)
            cz3=sum(pt[2] for pt in all_pts)/len(all_pts)
        else:
            cx3=cy3=cz3=0.0
        gx,gy=to_screen(cx3,cy3,cz3)
        arm=max(45,min(W,H)*0.15)
        axes=[((1,0,0),QColor(220,60,60),'X'),((0,1,0),QColor(60,200,60),'Y'),((0,0,1),QColor(60,120,220),'Z')]
        sorted_axes=sorted(axes,key=lambda a:self._proj(*a[0])[1],reverse=True)
        if self._gizmo_mode=='translate':
            for (dx,dy,dz),color,label in sorted_axes:
                px2,py2=self._proj(dx,dy,dz)
                tx,ty=gx+px2*arm,gy+py2*arm
                p.setPen(QPen(color,2)); p.drawLine(int(gx),int(gy),int(tx),int(ty))
                ang=math.atan2(ty-gy,tx-gx); aw,ah=12,6
                tip=QPointF(tx,ty)
                lpt=QPointF(tx-aw*math.cos(ang)+ah*math.sin(ang),ty-aw*math.sin(ang)-ah*math.cos(ang))
                rpt=QPointF(tx-aw*math.cos(ang)-ah*math.sin(ang),ty-aw*math.sin(ang)+ah*math.cos(ang))
                p.setBrush(QBrush(color)); p.setPen(QPen(color,1))
                p.drawPolygon(QPolygonF([tip,lpt,rpt]))
                lx=tx+(9 if tx>=gx else -14); ly=ty+(5 if ty>=gy else -3)
                p.setFont(QFont('Arial',8,QFont.Weight.Bold)); p.setPen(color)
                p.drawText(int(lx),int(ly),label)
        else:
            N=64
            rings=[((1,0,0),(0,1,0),(0,0,1),QColor(220,60,60),'X'),
                   ((0,1,0),(1,0,0),(0,0,1),QColor(60,200,60),'Y'),
                   ((0,0,1),(1,0,0),(0,1,0),QColor(60,120,220),'Z')]
            for (_,t1,t2,color,label) in sorted(rings,key=lambda r:self._proj(*r[0])[1],reverse=True):
                t1x,t1y,t1z=t1; t2x,t2y,t2z=t2
                pts=[]
                for i in range(N+1):
                    a2=2*math.pi*i/N
                    wx=math.cos(a2)*t1x+math.sin(a2)*t2x
                    wy=math.cos(a2)*t1y+math.sin(a2)*t2y
                    wz=math.cos(a2)*t1z+math.sin(a2)*t2z
                    px2,py2=self._proj(wx,wy,wz)
                    pts.append(QPointF(gx+px2*arm,gy+py2*arm))
                p.setPen(QPen(color,2)); p.setBrush(Qt.BrushStyle.NoBrush)
                for i in range(len(pts)-1): p.drawLine(pts[i],pts[i+1])
                p45x=math.cos(math.pi/4)*t1x+math.sin(math.pi/4)*t2x
                p45y=math.cos(math.pi/4)*t1y+math.sin(math.pi/4)*t2y
                p45z=math.cos(math.pi/4)*t1z+math.sin(math.pi/4)*t2z
                lp,lq=self._proj(p45x,p45y,p45z)
                p.setFont(QFont('Arial',8,QFont.Weight.Bold)); p.setPen(color)
                p.drawText(int(gx+lp*arm+(6 if lp>=0 else -12)),int(gy+lq*arm+(5 if lq>=0 else -3)),label)
        p.setBrush(QBrush(QColor(230,230,230))); p.setPen(QPen(QColor(160,160,160),1))
        p.drawEllipse(int(gx)-5,int(gy)-5,10,10)

        # ── Top-right overlay — normal mode: Move/Rotate + Render chips ──
        #                         paint mode:  material + tool chips
        if self._paint_mode:
            # ── Tunable layout constants ────────────────────────────────
            _CHIP_H   = 26   # height of each chip row  (px)
            _BTN_W    = 32   # width  of each tool button
            _BTN_GAP  = 3    # gap between buttons
            _ICON_SZ  = 18   # SVG icon size inside button
            _MAT_W    = 200  # width of the material name chip
            _MARGIN   = 8    # right margin from viewport edge
            _ROW1_Y   = 4    # y of material chip
            _ROW2_Y   = _ROW1_Y + _CHIP_H + 2   # y of tool buttons row
            # ────────────────────────────────────────────────────────────

            from PyQt6.QtCore import QRect
            mat_id = self._paint_material
            # Use cached list if available, else fall back to direct lookup
            _mat_cache = getattr(self._find_workshop(), '_paint_mat_list', [])                          if self._find_workshop() else []
            _mat_entry = next((m for m in _mat_cache if m[0] == mat_id), None)
            if _mat_entry:
                _, mat_name, hex_col = _mat_entry
            else:
                from apps.methods.col_materials import get_material_name, get_material_colour, COLGame
                mat_name = get_material_name(mat_id, COLGame.SA)
                hex_col  = get_material_colour(mat_id, COLGame.SA)
            mc = QColor(f"#{hex_col}")

            # Row 1: [◀] [■ mat swatch | id — name | ▶]
            _ARW = 22   # arrow button width
            rx = W - _MAT_W - _MARGIN
            # ◀ prev button
            p.setBrush(QBrush(QColor(30,30,50,210))); p.setPen(QPen(QColor(180,120,0),1))
            p.drawRoundedRect(rx, _ROW1_Y, _ARW, _CHIP_H, 3, 3)
            p.setPen(QColor(255,180,0)); p.setFont(QFont('Arial',10,QFont.Weight.Bold))
            p.drawText(rx+5, _ROW1_Y+17, "◀")
            # material name chip
            p.setBrush(QBrush(QColor(20,20,40,220))); p.setPen(QPen(QColor(255,140,0),1))
            p.drawRoundedRect(rx+_ARW+2, _ROW1_Y, _MAT_W-_ARW*2-4, _CHIP_H, 4, 4)
            p.setBrush(QBrush(mc)); p.setPen(Qt.PenStyle.NoPen)
            p.drawRoundedRect(rx+_ARW+6, _ROW1_Y+4, _CHIP_H-8, _CHIP_H-8, 2, 2)
            p.setPen(QColor(255,200,80)); p.setFont(QFont('Arial',8,QFont.Weight.Bold))
            p.drawText(rx+_ARW+_CHIP_H+4, _ROW1_Y+17, f"{mat_id} — {mat_name[:20]}")
            # ▶ next button
            p.setBrush(QBrush(QColor(30,30,50,210))); p.setPen(QPen(QColor(180,120,0),1))
            p.drawRoundedRect(rx+_MAT_W-_ARW, _ROW1_Y, _ARW, _CHIP_H, 3, 3)
            p.setPen(QColor(255,180,0)); p.setFont(QFont('Arial',10,QFont.Weight.Bold))
            p.drawText(rx+_MAT_W-_ARW+4, _ROW1_Y+17, "▶")

            # Row 2: tool buttons using SVG icons via QIcon.paint()
            tool = getattr(self, '_tool_mode', 'paint')
            tx = W - _MAT_W - _MARGIN
            ws = self._find_workshop()
            icon_fac = getattr(ws, 'icon_factory', None) if ws else None

            tool_defs = [
                ('paint',   'paint_icon',   '#ff8c00'),   # orange when active
                ('dropper', 'dropper_icon', '#4fc3f7'),   # blue
                ('fill',    'fill_icon',    '#a5d6a7'),   # green
            ]
            for t_name, icon_fn, active_col in tool_defs:
                active = (tool == t_name)
                bg  = QColor(active_col) if active else QColor(30,30,50,210)
                bdr = QColor(active_col) if active else QColor(80,90,130)
                p.setBrush(QBrush(bg)); p.setPen(QPen(bdr, 1))
                p.drawRoundedRect(tx, _ROW2_Y, _BTN_W, _CHIP_H, 3, 3)
                # Draw SVG icon centred in button
                if icon_fac:
                    icon_col = '#000000' if active else active_col
                    try:
                        icon = getattr(icon_fac, icon_fn)(color=icon_col)
                        icon_x = tx + (_BTN_W - _ICON_SZ) // 2
                        icon_y = _ROW2_Y + (_CHIP_H - _ICON_SZ) // 2
                        icon.paint(p, QRect(icon_x, icon_y, _ICON_SZ, _ICON_SZ))
                    except Exception:
                        pass
                tx += _BTN_W + _BTN_GAP

            # Tool name label below row 2 (acts as tooltip)
            tool_labels = {'paint':'Paint', 'dropper':'Pick', 'fill':'Fill'}
            p.setPen(QColor(200,200,160,180)); p.setFont(QFont('Arial',7))
            p.drawText(W - _MAT_W - _MARGIN, _ROW2_Y + _CHIP_H + 10,
                       f"Tool: {tool_labels.get(tool, tool)}")

            # Undo button
            p.setBrush(QBrush(QColor(30,30,50,210))); p.setPen(QPen(QColor(80,90,130),1))
            p.drawRoundedRect(tx, _ROW2_Y, _BTN_W, _CHIP_H, 3, 3)
            if icon_fac:
                try:
                    icon = icon_fac.undo_paint_icon(color='#b0bec5')
                    icon_x = tx + (_BTN_W - _ICON_SZ)//2
                    icon_y = _ROW2_Y + (_CHIP_H - _ICON_SZ)//2
                    icon.paint(p, QRect(icon_x, icon_y, _ICON_SZ, _ICON_SZ))
                except Exception:
                    p.setPen(QColor(180,200,255)); p.setFont(QFont('Arial',9))
                    p.drawText(tx+6, _ROW2_Y+15, "↩")
            tx += _BTN_W + _BTN_GAP

            # Save button (between undo and exit)
            p.setBrush(QBrush(QColor(20,50,30,220))); p.setPen(QPen(QColor(80,200,100),1))
            p.drawRoundedRect(tx, _ROW2_Y, _BTN_W, _CHIP_H, 3, 3)
            if icon_fac:
                try:
                    icon = icon_fac.save_icon(color='#66bb6a')
                    icon_x = tx + (_BTN_W - _ICON_SZ)//2
                    icon_y = _ROW2_Y + (_CHIP_H - _ICON_SZ)//2
                    icon.paint(p, QRect(icon_x, icon_y, _ICON_SZ, _ICON_SZ))
                except Exception:
                    p.setPen(QColor(100,220,120)); p.setFont(QFont('Arial',8,QFont.Weight.Bold))
                    p.drawText(tx+5, _ROW2_Y+15, "S")
            tx += _BTN_W + _BTN_GAP

            # Exit button (✕)
            p.setBrush(QBrush(QColor(30,30,50,210))); p.setPen(QPen(QColor(200,80,60),1))
            p.drawRoundedRect(tx, _ROW2_Y, _BTN_W, _CHIP_H, 3, 3)
            if icon_fac:
                try:
                    icon = icon_fac.close_icon(color='#ef5350')
                    icon_x = tx + (_BTN_W - _ICON_SZ)//2
                    icon_y = _ROW2_Y + (_CHIP_H - _ICON_SZ)//2
                    icon.paint(p, QRect(icon_x, icon_y, _ICON_SZ, _ICON_SZ))
                except Exception:
                    p.setPen(QColor(255,100,80)); p.setFont(QFont('Arial',9,QFont.Weight.Bold))
                    p.drawText(tx+7, _ROW2_Y+15, "x")
        else:
            bx,by,bw,bh=W-70,4,66,22
            p.setBrush(QBrush(QColor(40,44,62))); p.setPen(QPen(QColor(80,90,130),1))
            p.drawRoundedRect(bx,by,bw,bh,4,4)
            p.setFont(QFont('Arial',8)); p.setPen(QColor(200,200,220))
            lbl='↕ Move [G]' if self._gizmo_mode=='translate' else '↻ Rotate [R]'
            p.drawText(bx+4,by+15,lbl)
            mode_lbl={'wireframe':'Wire','semi':'Semi','solid':'Solid'}.get(rs,'?')
            mode_col={'wireframe':QColor(100,180,100),'semi':QColor(180,180,100),'solid':QColor(100,140,220)}.get(rs,QColor(180,180,180))
            p.setBrush(QBrush(QColor(40,44,62))); p.setPen(QPen(mode_col,1))
            p.drawRoundedRect(W-70,28,66,18,3,3)
            p.setPen(mode_col); p.setFont(QFont('Arial',7))
            p.drawText(W-66,41,f"[V] {mode_lbl}")

        # ── HUD ───────────────────────────────────────────────────────────
        p.setFont(QFont('Arial',8)); p.setPen(QColor(200,200,200))
        p.drawText(6,14,getattr(model,'name','') or '')
        y2=H-54
        for col_c,txt in [(QColor(100,180,100),f"Mesh  F:{len(faces)} V:{len(verts)}"),
                          (QColor(220,180,50), f"Boxes  {len(boxes)}"),
                          (QColor(80,200,220), f"Spheres  {len(spheres)}")]:
            p.setPen(col_c); p.drawText(6,y2,txt); y2+=14
        p.setPen(QColor(120,125,140)); p.setFont(QFont('Arial',7))
        p.drawText(6,H-4,f"Y:{self._yaw:.0f}° P:{self._pitch:.0f}° Z:{self._zoom:.2f}x")

        # Paint mode indicator now shown in paint_toolbar above viewport (not drawn here)
        p.drawText(W-68,H-4,f"grid {step:.3g}")


    def _apply_to_selected_faces(self):
        vp = self.preview_widget
        model = self._get_selected_model()

        if not vp or not model:
            return

        sel = sorted(getattr(vp, '_selected_faces', []))
        if not sel:
            return

        mat_id = self._paint_active_mat

        for fi in sel:
            if fi < len(model.faces):
                f = model.faces[fi]
                if isinstance(f.material, int):
                    f.material = mat_id
                else:
                    f.material.material_id = mat_id

        vp.update()
        self._set_status(f"Applied material {mat_id} to {len(sel)} faces")


    def _apply_to_all_faces(self):
        model = self._get_selected_model()
        if not model:
            return

        mat_id = self._paint_active_mat

        for f in model.faces:
            if isinstance(f.material, int):
                f.material = mat_id
            else:
                f.material.material_id = mat_id

        self.preview_widget.update()
        self._set_status(f"Applied material {mat_id} to all faces")


    def _show_face_context_menu(self, global_pos, face_index, face): #vers 1
        """Right-click context menu for a picked face — material operations."""
        from PyQt6.QtWidgets import QMenu  # QAction imported at module level
        from PyQt6.QtGui import QColor, QPixmap, QIcon
        from PyQt6.QtCore import Qt as _Qt

        ws = self._find_workshop()

        # Resolve material
        mat = face.material
        mat_id = mat.material_id if hasattr(mat, 'material_id') else int(mat)

        try:
            from apps.methods.col_materials import (
                get_material_name, get_material_colour, COLGame)
            model = self._model
            ver = getattr(getattr(model, 'version', None), 'value', 3) if model else 3
            game = COLGame.VC if ver == 1 else COLGame.SA
            mat_name = get_material_name(mat_id, game)
            hex_col  = get_material_colour(mat_id, game)
        except Exception:
            mat_name = f"Material {mat_id}"
            hex_col  = "808080"

        # ── Build menu ───────────────────────────────────────────────────
        menu = QMenu(self)

        # Header — material info with colour swatch
        px = QPixmap(14, 14)
        px.fill(QColor(f"#{hex_col}"))
        info_act = QAction(QIcon(px),
            f"  Face {face_index}:  {mat_id} — {mat_name}", self)
        info_act.setEnabled(False)
        menu.addAction(info_act)
        menu.addSeparator()

        # Copy material
        act_copy = menu.addAction("Copy material")
        # Paste material (only if clipboard has one)
        _clip = getattr(ws, '_mat_clipboard', None) if ws else None
        act_paste = menu.addAction(
            f"Paste material  ({_clip})" if _clip is not None
            else "Paste material")
        act_paste.setEnabled(_clip is not None)
        menu.addSeparator()

        # Apply to selection
        n_sel = len(self._selected_faces)
        sel_label = (f"✓  Apply to {n_sel} selected face(s)"
                     if n_sel > 1 else "✓  Apply to selection")
        act_apply_sel = menu.addAction(sel_label)
        act_apply_sel.setEnabled(n_sel > 1)

        # Clear material on this face (→ material 0)
        act_clear_face = menu.addAction("✕  Clear material on this face")
        # Clear material on ALL faces in model
        model = self._model
        n_faces = len(getattr(model, 'faces', []))
        act_clear_all = menu.addAction(
            f"✕✕  Clear material on all {n_faces} faces")

        menu.addSeparator()
        # Open full paint editor
        act_paint = menu.addAction("Paint — open material editor…")

        # ── Execute ──────────────────────────────────────────────────────
        chosen = menu.exec(global_pos)
        if chosen is None:
            return

        if chosen == act_copy:
            if ws:
                ws._mat_clipboard = mat_id
                if hasattr(ws, '_set_status'):
                    ws._set_status(f"Copied material {mat_id} — {mat_name}")

        elif chosen == act_paste and _clip is not None:
            if ws and model:
                models = getattr(getattr(ws, 'current_col_file', None), 'models', [])
                mi = models.index(model) if model in models else -1
                if mi >= 0 and hasattr(ws, '_push_undo'):
                    ws._push_undo(mi, f"Paste material {_clip} → face {face_index}")
            if hasattr(mat, 'material_id'):
                mat.material_id = _clip
            else:
                face.material = _clip
            self.update()
            if ws and hasattr(ws, '_set_status'):
                ws._set_status(f"Pasted material {_clip} → face {face_index}")

        elif chosen == act_apply_sel:
            if ws and model:
                models = getattr(getattr(ws, 'current_col_file', None), 'models', [])
                mi = models.index(model) if model in models else -1
                if mi >= 0 and hasattr(ws, '_push_undo'):
                    ws._push_undo(mi, f"Apply material {mat_id} → {n_sel} faces")
            for fi in self._selected_faces:
                if fi < len(model.faces):
                    f2 = model.faces[fi]
                    m2 = f2.material
                    if hasattr(m2, 'material_id'):
                        m2.material_id = mat_id
                    else:
                        f2.material = mat_id
            self.update()
            if ws and hasattr(ws, '_set_status'):
                ws._set_status(f"Applied material {mat_id} to {n_sel} faces")

        elif chosen == act_clear_face:
            if ws and model:
                models = getattr(getattr(ws, 'current_col_file', None), 'models', [])
                mi = models.index(model) if model in models else -1
                if mi >= 0 and hasattr(ws, '_push_undo'):
                    ws._push_undo(mi, f"Clear material → face {face_index}")
            if hasattr(mat, 'material_id'):
                mat.material_id = 0
            else:
                face.material = 0
            self.update()
            if ws and hasattr(ws, '_set_status'):
                ws._set_status(f"Cleared material on face {face_index}")

        elif chosen == act_clear_all:
            if model:
                if ws:
                    models = getattr(getattr(ws, 'current_col_file', None), 'models', [])
                    mi = models.index(model) if model in models else -1
                    if mi >= 0 and hasattr(ws, '_push_undo'):
                        ws._push_undo(mi, "Clear all face materials")
                for f2 in model.faces:
                    m2 = f2.material
                    if hasattr(m2, 'material_id'):
                        m2.material_id = 0
                    else:
                        f2.material = 0
                self.update()
                if ws and hasattr(ws, '_set_status'):
                    ws._set_status(f"Cleared material on all {n_faces} faces")

        elif chosen == act_paint:
            if ws and hasattr(ws, '_open_paint_editor'):
                ws._open_paint_editor()

    def _find_workshop(self):
        ref = getattr(self, '_workshop_ref', None)
        if ref is not None: return ref
        p = self.parent()
        while p:
            if isinstance(p, COLWorkshop): return p
            p = p.parent() if callable(getattr(p, 'parent', None)) else None
        return None
VIEWPORT_AVAILABLE = True

# Add root directory to path
App_name = "Col Workshop"
DEBUG_STANDALONE = False

# Import AppSettings
try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    print("Warning: AppSettings not available")


class COLModelListWidget(QListWidget): #vers 1
    """Enhanced model list widget"""

    model_selected = pyqtSignal(int)  # Model index
    model_context_menu = pyqtSignal(int, object)  # Model index, position

    def __init__(self, parent=None):
        self.icon_factory = SVGIconFactory()
        super().__init__(parent)
        self.current_file = None

        # Enable context menu
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self.show_context_menu)

        # Connect selection
        self.currentRowChanged.connect(self.on_selection_changed)


    def populate_models(self): #vers 1
        """Populate model list"""
        self.clear()

        if not self.current_file or not hasattr(self.current_file, 'models'):
            return

        for i, model in enumerate(self.current_file.models):
            name = getattr(model, 'name', f'Model_{i}')
            version = getattr(model, 'version', COLVersion.COL_1)

            # Count collision elements
            spheres = len(getattr(model, 'spheres', []))
            boxes = len(getattr(model, 'boxes', []))
            faces = len(getattr(model, 'faces', []))

            item_text = f"{name} ({version.name} - S:{spheres} B:{boxes} F:{faces})"

            item = QListWidgetItem(item_text)
            item.setData(Qt.ItemDataRole.UserRole, i)  # Store model index
            self.addItem(item)


    def on_selection_changed(self, row): #vers 1
        """Handle selection change"""
        if row >= 0:
            self.model_selected.emit(row)


    def show_context_menu(self, position): #vers 1
        """Show context menu"""
        item = self.itemAt(position)
        if item:
            model_index = item.data(Qt.ItemDataRole.UserRole)
            self.model_context_menu.emit(model_index, self.mapToGlobal(position))




class _ColListDelegate(QStyledItemDelegate): #vers 1
    """Word-wrapping delegate for the COL compact list Details column."""
    def paint(self, painter, option, index):
        if index.column() != 1:
            super().paint(painter, option, index)
            return
        from PyQt6.QtWidgets import QStyle, QApplication
        from PyQt6.QtCore import Qt
        # Background
        QApplication.style().drawPrimitive(
            QStyle.PrimitiveElement.PE_PanelItemViewItem, option, painter)
        text = index.data(Qt.ItemDataRole.DisplayRole) or ''
        painter.save()
        painter.setClipRect(option.rect)
        r = option.rect.adjusted(4, 4, -4, -4)
        painter.setPen(option.palette.text().color())
        painter.setFont(option.font)
        painter.drawText(r, Qt.TextFlag.TextWordWrap | Qt.AlignmentFlag.AlignTop, text)
        painter.restore()

    def sizeHint(self, option, index):
        if index.column() != 1:
            return super().sizeHint(option, index)
        from PyQt6.QtCore import Qt, QSize
        text = index.data(Qt.ItemDataRole.DisplayRole) or ''
        fm = option.fontMetrics
        # Measure the text in a column ~160px wide
        w = 160
        r = fm.boundingRect(0, 0, w, 9999,
            Qt.TextFlag.TextWordWrap | Qt.AlignmentFlag.AlignTop, text)
        return QSize(w, max(72, r.height() + 12))

class COLWorkshop(ToolMenuMixin, QWidget): #vers 4
    """COL Workshop - Main window"""

    # ── ToolMenuMixin implementation ─────────────────────────────────────

    def get_menu_title(self) -> str: #vers 1
        """Return menu label for imgfactory menu bar."""
        return "COL Workshop"

    def _build_menus_into_qmenu(self, parent_menu): #vers 1
        """Populate parent_menu with COL Workshop actions."""
        # File
        fm = parent_menu.addMenu("File")
        fm.addAction("Open COL…",        self._open_file)
        fm.addAction("Save COL",         self._save_file)
        fm.addAction("Save COL As…",     self._save_file_as)
        fm.addSeparator()
        fm.addAction("Import COL…",      self._import_col_data)
        fm.addAction("Export COL…",      self._export_col_data)

        # Edit
        em = parent_menu.addMenu("Edit")
        em.addAction("Undo",             lambda: getattr(self, 'undo_action', lambda: None) and self.undo_action())

        # View
        vm = parent_menu.addMenu("View")
        vm.addAction("Sort Models",      self._show_sort_menu if hasattr(self, '_show_sort_menu') else lambda: None)

    workshop_closed = pyqtSignal()
    window_closed = pyqtSignal()

    def __init__(self, parent=None, main_window=None): #vers 10
        """initialize_features"""
        if DEBUG_STANDALONE and main_window is None:
            print(App_name + " Initializing ...")

        super().__init__(parent)
        self.setWindowTitle(App_name)
        self.setWindowIcon(SVGIconFactory.col_workshop_icon())
        self.icon_factory = SVGIconFactory()

        self.main_window = main_window

        self.undo_stack = []
        self.button_display_mode = 'both'
        self.last_save_directory = None
        # Thumbnail spin animation state
        self._spin_timer  = None
        self._spin_row    = None
        self._spin_model  = None
        self._spin_yaw    = 0.0
        self._spin_pitch  = 0.0
        self._spin_dyaw   = 1.0
        self._spin_dpitch = 0.2
        # Thumbnail view axis (applied to all static thumbnails)
        self._thumb_yaw   = 0.0    # top-down (XY plane) by default
        self._thumb_pitch = 0.0

        # Set default fonts
        from PyQt6.QtGui import QFont
        default_font = QFont("Fira Sans Condensed", 14)
        self.setFont(default_font)
        self.title_font = QFont("Arial", 14)
        self.panel_font = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)
        self.standalone_mode = (main_window is None)

        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        else:
            # FIXED: Create AppSettings for standalone mode
            try:
                from apps.utils.app_settings_system import AppSettings
                self.app_settings = AppSettings()
            except Exception as e:
                print(f"Could not initialize AppSettings: {e}")
                self.app_settings = None
        if hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        self._show_boxes = True
        self._show_mesh = True

        self._checkerboard_size = 16
        self._overlay_opacity = 50
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        self.background_color = QColor(42, 42, 42)
        self.background_mode = 'solid'
        self.placeholder_text = "No Surface"
        self.setMinimumSize(200, 200)
        preview_widget = False

        # Docking state
        self.is_docked = (main_window is not None)
        self.dock_widget = None
        self.is_overlay = False
        self.overlay_table = None
        self.overlay_tab_index = -1

        self.setWindowTitle(App_name + ": No File")
        self.resize(1400, 800)
        self.use_system_titlebar = False
        self.window_always_on_top = False

        # Window flags
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        self._initialize_features()

        # Corner resize variables
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None

        if parent:
            parent_pos = parent.pos()
            self.move(parent_pos.x() + 50, parent_pos.y() + 80)


        # Paint toolbar attrs — set by _create_paint_bar() called from _create_right_panel
        self.paint_toolbar   = None
        self.paint_mat_combo = None
        self.paint_swatch    = None
        self.paint_undo_btn  = None
        self.paint_exit_btn  = None
        self.tool_paint_btn  = None
        self.tool_dropper_btn = None
        self.tool_fill_btn   = None

        # Setup UI FIRST
        self.setup_ui()
        # Setup hotkeys
        self._setup_hotkeys()

        # Apply theme ONCE at the end
        self._apply_theme()


    def setup_ui(self): #vers 8
        """Setup the main UI layout"""
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(5)

        # Toolbar - hidden when embedded in main window tab
        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        if not self.standalone_mode:
            toolbar.setVisible(False)
        main_layout.addWidget(toolbar)

        # Tab bar for multiple col files
        self.col_tabs = QTabWidget()
        self.col_tabs.setTabsClosable(True)
        self.col_tabs.tabCloseRequested.connect(self._close_col_tab)


        # Create initial tab with main content
        initial_tab = QWidget()
        tab_layout = QVBoxLayout(initial_tab)
        tab_layout.setContentsMargins(0, 0, 0, 0)


        # Main splitter
        self._main_splitter = QSplitter(Qt.Orientation.Horizontal)

        # Create all panels first
        left_panel = self._create_left_panel()
        middle_panel = self._create_middle_panel()
        right_panel = self._create_right_panel()

        # Add panels to splitter based on mode
        if left_panel is not None:  # IMG Factory mode
            self._main_splitter.addWidget(left_panel)
            self._main_splitter.addWidget(middle_panel)
            self._main_splitter.addWidget(right_panel)
            # Set proportions (2:3:5)
            self._main_splitter.setStretchFactor(0, 2)
            self._main_splitter.setStretchFactor(1, 3)
            self._main_splitter.setStretchFactor(2, 5)
        else:  # Standalone mode
            self._main_splitter.addWidget(middle_panel)
            self._main_splitter.addWidget(right_panel)
            # Set proportions (1:1)
            self._main_splitter.setStretchFactor(0, 1)
            self._main_splitter.setStretchFactor(1, 1)

        main_layout.addWidget(self._main_splitter)
        self._main_splitter.splitterMoved.connect(self._on_splitter_moved)

        # Status indicators - hidden when embedded in main window tab
        if hasattr(self, '_setup_status_indicators'):
            status_frame = self._setup_status_indicators()
            if not self.standalone_mode:
                status_frame.setVisible(False)
            main_layout.addWidget(status_frame)

        # Apply theme colours to all icons now that UI is fully built
        self._refresh_icons()
        self._connect_all_buttons()


    def _connect_all_buttons(self): #vers 2
        """Wire flip/rotate transform buttons to preview_widget.
        Called once from setup_ui after all panels are built."""
        pw = getattr(self, 'preview_widget', None)
        if not (pw and isinstance(pw, COL3DViewport)):
            return

        def _safe(btn_name, fn):
            btn = getattr(self, btn_name, None)
            if not btn: return
            try: btn.clicked.disconnect()
            except Exception: pass
            btn.clicked.connect(fn)

        _safe('flip_vert_btn',  pw.flip_vertical)
        _safe('flip_horz_btn',  pw.flip_horizontal)
        _safe('rotate_cw_btn',  pw.rotate_cw)
        _safe('rotate_ccw_btn', pw.rotate_ccw)


    # ── Stub implementations (log until fully implemented) ──────────────────

    def _create_new_model(self): #vers 1
        from PyQt6.QtWidgets import QInputDialog
        name, ok = QInputDialog.getText(self, "New Model", "Model name:")
        if not ok or not name.strip(): return
        from apps.methods.col_workshop_classes import COLModel, COLHeader, COLVersion, COLBounds
        m = COLModel()
        m.name = name.strip(); m.version = COLVersion.COL_1
        if not self.current_col_file: return
        self.current_col_file.models.append(m)
        self._populate_collision_list()
        self.collision_list.selectRow(self.collision_list.rowCount()-1)

    def _delete_selected_model(self): #vers 2
        """Delete selected collision model(s) — uses currentRow() for reliability."""
        if not self.current_col_file: return
        models = getattr(self.current_col_file, 'models', [])
        if not models: return

        lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
              else self.collision_list)

        # Collect selected indices (highest first so deletion doesn't shift lower rows)
        indices = sorted({i.row() for i in lw.selectionModel().selectedRows()
                          if 0 <= i.row() < len(models)}, reverse=True)
        cr = lw.currentRow()
        if 0 <= cr < len(models):
            indices = sorted(set(indices) | {cr}, reverse=True)
        if not indices: return

        from PyQt6.QtWidgets import QMessageBox
        if len(indices) == 1:
            name = models[indices[0]].name
            if QMessageBox.question(self, "Delete", f"Delete '{name}'?",
               QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
               ) != QMessageBox.StandardButton.Yes:
                return
        else:
            if QMessageBox.question(self, "Delete",
               f"Delete {len(indices)} collision models?",
               QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
               ) != QMessageBox.StandardButton.Yes:
                return

        for idx in indices:
            del models[idx]
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._set_status(f"Deleted {len(indices)} model(s).")

    def _duplicate_selected_model(self): #vers 1
        rows = self.collision_list.selectionModel().selectedRows()
        if not rows or not self.current_col_file: return
        row = rows[0].row()
        item = self.collision_list.item(row, 1)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        import copy
        m = copy.deepcopy(self.current_col_file.models[idx])
        m.name = m.name + "_copy"
        self.current_col_file.models.insert(idx+1, m)
        self._populate_collision_list()
        self.collision_list.selectRow(row+1)

    def _copy_model_to_clipboard(self): #vers 1
        rows = self.collision_list.selectionModel().selectedRows()
        if not rows or not self.current_col_file: return
        row = rows[0].row()
        item = self.collision_list.item(row, 1)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        import copy
        self._clipboard_model = copy.deepcopy(self.current_col_file.models[idx])
        if hasattr(self, 'paste_btn') and self.paste_btn:
            self.paste_btn.setEnabled(True)

    def _paste_model_from_clipboard(self): #vers 1
        if not hasattr(self, '_clipboard_model') or not self._clipboard_model: return
        if not self.current_col_file: return
        import copy
        m = copy.deepcopy(self._clipboard_model)
        m.name = m.name + "_paste"
        self.current_col_file.models.append(m)
        self._populate_collision_list()
        self.collision_list.selectRow(self.collision_list.rowCount()-1)

    def _open_surface_paint_dialog(self): #vers 2
        """Open material paint dialog (delegates to _open_paint_editor)."""
        self._open_paint_editor()

    def _open_surface_type_dialog(self): #vers 1
        """Show surface material type picker for selected model."""
        rows = self.collision_list.selectionModel().selectedRows()
        if not rows or not self.current_col_file: return
        row = rows[0].row()
        item = self.collision_list.item(row, 1)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        model = self.current_col_file.models[idx]
        types = {0:"Default",1:"Tarmac",2:"Gravel",3:"Grass",4:"Sand",5:"Water",
                 6:"Metal",7:"Wood",8:"Concrete",63:"Obstacle"}
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QListWidget, QDialogButtonBox
        dlg = QDialog(self); dlg.setWindowTitle(f"Surface Type — {model.name}")
        lay = QVBoxLayout(dlg)
        lst = QListWidget()
        for k,v in types.items(): lst.addItem(f"{k:3d}  {v}")
        lay.addWidget(lst)
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept); btns.rejected.connect(dlg.reject)
        lay.addWidget(btns)
        dlg.exec()


    def _cycle_view_render_style(self): #vers 1
        """Cycle viewport render: wireframe -> semi -> solid."""
        pw = getattr(self, 'preview_widget', None)
        if not pw: return
        modes = ['wireframe','semi','solid']
        cur = getattr(pw, '_render_style', 'semi')
        pw._render_style = modes[(modes.index(cur)+1) % 3] if cur in modes else 'semi'
        pw.update()


    def _open_paint_editor(self): #vers 4
        """Enter paint mode immediately — no dialog.
        All material selection happens in the viewport overlay."""
        if not self.current_col_file:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "No File", "Load a COL file first.")
            return
        model = self._get_selected_model()
        if model is None:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "No Model Selected",
                "Select a model in the list first.")
            return
        if not getattr(model, 'faces', []):
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "No Mesh Faces",
                f"'{model.name}' has no mesh faces to paint.")
            return

        vp = getattr(self, 'preview_widget', None)
        if not vp:
            return

        models  = getattr(self.current_col_file, 'models', [])
        model_idx = models.index(model) if model in models else -1

        # Use last active mat or default 0
        mat_id  = getattr(self, '_paint_active_mat', 0)

        # Push one undo snapshot on entry
        if model_idx >= 0:
            self._push_undo(model_idx, f"Enter paint mode")

        # Cache the full material list for this model's version
        try:
            from apps.methods.col_materials import get_materials_for_version, COLGame
            ver     = getattr(getattr(model,'version',None),'value',3) if model else 3
            game    = COLGame.VC if ver == 1 else COLGame.SA
            self._paint_mat_list = get_materials_for_version(game, include_procedural=True)
        except Exception:
            self._paint_mat_list = [(i, f"Material {i}", "808080") for i in range(64)]

        # Set current index into the list
        mat_ids = [m[0] for m in self._paint_mat_list]
        self._paint_mat_idx = mat_ids.index(mat_id) if mat_id in mat_ids else 0
        mat_id = self._paint_mat_list[self._paint_mat_idx][0]
        self._paint_active_mat = mat_id

        # Enter viewport paint mode
        vp.set_paint_mode(True, mat_id)
        vp.on_face_selected = self._on_painted_face
        vp._paint_material  = mat_id
        vp.update()  # draw overlay immediately

        # Update paint button to show exit state
        for btn in self._find_all_paint_btns():
            try: btn.clicked.disconnect()
            except: pass
            btn.clicked.connect(self._exit_paint_mode)
            btn.setText("[ ] Exit Paint")
            btn.setStyleSheet("color:#ff6b35; font-weight:bold;")

        self._set_status(
            "Paint mode — click faces to paint  |  ◀▶ change material  "
            "|  Shift+drag to select  |  Esc to exit")


    def _open_paint_mat_popup(self): #vers 2
        """Searchable material popup anchored below the mat chip.
        Closes on item click, X button, or focus loss."""
        from PyQt6.QtWidgets import (QListWidget, QListWidgetItem, QFrame,
                                     QVBoxLayout, QHBoxLayout, QLineEdit,
                                     QPushButton, QLabel)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QColor

        lst = getattr(self, '_paint_mat_list', [])
        if not lst: return

        vp = getattr(self, 'preview_widget', None)
        if not vp: return

        # Close any existing popup
        old = getattr(self, '_mat_popup', None)
        if old:
            try: old.hide(); old.deleteLater()
            except: pass
            self._mat_popup = None

        popup = QFrame(vp)
        popup.setFrameStyle(QFrame.Shape.StyledPanel)
        popup.setStyleSheet(
            "QFrame { background:#1a1a2e; border:1px solid #ff8c00; border-radius:4px; }"
            "QListWidget { background:#1a1a2e; color:#eee; border:none; }"
            "QListWidget::item { padding:2px 4px; }"
            "QListWidget::item:hover { background:#252540; }"
            "QListWidget::item:selected { background:#ff8c00; color:#000; }"
            "QLineEdit { background:#252535; color:#eee; border:1px solid #555; "
            "            border-radius:3px; padding:2px 4px; }"
            "QPushButton { background:transparent; color:#ff6b35; border:none; "
            "              font-weight:bold; font-size:14px; }"
            "QPushButton:hover { color:#ff3300; }"
        )

        lay = QVBoxLayout(popup)
        lay.setContentsMargins(6, 4, 6, 6)
        lay.setSpacing(4)

        # Header: search + X
        hdr = QHBoxLayout()
        search = QLineEdit()
        search.setPlaceholderText("Filter materials…")
        search.setFixedHeight(26)
        hdr.addWidget(search)
        close_btn = QPushButton("✕")
        close_btn.setFixedSize(22, 22)
        close_btn.setToolTip("Close")
        hdr.addWidget(close_btn)
        lay.addLayout(hdr)

        lw = QListWidget()
        lw.setFixedHeight(220)
        lw.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        lay.addWidget(lw)

        ws = self

        def _close():
            popup.hide()
            popup.deleteLater()
            ws._mat_popup = None

        close_btn.clicked.connect(_close)

        def _populate(flt=""):
            lw.clear()
            for mid, name, hex_col in lst:
                if flt and flt.lower() not in name.lower()                         and flt not in str(mid):
                    continue
                item = QListWidgetItem(f"  {mid:3d}  {name}")
                item.setData(Qt.ItemDataRole.UserRole, mid)
                c = QColor(f"#{hex_col}")
                item.setBackground(
                    QColor(max(0,c.red()//4), max(0,c.green()//4),
                           min(255, c.blue()//4 + 15)))
                item.setForeground(c.lighter(200))
                lw.addItem(item)
            # Scroll to current material
            cur_id = getattr(ws, '_paint_active_mat', 0)
            for i in range(lw.count()):
                if lw.item(i).data(Qt.ItemDataRole.UserRole) == cur_id:
                    lw.setCurrentRow(i)
                    lw.scrollToItem(lw.item(i))
                    break

        _populate()
        search.textChanged.connect(_populate)

        def _pick(item):
            mid = item.data(Qt.ItemDataRole.UserRole)
            if mid is None: return
            mat_ids = [m[0] for m in lst]
            ws._paint_mat_idx    = mat_ids.index(mid) if mid in mat_ids else 0
            ws._paint_active_mat = mid
            vp._paint_material   = mid
            vp.update()
            _close()

        lw.itemClicked.connect(_pick)
        lw.itemDoubleClicked.connect(_pick)

        # Position: anchored below the mat chip, aligned to right edge
        W = vp.width()
        _MARGIN = 8; _MAT_W = 200; _ROW1_Y = 4; _CHIP_H = 26; _ARW = 22
        pw = 200   # popup width
        px = W - _MAT_W - _MARGIN + _ARW     # left-align with mat name chip
        py = _ROW1_Y + _CHIP_H + 4           # just below mat row
        # Keep inside viewport
        px = max(4, min(px, W - pw - 4))
        popup.move(px, py)
        popup.resize(pw, 264)
        popup.show()
        popup.raise_()
        self._mat_popup = popup
        search.setFocus()


    def _apply_to_selected_faces_paint(self): #vers 1
        """Apply current paint material to all selected faces (F key in paint mode)."""
        vp = getattr(self, 'preview_widget', None)
        if not vp: return
        sel = sorted(getattr(vp, '_selected_faces', set()))
        if not sel:
            self._set_status("No faces selected — click or drag to select faces first")
            return
        model = self._get_selected_model()
        if not model: return
        models = getattr(self.current_col_file, 'models', [])
        mi = models.index(model) if model in models else -1
        mat_id = getattr(self, '_paint_active_mat', 0)
        if mi >= 0:
            self._push_undo(mi, f"Paint {mat_id} → {len(sel)} selected faces")
        for fi in sel:
            if fi < len(model.faces):
                f = model.faces[fi]
                if hasattr(f.material, 'material_id'):
                    f.material.material_id = mat_id
                else:
                    f.material = mat_id
        vp.update()
        self._set_status(f"Applied material {mat_id} to {len(sel)} selected face(s)")

    def _paint_cycle_mat(self, delta: int): #vers 1
        """Cycle active paint material by delta steps (+1 next / -1 prev)."""
        lst = getattr(self, '_paint_mat_list', [])
        if not lst: return
        idx = getattr(self, '_paint_mat_idx', 0)
        idx = (idx + delta) % len(lst)
        self._paint_mat_idx   = idx
        mat_id, name, hex_col = lst[idx]
        self._paint_active_mat = mat_id
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp._paint_material = mat_id
            vp.update()
        self._set_status(f"Paint material: {mat_id} — {name}")

    def _show_paint_toolbar(self, mat_id: int, model=None): #vers 5
        """Populate combo then show the floating paint bar."""
        tb    = self.paint_toolbar
        combo = self.paint_mat_combo
        if not tb or not combo:
            return  # not built yet

        # Populate material combo
        try:
            from apps.methods.col_materials import get_materials_for_version, COLGame, get_material_colour
            ver = getattr(getattr(model, 'version', None), 'value', 3) if model else 3
            game = COLGame.VC if ver == 1 else COLGame.SA
            all_mats = get_materials_for_version(game, include_procedural=True)
        except Exception:
            all_mats = [(i, f"Material {i}", "808080") for i in range(10)]

        from PyQt6.QtGui import QPixmap, QColor, QIcon
        from PyQt6.QtCore import Qt as _Qt

        def _make_swatch_icon(hex_col: str) -> QIcon:
            """16×16 filled square icon for combo items."""
            px = QPixmap(16, 16)
            px.fill(QColor(f"#{hex_col}"))
            return QIcon(px)

        def _apply_swatch(hex_col: str):
            """Update the standalone swatch label colour."""
            sw = getattr(self, 'paint_swatch', None)
            if sw:
                sw.setStyleSheet(
                    f"background:#{hex_col}; border:2px solid #888; border-radius:3px;")
                sw.setToolTip(f"Colour: #{hex_col.upper()}")

        combo.blockSignals(True)
        combo.clear()
        sel_idx = 0
        for i, (mid, name, hex_col) in enumerate(all_mats):
            icon = _make_swatch_icon(hex_col)
            combo.addItem(icon, f"{mid:3d}  {name}", mid)
            if mid == mat_id:
                sel_idx = i
                _apply_swatch(hex_col)
        combo.setIconSize(QSize(16, 16))
        combo.setCurrentIndex(sel_idx)
        combo.blockSignals(False)

        # Update viewport + swatch when material changes
        try:
            combo.currentIndexChanged.disconnect()
        except Exception:
            pass

        # Build a quick hex_col lookup by mat_id
        _col_map = {mid: hx for mid, _, hx in all_mats}

        def _on_mat_changed(idx):
            new_mid = combo.itemData(idx)
            if new_mid is None:
                return
            self._paint_active_mat = new_mid
            vp = getattr(self, 'preview_widget', None)
            if vp:
                vp._paint_material = new_mid
                vp.update()
            _apply_swatch(_col_map.get(new_mid, "808080"))
            self._set_status(f"Paint material: {new_mid}")

        combo.currentIndexChanged.connect(_on_mat_changed)

        # Enable undo button only if undo stack has entries
        if undo_btn:
            undo_btn.setEnabled(bool(getattr(self, 'undo_stack', [])))

        # Trigger viewport repaint to show the paint overlay chips
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp.update()


    def _on_painted_face(self, face_index, face): #vers 2
        """Called by viewport when a face is painted. Status update only —
        undo state was pushed before entering paint mode."""
        mat_id = self._paint_active_mat if hasattr(self, '_paint_active_mat') else 0
        self._set_status(f"Painted face {face_index} → material {mat_id}  [Esc to exit]")

    def _set_paint_tool(self, mode: str): #vers 1
        """Switch active paint tool: 'paint' | 'dropper' | 'fill'."""
        self._current_paint_tool = mode
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp._tool_mode = mode
            if mode == 'dropper':
                from PyQt6.QtCore import Qt
                vp.setCursor(Qt.CursorShape.PointingHandCursor)
            elif mode == 'fill':
                from PyQt6.QtCore import Qt
                vp.setCursor(Qt.CursorShape.CrossCursor)
            else:
                from PyQt6.QtCore import Qt
                vp.setCursor(Qt.CursorShape.CrossCursor)
        # Update button check states
        for btn, name in [
            (getattr(self, 'tool_paint_btn', None), 'paint'),
            (getattr(self, 'tool_dropper_btn', None), 'dropper'),
            (getattr(self, 'tool_fill_btn', None), 'fill'),
        ]:
            if btn:
                btn.setChecked(name == mode)
        tool_names = {'paint': 'Paint', 'dropper': 'Dropper (pick material)', 'fill': 'Fill (same material)'}
        self._set_status(f"Tool: {tool_names.get(mode, mode)}")

    def _exit_paint_mode(self): #vers 2
        """Exit paint mode — hide toolbar, restore paint button."""
        # Close material popup if open
        old_popup = getattr(self, '_mat_popup', None)
        if old_popup:
            try: old_popup.hide(); old_popup.deleteLater()
            except: pass
            self._mat_popup = None

        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp.set_paint_mode(False)
            vp.on_face_selected = None

        # Hide QWidget paint bar if it was created, refresh viewport
        tb = getattr(self, 'paint_toolbar', None)
        if tb:
            tb.hide()
        vp = getattr(self, 'preview_widget', None)
        if vp:
            vp.update()

        # Reset paint button in both icon and text panels
        for btn in self._find_all_paint_btns():
            try: btn.clicked.disconnect()
            except: pass
            btn.clicked.connect(self._open_paint_editor)
            btn.setText("Paint")
            btn.setStyleSheet("")
            btn.setChecked(False) if btn.isCheckable() else None

        self._set_status("Paint mode exited.")

    def _on_paint_mode_exited(self): #vers 1
        """Called by viewport Escape key — sync button state."""
        self._exit_paint_mode()

    def _get_selected_model(self): #vers 4
        """Return the currently selected COLModel or None.
        Uses currentRow() on the active (visible) list widget — more reliable
        than selectedRows() which can fail with custom delegates."""
        if not self.current_col_file:
            return None
        models = getattr(self.current_col_file, 'models', [])
        if not models:
            return None

        # Only check the VISIBLE list — the hidden one may have stale state
        if getattr(self, '_col_view_mode', 'detail') == 'detail':
            lw = getattr(self, 'col_compact_list', None)
        else:
            lw = getattr(self, 'collision_list', None)

        if lw is None:
            return None

        # currentRow() is always reliable; selectedRows() can fail with delegates
        row = lw.currentRow()
        if row < 0:
            # Fallback: try selectedRows
            rows = lw.selectionModel().selectedRows()
            if rows:
                row = rows[0].row()
        if 0 <= row < len(models):
            return models[row]
        return None

    def _set_status(self, msg: str): #vers 1
        """Write msg to the status label (whichever one exists)."""
        if hasattr(self, 'status_label'):
            self.status_label.setText(msg)
        elif hasattr(self, 'status_bar') and hasattr(self.status_bar, 'showMessage'):
            self.status_bar.showMessage(msg, 3000)
        else:
            print(f"[COL] {msg}")

    def _create_new_surface(self): #vers 1
        """Add a new empty COL model to the loaded file."""
        if not self.current_col_file:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "No File", "Load a COL file first.")
            return
        from apps.methods.col_workshop_classes import COLModel, COLHeader, COLBounds, COLVersion
        from apps.methods.col_core_classes import Vector3
        hdr = COLHeader(fourcc=b'COLL', size=0, name='new_model',
                        model_id=0, version=COLVersion.COL_1)
        bnd = COLBounds(radius=1.0, center=Vector3(0,0,0),
                        min=Vector3(-1,-1,-1), max=Vector3(1,1,1))
        model = COLModel(header=hdr, bounds=bnd,
                         spheres=[], boxes=[], vertices=[], faces=[])
        self.current_col_file.models.append(model)
        self._populate_compact_col_list()
        self._populate_collision_list()
        new_row = len(self.current_col_file.models) - 1
        active = (self.col_compact_list
                  if self._col_view_mode == 'detail' else self.collision_list)
        if active.rowCount() > new_row:
            active.selectRow(new_row)

    def _open_surface_edit_dialog(self): #vers 2
        """Open the COL Mesh Editor for the currently selected model."""
        try:
            from apps.components.Col_Editor.col_mesh_editor import open_col_mesh_editor
            open_col_mesh_editor(self, parent=self)
        except Exception as e:
            import traceback; traceback.print_exc()
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Mesh Editor Error", str(e))
    def _build_col_from_txd(self): #vers 2
        """Create stub COL models for each texture name in a loaded TXD."""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        from apps.methods.col_workshop_loader import COLFile
        from apps.methods.col_workshop_classes import COLModel, COLVersion, COLBounds
        txd_path, _ = QFileDialog.getOpenFileName(
            self, "Select TXD file", "", "TXD Files (*.txd);;All Files (*)")
        if not txd_path:
            return
        try:
            import os
            # Extract texture names from TXD (scan for null-terminated strings after each 0x15 chunk)
            import struct
            names = []
            with open(txd_path, 'rb') as f:
                data = f.read()
            pos = 0
            while pos < len(data) - 12:
                t, s, v = struct.unpack_from('<III', data, pos)
                if t == 0x15 and pos + 12 + 12 < len(data):
                    body = pos + 12 + 12
                    name = data[body+8:body+40].rstrip(b'').decode('ascii','ignore').strip()
                    if name:
                        names.append(name)
                pos += 12 + s if s > 0 else 1
            if not names:
                QMessageBox.warning(self, "No Textures", "No texture names found in TXD.")
                return
            if not self.current_col_file:
                from apps.methods.col_workshop_loader import COLFile
                self.current_col_file = COLFile()
                self.current_col_file.models = []
            added = 0
            for name in names:
                m = COLModel()
                m.name = name
                m.version = COLVersion.COL_2
                m.spheres = []; m.boxes = []; m.vertices = []; m.faces = []
                m.shadow_verts = []; m.shadow_faces = []
                bounds = COLBounds()
                bounds.min = type('V', (), {'x': -1.0, 'y': -1.0, 'z': -1.0})()
                bounds.max = type('V', (), {'x':  1.0, 'y':  1.0, 'z':  1.0})()
                bounds.center = type('V', (), {'x': 0.0, 'y': 0.0, 'z': 0.0})()
                bounds.radius = 1.73
                m.bounds = bounds; m.model_id = 0
                self.current_col_file.models.append(m)
                added += 1
            self._populate_collision_list()
            self._populate_compact_col_list()
            msg = f"Created {added} stub COL model(s) from {os.path.basename(txd_path)}"
            self._set_status(msg)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(msg)
        except Exception as e:
            QMessageBox.critical(self, "Error", str(e))

    def _cycle_render_mode(self): #vers 1
        modes = ['wireframe','solid','painted']
        cur   = getattr(self, '_render_mode', 'wireframe')
        self._render_mode = modes[(modes.index(cur)+1) % len(modes)] if cur in modes else 'wireframe'
        if hasattr(self, 'preview_widget') and self.preview_widget:
            self.preview_widget._refresh()

    def _convert_surface(self): #vers 2
        """Convert selected model to a different COL version."""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox, QPushButton, QMessageBox
        from apps.methods.col_workshop_classes import COLVersion
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        current = getattr(model.version, 'name', str(model.version))
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Convert COL version — {model.name}")
        dlg.setFixedSize(300, 130)
        lay = QVBoxLayout(dlg)
        lay.addWidget(QLabel(f"Current version: <b>{current}</b>"))
        lay.addWidget(QLabel("Convert to:"))
        combo = QComboBox()
        ver_map = {'COL_1': COLVersion.COL_1, 'COL_2': COLVersion.COL_2, 'COL_3': COLVersion.COL_3}
        [combo.addItem(k) for k in ver_map if k != current]
        lay.addWidget(combo)
        btns = QHBoxLayout()
        ok = QPushButton("Convert"); cancel = QPushButton("Cancel")
        btns.addStretch(); btns.addWidget(ok); btns.addWidget(cancel)
        lay.addLayout(btns)
        cancel.clicked.connect(dlg.reject)
        def _do():
            model.version = ver_map[combo.currentText()]
            self._populate_collision_list()
            self._populate_compact_col_list()
            self._set_status(f"Converted {model.name} to {combo.currentText()}")
            dlg.accept()
        ok.clicked.connect(_do)
        dlg.exec()

    def _show_shadow_mesh(self): #vers 2
        """Show shadow mesh info for selected model."""
        from PyQt6.QtWidgets import QMessageBox
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        sv = len(getattr(model, 'shadow_verts', []))
        sf = len(getattr(model, 'shadow_faces', []))
        if sv == 0 and sf == 0:
            QMessageBox.information(self, "Shadow Mesh",
                f"'{model.name}' has no shadow mesh data.\n\n"
                "COL3+ models can have a separate low-poly shadow collision mesh.")
        else:
            QMessageBox.information(self, "Shadow Mesh",
                f"Model: {model.name}\n"
                f"Shadow vertices: {sv}\n"
                f"Shadow faces:    {sf}\n\n"
                "Shadow mesh is included in COL3 export.")

    def _create_shadow_mesh(self): #vers 2
        """Auto-generate shadow mesh as a copy of the main collision mesh."""
        from PyQt6.QtWidgets import QMessageBox
        import copy
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        if not model.vertices or not model.faces:
            QMessageBox.warning(self, "No Mesh", f"'{model.name}' has no vertex/face data.")
            return
        # Upgrade to COL3 if needed
        from apps.methods.col_workshop_classes import COLVersion
        if getattr(model.version, 'value', 0) < 3:
            reply = QMessageBox.question(self, "Upgrade to COL3",
                "Shadow mesh requires COL3. Upgrade this model?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if reply != QMessageBox.StandardButton.Yes:
                return
            model.version = COLVersion.COL_3
        model.shadow_verts = copy.deepcopy(model.vertices)
        model.shadow_faces = copy.deepcopy(model.faces)
        self._populate_collision_list()
        self._populate_compact_col_list()
        msg = (f"Shadow mesh created for {model.name}: "
               f"{len(model.shadow_verts)}V {len(model.shadow_faces)}F")
        self._set_status(msg)
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(msg)

    def _remove_shadow_mesh(self): #vers 2
        """Remove shadow mesh data from the selected COL model."""
        from PyQt6.QtWidgets import QMessageBox
        model = self._get_selected_model()
        if not model:
            QMessageBox.warning(self, "No Selection", "Select a collision model first.")
            return
        has_shadow = bool(getattr(model, 'shadow_verts', []) or getattr(model, 'shadow_faces', []))
        if not has_shadow:
            QMessageBox.information(self, "No Shadow Mesh", f"{model.name} has no shadow mesh.")
            return
        reply = QMessageBox.question(self, "Remove Shadow Mesh",
            f"Remove shadow mesh from '{model.name}'?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply == QMessageBox.StandardButton.Yes:
            model.shadow_verts = []
            model.shadow_faces = []
            self._set_status(f"Removed shadow mesh from {model.name}")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Shadow mesh removed from {model.name}")

    def _compress_col(self): #vers 2
        """Mark COL file for compressed output (sets flags on export)."""
        from PyQt6.QtWidgets import QMessageBox
        if not self.current_col_file:
            QMessageBox.warning(self, "No File", "No COL file loaded.")
            return
        QMessageBox.information(self, "COL Compression",
            "COL files do not use zlib/LZO compression internally.\n\n"
            "To reduce size: remove unused models, clear shadow meshes,\n"
            "or reduce vertex/face counts in the mesh editor.")

    def _uncompress_col(self): #vers 2
        """Reload COL file (parses fresh from disk, clears in-memory edits)."""
        from PyQt6.QtWidgets import QMessageBox
        if not self.current_col_file or not getattr(self, 'current_file_path', None):
            QMessageBox.warning(self, "No File", "No COL file loaded.")
            return
        reply = QMessageBox.question(self, "Reload File",
            "Reload from disk? Unsaved changes will be lost.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply == QMessageBox.StandardButton.Yes:
            self._open_file(self.current_file_path)

    def _open_render_settings_dialog(self): #vers 1
        """Render & background settings dialog."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel,
                                     QComboBox, QSlider, QPushButton, QColorDialog,
                                     QGroupBox, QDialogButtonBox, QCheckBox)
        from PyQt6.QtGui import QColor
        from PyQt6.QtCore import Qt

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
        style_combo.addItems(["Wireframe", "Semi-transparent", "Solid"])
        mapping = {"wireframe":"Wireframe","semi":"Semi-transparent","solid":"Solid"}
        style_combo.setCurrentText(mapping.get(pw._render_style, "Semi-transparent"))
        sg.addWidget(style_combo)
        lay.addWidget(style_grp)

        # Background
        bg_grp = QGroupBox("Background")
        bg = QHBoxLayout(bg_grp)
        r,g,b = pw._bg_color
        bg_preview = QPushButton("  ")
        bg_preview.setFixedSize(60, 28)
        bg_preview.setStyleSheet(f"background-color: rgb({r},{g},{b});")
        def _pick_bg():
            c = QColorDialog.getColor(QColor(r,g,b), dlg, "Background Colour")
            if c.isValid():
                bg_preview.setStyleSheet(f"background-color: {c.name()};")
                bg_preview.setProperty("chosen", (c.red(), c.green(), c.blue()))
        bg_preview.clicked.connect(_pick_bg)
        bg.addWidget(QLabel("Colour:"))
        bg.addWidget(bg_preview)

        scene_cb = QComboBox()
        scene_cb.addItems(["Dark", "Mid", "Light"])
        bg.addWidget(scene_cb)
        lay.addWidget(bg_grp)

        # Buttons
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                QDialogButtonBox.StandardButton.Cancel)
        btns.rejected.connect(dlg.reject)
        def _apply():
            s = style_combo.currentText()
            rev = {"Wireframe":"wireframe","Semi-transparent":"semi","Solid":"solid"}
            pw.set_render_style(rev.get(s,"semi"))
            chosen = bg_preview.property("chosen")
            if chosen:
                pw.set_background_color(chosen)
            dlg.accept()
        btns.accepted.connect(_apply)
        lay.addWidget(btns)
        dlg.exec()


    # ── Method aliases — drop Qt signal args (*_, **__) before forwarding ──
    # These are called from Qt signals that pass e.g. bool(checked) as arg.
    # Target methods take only self, so we must not forward the signal args.
    def _compress_surface(self, *_, **__): return self._compress_col()
    def _copy_surface(self, *_, **__): return self._copy_model_to_clipboard()
    def _delete_surface(self, *_, **__): return self._delete_selected_model()
    def _duplicate_surface(self, *_, **__): return self._duplicate_selected_model()
    def _force_save_col(self, *_, **__): return self._save_file()
    def _import_selected(self, *_, **__): return self._import_col_data()
    def _import_surface(self, *_, **__): return self._import_col_data()
    def _open_col_file(self, *_, **__): return self._open_file()
    def _open_mipmap_manager(self, *_, **__): return self._show_shadow_mesh()
    def _paste_surface(self, *_, **__): return self._paste_model_from_clipboard()
    def _reload_surface_table(self, *_, **__): return self._populate_collision_list()
    def _remove_shadow(self, *_, **__): return self._remove_shadow_mesh()
    def _save_as_col_file(self, *_, **__): return self._save_file()
    def _save_col_file(self, *_, **__): return self._save_file()
    def _saveall_file(self, *_, **__): return self._save_file()
    def _uncompress_surface(self, *_, **__): return self._uncompress_col()
    def export_all(self, *_, **__): return self._export_col_data()
    def export_all_surfaces(self, *_, **__): return self._export_col_data()
    def export_selected(self, *_, **__): return self._export_col_data()
    def export_selected_surface(self, *_, **__): return self._export_col_data()
    def refresh(self, *_, **__): return self._populate_collision_list()
    def reload_surface_table(self, *_, **__): return self._populate_collision_list()
    def save_col_file(self, *a, **kw): return self._save_file(*a, **kw)
    def shadow_dialog(self, *a, **kw): return self._create_shadow_mesh(*a, **kw)
    def switch_surface_view(self, *a, **kw): return self._cycle_render_mode(*a, **kw)

    def _change_format(self, *a, **kw): pass
    def _close_col_tab(self, *a, **kw): pass
    def _edit_main_surface(self, *a, **kw): pass
    def _focus_search(self, *a, **kw): pass
    def _rename_shadow_shortcut(self, *a, **kw): pass
    def _save_surface_name(self, *a, **kw): pass
    def _show_detailed_info(self, *a, **kw): pass
    def _show_surface_info(self, *a, **kw): pass
    def show_help(self, *a, **kw): pass
    def show_settings_dialog(self, *a, **kw): pass

    def _set_thumbnail_view(self, yaw, pitch, label="Custom"): #vers 1
        """Change the view angle for all thumbnails and regenerate them."""
        self._thumb_yaw   = float(yaw)
        self._thumb_pitch = float(pitch)
        self._stop_thumbnail_spin()
        self._regenerate_all_thumbnails()
        if hasattr(self, 'main_window') and self.main_window:
            self.main_window.log_message(f"Thumbnail view: {label}")

    def _regenerate_all_thumbnails(self): #vers 1
        """Redraw every thumbnail in both lists at current _thumb_yaw/pitch."""
        if not self.current_col_file:
            return
        models = getattr(self.current_col_file, 'models', [])
        # Compact list
        for row in range(self.col_compact_list.rowCount()):
            item = self.col_compact_list.item(row, 0)
            if item and row < len(models):
                thumb = self._generate_collision_thumbnail(
                    models[row], 64, 64,
                    yaw=self._thumb_yaw, pitch=self._thumb_pitch)
                item.setData(Qt.ItemDataRole.DecorationRole, thumb)
                item.setData(Qt.ItemDataRole.UserRole + 1, True)
        # Detail list
        for row in range(self.collision_list.rowCount()):
            item = self.collision_list.item(row, 0)
            if item and row < len(models):
                thumb = self._generate_collision_thumbnail(
                    models[row], 64, 64,
                    yaw=self._thumb_yaw, pitch=self._thumb_pitch)
                item.setData(Qt.ItemDataRole.DecorationRole, thumb)
                item.setData(Qt.ItemDataRole.UserRole + 1, True)

    def _start_thumbnail_spin(self, row, model): #vers 1
        """Start slowly rotating the thumbnail of the selected row."""
        self._stop_thumbnail_spin()
        self._spin_row   = row
        self._spin_model = model
        self._spin_yaw   = 0.0
        # Random slow axis: yaw + slight pitch drift
        import random
        self._spin_dyaw   = random.uniform(0.8, 1.4)
        self._spin_dpitch = random.uniform(-0.3, 0.3)
        self._spin_pitch  = random.uniform(-20.0, 20.0)
        from PyQt6.QtCore import QTimer
        self._spin_timer = QTimer(self)
        self._spin_timer.setInterval(50)   # 20 fps
        self._spin_timer.timeout.connect(self._tick_thumbnail_spin)
        self._spin_timer.start()

    def _stop_thumbnail_spin(self): #vers 1
        """Stop any running thumbnail rotation."""
        t = getattr(self, '_spin_timer', None)
        if t:
            t.stop()
            t.deleteLater()
            self._spin_timer = None
        self._spin_row   = None
        self._spin_model = None

    def _tick_thumbnail_spin(self): #vers 1
        """Advance the spin angle and update the thumbnail."""
        model = getattr(self, '_spin_model', None)
        row   = getattr(self, '_spin_row',   None)
        if model is None or row is None:
            self._stop_thumbnail_spin()
            return
        # Advance angles
        self._spin_yaw   = (self._spin_yaw + self._spin_dyaw) % 360
        self._spin_pitch = max(-35.0, min(35.0,
            self._spin_pitch + self._spin_dpitch))
        # Flip pitch direction at limits
        if abs(self._spin_pitch) >= 35.0:
            self._spin_dpitch *= -1

        # Only spin if model has geometry
        has_geo = (getattr(model, 'vertices', []) or
                   getattr(model, 'spheres',  []) or
                   getattr(model, 'boxes',    []))
        if not has_geo:
            self._stop_thumbnail_spin()
            return

        # Render thumbnail at current angle
        thumb = self._generate_collision_thumbnail(
            model, 64, 64,
            yaw=self._spin_yaw, pitch=self._spin_pitch)

        # [T] view no longer has thumbnails — spin does nothing visible there
        # The viewport itself rotates via _yaw/_pitch so just stop the timer
        self._stop_thumbnail_spin()

    def _enable_name_edit(self, event, is_alpha): #vers 1
        """Enable name editing on click"""
        self.info_name.setReadOnly(False)
        self.info_name.selectAll()
        self.info_name.setFocus()


    def _update_status_indicators(self): #vers 2
        """Update status indicators"""
        if hasattr(self, 'status_collision'):
            self.status_textures.setText(f"collision: {len(self.collision_list)}")

        if hasattr(self, 'status_selected'):
            if self.selected_texture:
                name = self.selected_collision.get('name', 'Unknown')
                self.status_selected.setText(f"Selected: {name}")
            else:
                self.status_selected.setText("Selected: None")

        if hasattr(self, 'status_size'):
            if self.current_txd_data:
                size_kb = len(self.current_col_data) / 1024
                self.status_size.setText(f"COL Size: {size_kb:.1f} KB")
            else:
                self.status_size.setText("COL Size: Unknown")

        if hasattr(self, 'status_modified'):
            if self.windowTitle().endswith("*"):
                self.status_modified.setText("MODIFIED")
                self.status_modified.setStyleSheet("color: orange; font-weight: bold;")
            else:
                self.status_modified.setText("")
                self.status_modified.setStyleSheet("")
# - Panel Creation

    def _create_status_bar(self): #vers 1
        """Create bottom status bar - single line compact"""
        from PyQt6.QtWidgets import QFrame, QHBoxLayout, QLabel

        status_bar = QFrame()
        status_bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        status_bar.setFixedHeight(22)

        layout = QHBoxLayout(status_bar)
        layout.setContentsMargins(5, 0, 5, 0)
        layout.setSpacing(15)

        # Left: Ready
        self.status_label = QLabel("Ready")
        layout.addWidget(self.status_label)

        if hasattr(self, 'status_info'):
            size_kb = len(col_data) / 1024
            tex_count = len(self.collision_list)
            #self.status_col_info.setText(f"Collision: {tex_count} | col: {size_kb:.1f} KB")

        return status_bar

    def _refresh_icons(self): #vers 2
        """Refresh all button icons after theme change — picks up current text_primary colour."""
        SVGIconFactory.clear_cache()
        c = self._get_icon_color()
        SVGIconFactory.set_theme_color(c)

        # Map: attribute name → icon factory method
        _icon_map = [
            # Toolbar
            ('settings_btn',       'settings_icon'),
            ('open_btn',           'open_icon'),
            ('save_btn',           'save_icon'),
            ('saveall_btn',        'saveas_icon'),
            ('export_all_btn',     'package_icon'),
            ('undo_btn',           'undo_icon'),
            ('info_btn',           'info_icon'),
            ('minimize_btn',       'minimize_icon'),
            ('maximize_btn',       'maximize_icon'),
            ('close_btn',          'close_icon'),
            ('open_img_btn',       'folder_icon'),
            ('from_img_btn',       'open_icon'),
            # Middle panel mini-toolbar
            ('open_col_btn',       'open_icon'),
            ('save_col_btn',       'save_icon'),
            ('export_col_btn',     'package_icon'),
            ('undo_col_btn',       'undo_icon'),
            # Transform icon panel
            ('flip_vert_btn',      'flip_vert_icon'),
            ('flip_horz_btn',      'flip_horz_icon'),
            ('rotate_cw_btn',      'rotate_cw_icon'),
            ('rotate_ccw_btn',     'rotate_ccw_icon'),
            ('analyze_btn',        'analyze_icon'),
            ('copy_btn',           'copy_icon'),
            ('paste_btn',          'paste_icon'),
            ('create_surface_btn', 'add_icon'),
            ('delete_surface_btn', 'remove_icon'),
            ('import_btn',         'import_icon'),
            ('export_btn',         'export_icon'),
            # Right panel toolbar buttons
            ('switch_btn',         'flip_vert_icon'),
            ('convert_btn',        'convert_icon'),
            ('properties_btn',     'settings_icon'),
        ]
        for attr, method in _icon_map:
            btn = getattr(self, attr, None)
            if btn is None:
                continue
            fn = getattr(self.icon_factory, method, None)
            if fn is None:
                continue
            try:
                btn.setIcon(fn(color=c))
            except TypeError:
                try:
                    btn.setIcon(fn())
                except Exception:
                    pass


# - Settings Reusable

        # Sync mini toolbar visibility with current dock state
        if hasattr(self, '_middle_btn_row'):
            self._middle_btn_row.setVisible(
                self.is_docked and not self.standalone_mode)

    def _show_workshop_settings(self): #vers 1
        """Show complete workshop settings dialog"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                    QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + "Settings")
        dialog.setMinimumWidth(650)
        dialog.setMinimumHeight(550)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # TAB 1: FONTS (FIRST TAB)

        fonts_tab = QWidget()
        fonts_layout = QVBoxLayout(fonts_tab)

        # Default Font
        default_font_group = QGroupBox("Default Font")
        default_font_layout = QHBoxLayout()

        default_font_combo = QFontComboBox()
        default_font_combo.setCurrentFont(self.font())
        default_font_layout.addWidget(default_font_combo)

        default_font_size = QSpinBox()
        default_font_size.setRange(8, 24)
        default_font_size.setValue(self.font().pointSize())
        default_font_size.setSuffix(" pt")
        default_font_size.setFixedWidth(80)
        default_font_layout.addWidget(default_font_size)

        default_font_group.setLayout(default_font_layout)
        fonts_layout.addWidget(default_font_group)

        # Title Font
        title_font_group = QGroupBox("Title Font")
        title_font_layout = QHBoxLayout()

        title_font_combo = QFontComboBox()
        if hasattr(self, 'title_font'):
            title_font_combo.setCurrentFont(self.title_font)
        else:
            title_font_combo.setCurrentFont(QFont("Arial", 14))
        title_font_layout.addWidget(title_font_combo)

        title_font_size = QSpinBox()
        title_font_size.setRange(10, 32)
        title_font_size.setValue(getattr(self, 'title_font', QFont("Arial", 14)).pointSize())
        title_font_size.setSuffix(" pt")
        title_font_size.setFixedWidth(80)
        title_font_layout.addWidget(title_font_size)

        title_font_group.setLayout(title_font_layout)
        fonts_layout.addWidget(title_font_group)

        # Panel Font
        panel_font_group = QGroupBox("Panel Headers Font")
        panel_font_layout = QHBoxLayout()

        panel_font_combo = QFontComboBox()
        if hasattr(self, 'panel_font'):
            panel_font_combo.setCurrentFont(self.panel_font)
        else:
            panel_font_combo.setCurrentFont(QFont("Arial", 10))
        panel_font_layout.addWidget(panel_font_combo)

        panel_font_size = QSpinBox()
        panel_font_size.setRange(8, 18)
        panel_font_size.setValue(getattr(self, 'panel_font', QFont("Arial", 10)).pointSize())
        panel_font_size.setSuffix(" pt")
        panel_font_size.setFixedWidth(80)
        panel_font_layout.addWidget(panel_font_size)

        panel_font_group.setLayout(panel_font_layout)
        fonts_layout.addWidget(panel_font_group)

        # Button Font
        button_font_group = QGroupBox("Button Font")
        button_font_layout = QHBoxLayout()

        button_font_combo = QFontComboBox()
        if hasattr(self, 'button_font'):
            button_font_combo.setCurrentFont(self.button_font)
        else:
            button_font_combo.setCurrentFont(QFont("Arial", 10))
        button_font_layout.addWidget(button_font_combo)

        button_font_size = QSpinBox()
        button_font_size.setRange(8, 16)
        button_font_size.setValue(getattr(self, 'button_font', QFont("Arial", 10)).pointSize())
        button_font_size.setSuffix(" pt")
        button_font_size.setFixedWidth(80)
        button_font_layout.addWidget(button_font_size)

        button_font_group.setLayout(button_font_layout)
        fonts_layout.addWidget(button_font_group)

        # Info Bar Font
        infobar_font_group = QGroupBox("Info Bar Font")
        infobar_font_layout = QHBoxLayout()

        infobar_font_combo = QFontComboBox()
        if hasattr(self, 'infobar_font'):
            infobar_font_combo.setCurrentFont(self.infobar_font)
        else:
            infobar_font_combo.setCurrentFont(QFont("Courier New", 9))
        infobar_font_layout.addWidget(infobar_font_combo)

        infobar_font_size = QSpinBox()
        infobar_font_size.setRange(7, 14)
        infobar_font_size.setValue(getattr(self, 'infobar_font', QFont("Courier New", 9)).pointSize())
        infobar_font_size.setSuffix(" pt")
        infobar_font_size.setFixedWidth(80)
        infobar_font_layout.addWidget(infobar_font_size)

        infobar_font_group.setLayout(infobar_font_layout)
        fonts_layout.addWidget(infobar_font_group)

        fonts_layout.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # TAB 2: DISPLAY SETTINGS

        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Button display mode
        button_group = QGroupBox("Button Display Mode")
        button_layout = QVBoxLayout()

        button_mode_combo = QComboBox()
        button_mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        current_mode = getattr(self, 'button_display_mode', 'both')
        mode_map = {'both': 0, 'icons': 1, 'text': 2}
        button_mode_combo.setCurrentIndex(mode_map.get(current_mode, 0))
        button_layout.addWidget(button_mode_combo)

        button_hint = QLabel("Changes how toolbar buttons are displayed")
        button_hint.setStyleSheet("color: #888; font-style: italic;")
        button_layout.addWidget(button_hint)

        button_group.setLayout(button_layout)
        display_layout.addWidget(button_group)

        # Table display
        table_group = QGroupBox("Surface List Display")
        table_layout = QVBoxLayout()

        show_thumbnails = QCheckBox("Show Surface types")
        show_thumbnails.setChecked(True)
        table_layout.addWidget(show_thumbnails)

        show_warnings = QCheckBox("Show warning icons for suspicious files")
        show_warnings.setChecked(True)
        show_warnings.setToolTip("Shows surface types")
        table_layout.addWidget(show_warnings)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")


        # TAB 3: placeholder
        # TAB 4: PERFORMANCE

        perf_tab = QWidget()
        perf_layout = QVBoxLayout(perf_tab)

        perf_group = QGroupBox("Performance Settings")
        perf_form = QFormLayout()

        preview_quality = QComboBox()
        preview_quality.addItems(["Low (Fast)", "Medium", "High (Slow)"])
        preview_quality.setCurrentIndex(1)
        perf_form.addRow("Preview Quality:", preview_quality)

        thumb_size = QSpinBox()
        thumb_size.setRange(32, 128)
        thumb_size.setValue(64)
        thumb_size.setSuffix(" px")
        perf_form.addRow("Thumbnail Size:", thumb_size)

        perf_group.setLayout(perf_form)
        perf_layout.addWidget(perf_group)

        # Caching
        cache_group = QGroupBox("Caching")
        cache_layout = QVBoxLayout()

        enable_cache = QCheckBox("Enable surface preview caching")
        enable_cache.setChecked(True)
        cache_layout.addWidget(enable_cache)

        cache_hint = QLabel("Caching improves performance but uses more memory")
        cache_hint.setStyleSheet("color: #888; font-style: italic;")
        cache_layout.addWidget(cache_hint)

        cache_group.setLayout(cache_layout)
        perf_layout.addWidget(cache_group)

        perf_layout.addStretch()
        tabs.addTab(perf_tab, "Performance")

        # TAB 5: PREVIEW SETTINGS (LAST TAB)

        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Zoom Settings
        zoom_group = QGroupBox("Zoom Settings")
        zoom_form = QFormLayout()

        zoom_spin = QSpinBox()
        zoom_spin.setRange(10, 500)
        zoom_spin.setValue(int(getattr(self, 'zoom_level', 1.0) * 100))
        zoom_spin.setSuffix("%")
        zoom_form.addRow("Default Zoom:", zoom_spin)

        zoom_group.setLayout(zoom_form)
        preview_layout.addWidget(zoom_group)

        # Background Settings
        bg_group = QGroupBox("Background Settings")
        bg_layout = QVBoxLayout()

        # Background mode
        bg_mode_layout = QFormLayout()
        bg_mode_combo = QComboBox()
        bg_mode_combo.addItems(["Solid Color", "Checkerboard", "Grid"])
        current_bg_mode = getattr(self, 'background_mode', 'solid')
        mode_idx = {"solid": 0, "checkerboard": 1, "checker": 1, "grid": 2}.get(current_bg_mode, 0)
        bg_mode_combo.setCurrentIndex(mode_idx)
        bg_mode_layout.addRow("Background Mode:", bg_mode_combo)
        bg_layout.addLayout(bg_mode_layout)

        bg_layout.addSpacing(10)

        # Checkerboard size
        cb_label = QLabel("Checkerboard Size:")
        bg_layout.addWidget(cb_label)

        cb_layout = QHBoxLayout()
        cb_slider = QSlider(Qt.Orientation.Horizontal)
        cb_slider.setMinimum(4)
        cb_slider.setMaximum(64)
        cb_slider.setValue(getattr(self, '_checkerboard_size', 16))
        cb_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        cb_slider.setTickInterval(8)
        cb_layout.addWidget(cb_slider)

        cb_spin = QSpinBox()
        cb_spin.setMinimum(4)
        cb_spin.setMaximum(64)
        cb_spin.setValue(getattr(self, '_checkerboard_size', 16))
        cb_spin.setSuffix(" px")
        cb_spin.setFixedWidth(80)
        cb_layout.addWidget(cb_spin)

        bg_layout.addLayout(cb_layout)

        # Connect checkerboard controls
        #cb_slider.valueChanged.connect(cb_spin.setValue)
        #cb_spin.valueChanged.connect(cb_slider.setValue)

        # Hint
        cb_hint = QLabel("Smaller = tighter pattern, larger = bigger squares")
        cb_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        bg_layout.addWidget(cb_hint)

        bg_group.setLayout(bg_layout)
        preview_layout.addWidget(bg_group)

        # Overlay Settings
        overlay_group = QGroupBox("Overlay View Settings")
        overlay_layout = QVBoxLayout()

        overlay_label = QLabel("Overlay Opacity (Wireframe over mesh):")
        overlay_layout.addWidget(overlay_label)

        opacity_layout = QHBoxLayout()
        opacity_slider = QSlider(Qt.Orientation.Horizontal)
        opacity_slider.setMinimum(0)
        opacity_slider.setMaximum(100)
        opacity_slider.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        opacity_slider.setTickInterval(10)
        opacity_layout.addWidget(opacity_slider)

        opacity_spin = QSpinBox()
        opacity_spin.setMinimum(0)
        opacity_spin.setMaximum(100)
        opacity_spin.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_spin.setSuffix(" %")
        opacity_spin.setFixedWidth(80)
        opacity_layout.addWidget(opacity_spin)

        overlay_layout.addLayout(opacity_layout)

        # Connect opacity controls
        #opacity_slider.valueChanged.connect(opacity_spin.setValue)
        #opacity_spin.valueChanged.connect(opacity_slider.setValue)

        # Hint
        opacity_hint = QLabel("0")
        opacity_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        overlay_layout.addWidget(opacity_hint)

        overlay_group.setLayout(overlay_layout)
        preview_layout.addWidget(overlay_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # Add tabs to dialog
        layout.addWidget(tabs)

        # BUTTONS

        btn_layout = QHBoxLayout()
        btn_layout.addStretch()

        # Apply button
        apply_btn = QPushButton("Apply Settings")
        apply_btn.setStyleSheet("""
            QPushButton {
                background: #0078d4;
                color: white;
                padding: 10px 24px;
                font-weight: bold;
                border-radius: 4px;
                font-size: 13px;
            }
            QPushButton:hover {
                background: #1984d8;
            }
        """)


        def apply_settings():
            # Adjusted for COL Wireframe, Mesh
            self.setFont(QFont(default_font_combo.currentFont().family(),
                            default_font_size.value()))
            self.title_font = QFont(title_font_combo.currentFont().family(),
                                title_font_size.value())
            self.panel_font = QFont(panel_font_combo.currentFont().family(),
                                panel_font_size.value())
            self.button_font = QFont(button_font_combo.currentFont().family(),
                                    button_font_size.value())
            self.infobar_font = QFont(infobar_font_combo.currentFont().family(),
                                    infobar_font_size.value())

            # Apply fonts to UI
            self._apply_title_font()
            self._apply_panel_font()
            self._apply_button_font()
            self._apply_infobar_font()

            mode_map = {0: 'both', 1: 'icons', 2: 'text'}
            self.button_display_mode = mode_map[button_mode_combo.currentIndex()]

            # EXPORT
            self.default_export_format = self.format_combo.currentText()

            # PREVIEW
            self.zoom_level = zoom_spin.value() / 100.0

            bg_modes = ['solid', 'checkerboard', 'grid']
            self.background_mode = bg_modes[bg_mode_combo.currentIndex()]

            self._checkerboard_size = cb_spin.value()
            self._overlay_opacity = opacity_spin.value()

            # Update preview widget
            if hasattr(self, 'preview_widget'):
                if self.background_mode == 'checkerboard':
                    self.preview_widget.set_checkerboard_background()
                    self.preview_widget._checkerboard_size = self._checkerboard_size
                else:
                    self.preview_widget.set_background_color(self.preview_widget.bg_color)

            # Apply button display mode
            if hasattr(self, '_update_all_buttons'):
                self._update_all_buttons()

            # Refresh display

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Workshop settings updated successfully")

        apply_btn.clicked.connect(apply_settings)
        btn_layout.addWidget(apply_btn)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.setStyleSheet("padding: 10px 24px; font-size: 13px;")
        close_btn.clicked.connect(dialog.close)
        btn_layout.addWidget(close_btn)

        layout.addLayout(btn_layout)

        # Show dialog
        dialog.exec()


    def _apply_window_flags(self): #vers 1
        """Apply window flags based on settings"""
        # Save current geometry
        current_geometry = self.geometry()
        was_visible = self.isVisible()

        if self.use_system_titlebar:
            # Use system window with title bar
            self.setWindowFlags(
                Qt.WindowType.Window |
                Qt.WindowType.WindowMinimizeButtonHint |
                Qt.WindowType.WindowMaximizeButtonHint |
                Qt.WindowType.WindowCloseButtonHint
            )
        else:
            # Use custom frameless window
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        # Restore geometry and visibility
        self.setGeometry(current_geometry)

        if was_visible:
            self.show()

        if self.main_window and hasattr(self.main_window, 'log_message'):
            mode = "System title bar" if self.use_system_titlebar else "Custom frameless"
            self.main_window.log_message(f"Window mode: {mode}")


    def _apply_always_on_top(self): #vers 1
        """Apply always on top window flag"""
        current_flags = self.windowFlags()

        if self.window_always_on_top:
            new_flags = current_flags | Qt.WindowType.WindowStaysOnTopHint
        else:
            new_flags = current_flags & ~Qt.WindowType.WindowStaysOnTopHint

        if new_flags != current_flags:
            # Save state
            current_geometry = self.geometry()
            was_visible = self.isVisible()

            self.setWindowFlags(new_flags)

            self.setGeometry(current_geometry)
            if was_visible:
                self.show()


    def _scan_available_locales(self): #vers 2
        """Scan locale folder and return list of available languages"""
        import os
        import configparser

        locales = []
        locale_path = os.path.join(os.path.dirname(__file__), 'locale')

        if not os.path.exists(locale_path):
            # Easter egg: Amiga Workbench 3.1 style error
            self._show_amiga_locale_error()
            # Return default English
            return [("English", "en", None)]

        try:
            for filename in os.listdir(locale_path):
                if filename.endswith('.lang'):
                    filepath = os.path.join(locale_path, filename)

                    try:
                        config = configparser.ConfigParser()
                        config.read(filepath, encoding='utf-8')

                        if 'Metadata' in config:
                            lang_name = config['Metadata'].get('LanguageName', 'Unknown')
                            lang_code = config['Metadata'].get('LanguageCode', 'unknown')
                            locales.append((lang_name, lang_code, filepath))

                    except Exception as e:
                        if self.main_window and hasattr(self.main_window, 'log_message'):
                            self.main_window.log_message(f"Failed to load locale {filename}: {e}")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Locale scan error: {e}")

        locales.sort(key=lambda x: x[0])

        if not locales:
            locales = [("English", "en", None)]

        return locales


    def _show_amiga_locale_error(self): #vers 2
        """Show Amiga Workbench 3.1 style error dialog"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QHBoxLayout
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle("Workbench Request")
        dialog.setFixedSize(450, 150)

        # Amiga Workbench styling
        dialog.setStyleSheet("""
            QDialog {
                background-color: #aaaaaa;
                border: 2px solid #ffffff;
            }
            QLabel {
                color: #000000;
                background-color: #aaaaaa;
            }
            QPushButton {
                background-color: #8899aa;
                color: #000000;
                border: 2px outset #ffffff;
                padding: 5px 15px;
                min-width: 80px;
            }
            QPushButton:pressed {
                border: 2px inset #555555;
            }
        """)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)
        layout.setContentsMargins(20, 20, 20, 20)

        # Amiga Topaz font style
        amiga_font = QFont("Courier", 10, QFont.Weight.Normal)

        # Error message
        message = QLabel("Workbench 3.1 installer\n\nPlease insert Local disk in any drive")
        message.setFont(amiga_font)
        message.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(message)

        layout.addStretch()

        # Button layout
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        # Retry and Cancel buttons (Amiga style)
        retry_btn = QPushButton("Retry")
        retry_btn.setFont(amiga_font)
        retry_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(retry_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.setFont(amiga_font)
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        button_layout.addStretch()
        layout.addLayout(button_layout)

        dialog.exec()


# - Docking functions

    def _update_dock_button_visibility(self): #vers 2
        """Show/hide dock and tearoff buttons based on docked state"""
        if hasattr(self, 'dock_btn'):
            # Hide D button when docked, show when standalone
            self.dock_btn.setVisible(not self.is_docked)

        if hasattr(self, 'tearoff_btn'):
            # T button only visible when docked and not in standalone mode
            self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)


    def toggle_dock_mode(self): #vers 2
        """Toggle between docked and standalone mode"""
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

        self._update_dock_button_visibility()


    def _dock_to_main(self): #vers 9
        """Dock handled by overlay system in imgfactory - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                self.show()
                self.raise_()
                return

            # For proper docking, we need to be called from imgfactory
            # This method should be handled by imgfactory's overlay system
            if self.main_window and hasattr(self.main_window, App_name + '_docked'):
                # If available, use the main window's docking system
                self.main_window.open_col_workshop_docked()
            else:
                # Fallback: just show the window
                self.show()
                self.raise_()

            # Update dock state
            self.is_docked = True
            self._update_dock_button_visibility()
            if hasattr(self, '_middle_btn_row'):
                self._middle_btn_row.setVisible(True)

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} docked to main window")


        except Exception as e:
            print(f"Error docking: {str(e)}")
            self.show()


    def _undock_from_main(self): #vers 4
        """Undock from overlay mode to standalone window - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                # Switch from overlay to normal window
                self.setWindowFlags(Qt.WindowType.Window)
                self.is_overlay = False
                self.overlay_table = None

            # Set proper window flags for standalone mode
            self.setWindowFlags(Qt.WindowType.Window)
            
            # Ensure proper size when undocking
            if hasattr(self, 'original_size'):
                self.resize(self.original_size)
            else:
                self.resize(1000, 700)  # Reasonable default size
                
            self.is_docked = False
            self._update_dock_button_visibility()
            if hasattr(self, '_middle_btn_row'):
                self._middle_btn_row.setVisible(False)

            self.show()
            self.raise_()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} undocked to standalone")
                
        except Exception as e:
            print(f"Error undocking: {str(e)}")
            # Fallback
            self.setWindowFlags(Qt.WindowType.Window)
            self.show()


    def _apply_button_mode(self, dialog): #vers 1
        """Apply button display mode"""
        mode_index = self.button_mode_combo.currentIndex()
        mode_map = {0: 'both', 1: 'icons', 2: 'text'}

        new_mode = mode_map[mode_index]

        if new_mode != self.button_display_mode:
            self.button_display_mode = new_mode
            self._update_all_buttons()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                mode_names = {0: 'Icons + Text', 1: 'Icons Only', 2: 'Text Only'}
                self.main_window.log_message(f"✨ Button style: {mode_names[mode_index]}")

        dialog.close()


# - Window functionality

    def _initialize_features(self): #vers 3
        """Initialize all features after UI setup"""
        try:
            self._apply_theme()
            self._update_status_indicators()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("All features initialized")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Feature init error: {str(e)}")


    def _is_on_draggable_area(self, pos): #vers 7
        """Check if position is on draggable titlebar area

        Args:
            pos: Position in titlebar coordinates (from eventFilter)

        Returns:
            True if position is on titlebar but not on any button
        """
        if not hasattr(self, 'titlebar'):
            print("[DRAG] No titlebar attribute")
            return False

        # Verify pos is within titlebar bounds
        if not self.titlebar.rect().contains(pos):
            print(f"[DRAG] Position {pos} outside titlebar rect {self.titlebar.rect()}")
            return False

        # Check if clicking on any button - if so, NOT draggable
        for widget in self.titlebar.findChildren(QPushButton):
            if widget.isVisible():
                # Get button geometry in titlebar coordinates
                button_rect = widget.geometry()
                if button_rect.contains(pos):
                    print(f"[DRAG] Clicked on button: {widget.toolTip()}")
                    return False

        # Not on any button = draggable
        print(f"[DRAG] On draggable area at {pos}")
        return True


# - From the fixed gui - move, drag

    def _update_all_buttons(self): #vers 4
        """Update all buttons to match display mode"""
        buttons_to_update = [
            # Toolbar buttons
            ('open_btn', 'Open'),
            ('save_btn', 'Save'),
            ('save_col_btn', 'Save TXD'),
        ]

        # Adjust transform panel width based on mode
        if hasattr(self, 'transform_icon_panel'):
            if self.button_display_mode == 'icons':
                self.transform_icon_panel.setMaximumWidth(50)
            else:
                self.transform_text_panel.setMaximumWidth(200)

        for btn_name, btn_text in buttons_to_update:
            if hasattr(self, btn_name):
                button = getattr(self, btn_name)
                self._apply_button_mode_to_button(button, btn_text)
        self._update_dock_button_visibility()

    def paintEvent(self, event): #vers 2
        """Paint corner resize triangles"""
        super().paintEvent(event)

        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QPainterPath

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Colors
        normal_color = QColor(100, 100, 100, 150)
        hover_color = QColor(150, 150, 255, 200)

        w = self.width()
        h = self.height()
        grip_size = 8  # Make corners visible (8x8px)
        size = self.corner_size

        # Define corner triangles
        corners = {
            'top-left': [(0, 0), (size, 0), (0, size)],
            'top-right': [(w, 0), (w-size, 0), (w, size)],
            'bottom-left': [(0, h), (size, h), (0, h-size)],
            'bottom-right': [(w, h), (w-size, h), (w, h-size)]
        }
        corners2 = {
            "top-left": [(0, grip_size), (0, 0), (grip_size, 0)],
            "top-right": [(w-grip_size, 0), (w, 0), (w, grip_size)],
            "bottom-left": [(0, h-grip_size), (0, h), (grip_size, h)],
            "bottom-right": [(w-grip_size, h), (w, h), (w, h-grip_size)]
        }

        # Get theme colors for corner indicators
        if self.app_settings:
            theme_colors = self.app_settings.get_theme_colors()
            accent_color = QColor(theme_colors.get('accent_primary', '#1976d2'))
            accent_color.setAlpha(180)
        else:
            accent_color = QColor(100, 150, 255, 180)

        hover_color = QColor(accent_color)
        hover_color.setAlpha(255)

        # Draw all corners with hover effect
        for corner_name, points in corners.items():
            path = QPainterPath()
            path.moveTo(points[0][0], points[0][1])
            path.lineTo(points[1][0], points[1][1])
            path.lineTo(points[2][0], points[2][1])
            path.closeSubpath()

            # Use hover color if mouse is over this corner
            color = hover_color if self.hover_corner == corner_name else accent_color

            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(color))
            painter.drawPath(path)

        painter.end()


    def _get_resize_corner(self, pos): #vers 3
        """Determine which corner is under mouse position"""
        size = self.corner_size; w = self.width(); h = self.height()

        if pos.x() < size and pos.y() < size:
            return "top-left"
        if pos.x() > w - size and pos.y() < size:
            return "top-right"
        if pos.x() < size and pos.y() > h - size:
            return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:
            return "bottom-right"

        return None


    def mousePressEvent(self, event): #vers 8
        """Handle ALL mouse press - dragging and resizing"""
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        pos = event.pos()

        # Check corner resize FIRST
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept()
            return

        # Check if on titlebar
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            titlebar_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(titlebar_pos):
                handle = self.windowHandle()
                if handle:
                    handle.startSystemMove()
                event.accept()
                return

        super().mousePressEvent(event)


    def mouseMoveEvent(self, event): #vers 4
        """Handle mouse move for resizing and hover effects

        Window dragging is handled by eventFilter to avoid conflicts
        """
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept()
                return
        else:
            # Update hover state and cursor
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()  # Trigger repaint for hover effect
            self._update_cursor(corner)

        # Let parent handle everything else
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #vers 2
        """Handle mouse release"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = False
            self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #vers 2
        """Handle window resizing from corners"""
        if not self.resize_corner or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.initial_geometry

        min_width = 800
        min_height = 600

        # Calculate new geometry based on corner
        if self.resize_corner == "top-left":
            new_x = geometry.x() + delta.x()
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, new_y, new_width, new_height)

        elif self.resize_corner == "top-right":
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(geometry.x(), new_y, new_width, new_height)

        elif self.resize_corner == "bottom-left":
            new_x = geometry.x() + delta.x()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, geometry.y(), new_width, new_height)

        elif self.resize_corner == "bottom-right":
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.resize(new_width, new_height)


    def _get_resize_direction(self, pos): #vers 1
        """Determine resize direction based on mouse position"""
        rect = self.rect()
        margin = self.resize_margin

        left = pos.x() < margin
        right = pos.x() > rect.width() - margin
        top = pos.y() < margin
        bottom = pos.y() > rect.height() - margin

        if left and top:
            return "top-left"
        elif right and top:
            return "top-right"
        elif left and bottom:
            return "bottom-left"
        elif right and bottom:
            return "bottom-right"
        elif left:
            return "left"
        elif right:
            return "right"
        elif top:
            return "top"
        elif bottom:
            return "bottom"

        return None


    def _update_cursor(self, direction): #vers 1
        """Update cursor based on resize direction"""
        if direction == "top" or direction == "bottom":
            self.setCursor(Qt.CursorShape.SizeVerCursor)
        elif direction == "left" or direction == "right":
            self.setCursor(Qt.CursorShape.SizeHorCursor)
        elif direction == "top-left" or direction == "bottom-right":
            self.setCursor(Qt.CursorShape.SizeFDiagCursor)
        elif direction == "top-right" or direction == "bottom-left":
            self.setCursor(Qt.CursorShape.SizeBDiagCursor)
        else:
            self.setCursor(Qt.CursorShape.ArrowCursor)


    def _handle_resize(self, global_pos): #vers 1
        """Handle window resizing"""
        if not self.resize_direction or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.frameGeometry()

        min_width = 800
        min_height = 600

        # Handle horizontal resizing
        if "left" in self.resize_direction:
            new_width = geometry.width() - delta.x()
            if new_width >= min_width:
                geometry.setLeft(geometry.left() + delta.x())
        elif "right" in self.resize_direction:
            new_width = geometry.width() + delta.x()
            if new_width >= min_width:
                geometry.setRight(geometry.right() + delta.x())

        # Handle vertical resizing
        if "top" in self.resize_direction:
            new_height = geometry.height() - delta.y()
            if new_height >= min_height:
                geometry.setTop(geometry.top() + delta.y())
        elif "bottom" in self.resize_direction:
            new_height = geometry.height() + delta.y()
            if new_height >= min_height:
                geometry.setBottom(geometry.bottom() + delta.y())

        self.setGeometry(geometry)
        self.drag_position = global_pos


    def resizeEvent(self, event): #vers 2
        """Update layout on resize."""
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)
        self._update_transform_text_panel_visibility()

    def _on_splitter_moved(self, pos, index): #vers 1
        """Called when main splitter is dragged — update text panel visibility."""
        self._update_transform_text_panel_visibility()

    def _update_transform_text_panel_visibility(self): #vers 2
        """Toggle between text+icon panel (wide) and icon-only strip (narrow).
        Reads threshold from IMG Factory settings. Also collapses bottom buttons."""
        tp   = getattr(self, '_transform_text_panel_ref', None)
        ip   = getattr(self, '_transform_icon_panel_ref', None)
        mode = getattr(self, 'button_display_mode', 'both')

        if mode == 'icons':
            if tp: tp.setVisible(False)
            if ip: ip.setVisible(True)
            return
        if mode == 'text':
            if tp: tp.setVisible(True)
            if ip: ip.setVisible(False)
            return

        # Measure right panel width directly
        rp = getattr(self, '_right_panel_ref', None)
        if rp:
            ref_w = rp.width()
        else:
            splitter = getattr(self, '_main_splitter', None)
            ref_w = self.width()
            if splitter and tp:
                w = tp
                while w and w.parent() is not splitter:
                    w = w.parent() if hasattr(w, 'parent') else None
                if w:
                    ref_w = w.width()

        try:
            from apps.methods.imgfactory_ui_settings import get_collapse_threshold
            threshold = get_collapse_threshold(getattr(self, 'main_window', None))
        except Exception:
            threshold = 550
        wide = ref_w >= threshold
        if tp: tp.setVisible(wide)
        if ip: ip.setVisible(not wide)

        # Toggle bottom panel rows the same way
        btr = getattr(self, '_bottom_text_row', None)
        bir = getattr(self, '_bottom_icon_row', None)
        if btr: btr.setVisible(wide)
        if bir: bir.setVisible(not wide)

    def resizeEvent(self, event): #vers 3
        """Keep resize grip in corner; auto-collapse text transform panel when narrow."""
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)
        self._update_transform_text_panel_visibility()


    def mouseDoubleClickEvent(self, event): #vers 2
        """Handle double-click - maximize/restore

        Handled here instead of eventFilter for better control
        """
        if event.button() == Qt.MouseButton.LeftButton:
            # Convert to titlebar coordinates if needed
            if hasattr(self, 'titlebar'):
                titlebar_pos = self.titlebar.mapFromParent(event.pos())
                if self._is_on_draggable_area(titlebar_pos):
                    self._toggle_maximize()
                    event.accept()
                    return

        super().mouseDoubleClickEvent(event)

# - Marker 3

    def _toggle_maximize(self): #vers 1
        """Toggle window maximize state"""
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()


    def closeEvent(self, event): #vers 1
        """Handle close event"""
        try:
            for attr in ('_col_left_toolbar', '_col_right_toolbar'):
                tb = getattr(self, attr, None)
                if tb and hasattr(tb, 'save_layout'):
                    tb.save_layout()
        except Exception:
            pass
        self.window_closed.emit()
        event.accept()


# - Panel Setup

    def _create_toolbar(self): #vers 13
        """Create toolbar - FIXED: Hide drag button when docked, ensure buttons visible"""
        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")

        # Install event filter for drag detection
        self.titlebar.installEventFilter(self)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        self.titlebar_layout = QHBoxLayout(self.titlebar)
        self.titlebar_layout.setContentsMargins(5, 5, 5, 5)
        self.titlebar_layout.setSpacing(5)

        # Get icon color from theme
        icon_color = self._get_icon_color()

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(50)

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(5)

        # Settings button
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(self.icon_factory.settings_icon(color=icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("Workshop Settings")
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # App title in center
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self.title_label)

        layout.addStretch()
        #layout.addStretch()

        # Only show "Open IMG" button if NOT standalone
        if not self.standalone_mode:
            self.open_img_btn = QPushButton("OpenIMG")
            self.open_img_btn.setFont(self.button_font)
            self.open_img_btn.setIcon(self.icon_factory.folder_icon(color=icon_color))
            self.open_img_btn.setIconSize(QSize(20, 20))
            self.open_img_btn.clicked.connect(self.open_img_archive)
            self.open_img_btn.setToolTip("Open an IMG archive and browse its COL entries")
            layout.addWidget(self.open_img_btn)

            # "From IMG" — pick a COL entry from the currently loaded IMG in IMG Factory
            self.from_img_btn = QPushButton("From IMG")
            self.from_img_btn.setFont(self.button_font)
            self.from_img_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
            self.from_img_btn.setIconSize(QSize(20, 20))
            self.from_img_btn.clicked.connect(self._pick_col_from_current_img)
            self.from_img_btn.setToolTip("Pick a COL entry from the currently loaded IMG")
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
        self.open_btn.setToolTip("Open COL file (Ctrl+O)")
        self.open_btn.clicked.connect(self._open_file)
        layout.addWidget(self.open_btn)

        # Save button
        self.save_btn = QPushButton()
        self.save_btn.setFont(self.button_font)
        self.save_btn.setIcon(self.icon_factory.save_icon(color=icon_color))
        self.save_btn.setText("Save")
        self.save_btn.setIconSize(QSize(20, 20))
        self.save_btn.setShortcut("Ctrl+S")
        if self.button_display_mode == 'icons':
            self.save_btn.setFixedSize(40, 40)
        self.save_btn.setEnabled(True)
        self.save_btn.setToolTip("Save COL file (Ctrl+S)")
        self.save_btn.clicked.connect(self._save_file)
        layout.addWidget(self.save_btn)

        # Save button
        self.saveall_btn = QPushButton()
        self.saveall_btn.setFont(self.button_font)
        self.saveall_btn.setIcon(self.icon_factory.saveas_icon(color=icon_color))
        self.saveall_btn.setText("Save All")
        self.saveall_btn.setIconSize(QSize(20, 20))
        self.saveall_btn.setShortcut("Ctrl+S")
        if self.button_display_mode == 'icons':
            self.saveall_btn.setFixedSize(40, 40)
        self.saveall_btn.setEnabled(True)
        self.saveall_btn.setToolTip("Save COL file (Ctrl+S)")
        self.saveall_btn.clicked.connect(self._saveall_file)
        #layout.addWidget(self.saveall_btn)

        self.export_all_btn = QPushButton("Extract")
        self.export_all_btn.setFont(self.button_font)
        self.export_all_btn.setIcon(self.icon_factory.package_icon(color=icon_color))
        self.export_all_btn.setIconSize(QSize(20, 20))
        self.export_all_btn.setToolTip("Export all as col, cst or 3ds files")
        self.export_all_btn.clicked.connect(self.export_all)
        self.export_all_btn.setEnabled(True)
        layout.addWidget(self.export_all_btn)

        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undo_icon(color=icon_color))
        self.undo_btn.setText("Undo")
        self.undo_btn.setIconSize(QSize(20, 20))
        self.undo_btn.clicked.connect(self._undo_last_action)
        self.undo_btn.setEnabled(True)
        self.undo_btn.setToolTip("Undo last change")
        layout.addWidget(self.undo_btn)

        # Info button
        self.info_btn = QPushButton("")
        self.info_btn.setText("")  # CHANGED from "Info"
        self.info_btn.setIcon(self.icon_factory.info_icon(color=icon_color))
        self.info_btn.setMinimumWidth(40)
        self.info_btn.setMaximumWidth(40)
        self.info_btn.setMinimumHeight(30)
        self.info_btn.setToolTip("Information")

        self.info_btn.setIconSize(QSize(20, 20))
        self.info_btn.setFixedWidth(35)
        self.info_btn.clicked.connect(self._show_col_info)
        layout.addWidget(self.info_btn)

        # Properties/Theme button
        self.properties_btn = QPushButton()
        self.properties_btn.setFont(self.button_font)
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(24, icon_color))
        self.properties_btn.setToolTip("Theme")
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        self.properties_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.properties_btn.customContextMenuRequested.connect(self._show_settings_context_menu)
        layout.addWidget(self.properties_btn)

        # Dock button [D]
        self.dock_btn = QPushButton("D")
        #self.dock_btn.setFont(self.button_font)
        self.dock_btn.setMinimumWidth(40)
        self.dock_btn.setMaximumWidth(40)
        self.dock_btn.setMinimumHeight(30)
        self.dock_btn.setToolTip("Dock")

        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        layout.addWidget(self.dock_btn)

        # Tear-off button [T] - only in IMG Factory mode
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton("T")
            #self.tearoff_btn.setFont(self.button_font)
            self.tearoff_btn.setMinimumWidth(40)
            self.tearoff_btn.setMaximumWidth(40)
            self.tearoff_btn.setMinimumHeight(30)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("TXD Workshop - Tearoff window")

            layout.addWidget(self.tearoff_btn)

        # Window controls
        self.minimize_btn = QPushButton()
        self.minimize_btn.setIcon(self.icon_factory.minimize_icon(color=icon_color))
        self.minimize_btn.setIconSize(QSize(20, 20))
        self.minimize_btn.setMinimumWidth(40)
        self.minimize_btn.setMaximumWidth(40)
        self.minimize_btn.setMinimumHeight(30)
        self.minimize_btn.clicked.connect(self.showMinimized)
        self.minimize_btn.setToolTip("Minimize Window") # click tab to restore
        layout.addWidget(self.minimize_btn)

        self.maximize_btn = QPushButton()
        self.maximize_btn.setIcon(self.icon_factory.maximize_icon(color=icon_color))
        self.maximize_btn.setIconSize(QSize(20, 20))
        self.maximize_btn.setMinimumWidth(40)
        self.maximize_btn.setMaximumWidth(40)
        self.maximize_btn.setMinimumHeight(30)
        self.maximize_btn.clicked.connect(self._toggle_maximize)
        self.maximize_btn.setToolTip("Maximize/Restore Window")
        layout.addWidget(self.maximize_btn)

        self.close_btn = QPushButton()
        self.close_btn.setIcon(self.icon_factory.close_icon(color=icon_color))
        self.close_btn.setIconSize(QSize(20, 20))
        self.close_btn.setMinimumWidth(40)
        self.close_btn.setMaximumWidth(40)
        self.close_btn.setMinimumHeight(30)
        self.close_btn.clicked.connect(self.close)
        self.close_btn.setToolTip("Close Window") # closes tab
        layout.addWidget(self.close_btn)

        return self.toolbar

    #Left side vertical panel
    def _create_transform_icon_panel(self): #vers 13
        """Icon toolbar — returns QFrame with QGridLayout.
        Buttons are registered in self._col_icon_buttons for grid reflow."""
        from PyQt6.QtWidgets import QGridLayout
        icon_color = self._get_icon_color()

        icon_frame = QFrame()
        icon_frame.setFrameStyle(QFrame.Shape.NoFrame)
        grid = QGridLayout(icon_frame)
        grid.setContentsMargins(0, 0, 0, 0)
        grid.setSpacing(2)

        self._col_icon_grid    = grid
        self._col_icon_buttons = []
        self._col_icon_frame   = icon_frame
        # Keep legacy ref pointing to the frame
        self.transform_icon_panel = icon_frame

        btn_height = 26
        btn_width  = 26
        icon_size  = QSize(16, 16)

        # Fake layout — collects buttons into self._col_icon_buttons
        class _FakeLayout:
            def addWidget(_, w, *a, **kw):
                self._col_icon_buttons.append(w)
            def addSpacing(_, *a): pass
            def addStretch(_): pass

        spacer = 0   # no-op — grid handles spacing
        layout = _FakeLayout()
        layout.addSpacing(0)

        # Flip Vertical
        self.flip_vert_btn = QPushButton()
        self.flip_vert_btn.setIcon(self.icon_factory.flip_vert_icon(color=icon_color))
        self.flip_vert_btn.setIconSize(icon_size)
        self.flip_vert_btn.setFixedHeight(btn_height)
        self.flip_vert_btn.setMinimumWidth(btn_width)
        self.flip_vert_btn.setEnabled(False)
        self.flip_vert_btn.setToolTip("Flip col vertically")
        self.flip_vert_btn.clicked.connect(lambda: getattr(self,"preview_widget",None) and self.preview_widget.flip_vertical())
        layout.addWidget(self.flip_vert_btn)
        layout.addSpacing(spacer)

        # Flip Horizontal
        self.flip_horz_btn = QPushButton()
        self.flip_horz_btn.setIcon(self.icon_factory.flip_horz_icon(color=icon_color))
        self.flip_horz_btn.setIconSize(icon_size)
        self.flip_horz_btn.setFixedHeight(btn_height)
        self.flip_horz_btn.setMinimumWidth(btn_width)
        self.flip_horz_btn.setEnabled(False)
        self.flip_horz_btn.setToolTip("Flip col horizontally")
        self.flip_horz_btn.clicked.connect(lambda: getattr(self,"preview_widget",None) and self.preview_widget.flip_horizontal())
        layout.addWidget(self.flip_horz_btn)
        layout.addSpacing(spacer)

        # Rotate Clockwise
        self.rotate_cw_btn = QPushButton()
        self.rotate_cw_btn.setIcon(self.icon_factory.rotate_cw_icon(color=icon_color))
        self.rotate_cw_btn.setIconSize(icon_size)
        self.rotate_cw_btn.setFixedHeight(btn_height)
        self.rotate_cw_btn.setMinimumWidth(btn_width)
        self.rotate_cw_btn.setEnabled(False)
        self.rotate_cw_btn.setToolTip("Rotate 90 degrees clockwise")
        self.rotate_cw_btn.clicked.connect(lambda: getattr(self,"preview_widget",None) and self.preview_widget.rotate_cw())
        layout.addWidget(self.rotate_cw_btn)
        layout.addSpacing(spacer)

        # Rotate Counter-Clockwise
        self.rotate_ccw_btn = QPushButton()
        self.rotate_ccw_btn.setIcon(self.icon_factory.rotate_ccw_icon(color=icon_color))
        self.rotate_ccw_btn.setIconSize(icon_size)
        self.rotate_ccw_btn.setFixedHeight(btn_height)
        self.rotate_ccw_btn.setMinimumWidth(btn_width)
        self.rotate_ccw_btn.setEnabled(False)
        self.rotate_ccw_btn.setToolTip("Rotate 90 degrees counter-clockwise")
        self.rotate_ccw_btn.clicked.connect(lambda: getattr(self,"preview_widget",None) and self.preview_widget.rotate_ccw())
        layout.addWidget(self.rotate_ccw_btn)
        layout.addSpacing(spacer)

        # Analyze
        self.analyze_btn = QPushButton()
        self.analyze_btn.setIcon(self.icon_factory.analyze_icon(color=icon_color))
        self.analyze_btn.setIconSize(icon_size)
        self.analyze_btn.setFixedHeight(btn_height)
        self.analyze_btn.setMinimumWidth(btn_width)
        self.analyze_btn.clicked.connect(self._analyze_collision)
        self.analyze_btn.setEnabled(False)
        self.analyze_btn.setToolTip("Analyze collision data")
        layout.addWidget(self.analyze_btn)
        layout.addSpacing(spacer)

        # Copy
        self.copy_btn = QPushButton()
        self.copy_btn.setIcon(self.icon_factory.copy_icon(color=icon_color))
        self.copy_btn.setIconSize(icon_size)
        self.copy_btn.setFixedHeight(btn_height)
        self.copy_btn.setMinimumWidth(btn_width)
        self.copy_btn.setEnabled(False)
        self.copy_btn.setToolTip("Copy col to clipboard")
        self.copy_btn.clicked.connect(self._copy_surface)
        layout.addWidget(self.copy_btn)
        layout.addSpacing(spacer)

        # Paste
        self.paste_btn = QPushButton()
        self.paste_btn.setIcon(self.icon_factory.paste_icon(color=icon_color))
        self.paste_btn.setIconSize(icon_size)
        self.paste_btn.setFixedHeight(btn_height)
        self.paste_btn.setMinimumWidth(btn_width)
        self.paste_btn.setEnabled(False)
        self.paste_btn.setToolTip("Paste col from clipboard")
        self.paste_btn.clicked.connect(self._paste_surface)
        layout.addWidget(self.paste_btn)
        layout.addSpacing(spacer)

        # Create
        self.create_surface_btn = QPushButton()
        self.create_surface_btn.setIcon(self.icon_factory.add_icon(color=icon_color))
        self.create_surface_btn.setIconSize(icon_size)
        self.create_surface_btn.setFixedHeight(btn_height)
        self.create_surface_btn.setMinimumWidth(btn_width)
        self.create_surface_btn.setToolTip("Create new blank Collision")
        self.create_surface_btn.clicked.connect(self._create_new_surface)
        layout.addWidget(self.create_surface_btn)
        layout.addSpacing(spacer)

        # Delete
        self.delete_surface_btn = QPushButton()
        self.delete_surface_btn.setIcon(self.icon_factory.delete_icon(color=icon_color))
        self.delete_surface_btn.setIconSize(icon_size)
        self.delete_surface_btn.setFixedHeight(btn_height)
        self.delete_surface_btn.setMinimumWidth(btn_width)
        self.delete_surface_btn.setEnabled(False)
        self.delete_surface_btn.setToolTip("Remove selected Collision")
        self.delete_surface_btn.clicked.connect(self._delete_surface)
        layout.addWidget(self.delete_surface_btn)
        layout.addSpacing(spacer)

        # Duplicate
        self.duplicate_surface_btn = QPushButton()
        self.duplicate_surface_btn.setIcon(self.icon_factory.duplicate_icon(color=icon_color))
        self.duplicate_surface_btn.setIconSize(icon_size)
        self.duplicate_surface_btn.setFixedHeight(btn_height)
        self.duplicate_surface_btn.setMinimumWidth(btn_width)
        self.duplicate_surface_btn.setEnabled(False)
        self.duplicate_surface_btn.setToolTip("Clone selected Collision")
        self.duplicate_surface_btn.clicked.connect(self._duplicate_surface)
        layout.addWidget(self.duplicate_surface_btn)
        layout.addSpacing(spacer)

        # Paint
        self.paint_btn = QPushButton()
        self.paint_btn.setIcon(self.icon_factory.paint_icon(color=icon_color))
        self.paint_btn.setIconSize(icon_size)
        self.paint_btn.setFixedHeight(btn_height)
        self.paint_btn.setMinimumWidth(btn_width)
        self.paint_btn.setEnabled(False)
        self.paint_btn.setToolTip("Paint free hand on surface — assign materials")
        self.paint_btn.clicked.connect(self._open_paint_editor)
        layout.addWidget(self.paint_btn)
        layout.addSpacing(spacer)

        # Surface Type
        self.surface_type_btn = QPushButton()
        self.surface_type_btn.setIcon(self.icon_factory.checkerboard_icon(color=icon_color))
        self.surface_type_btn.setIconSize(icon_size)
        self.surface_type_btn.setFixedHeight(btn_height)
        self.surface_type_btn.setMinimumWidth(btn_width)
        self.surface_type_btn.setToolTip("Surface types")
        self.surface_type_btn.clicked.connect(self._open_surface_type_dialog)
        layout.addWidget(self.surface_type_btn)
        layout.addSpacing(spacer)

        # Surface Edit
        self.surface_edit_btn = QPushButton()
        self.surface_edit_btn.setIcon(self.icon_factory.surfaceedit_icon(color=icon_color))
        self.surface_edit_btn.setIconSize(icon_size)
        self.surface_edit_btn.setFixedHeight(btn_height)
        self.surface_edit_btn.setMinimumWidth(btn_width)
        self.surface_edit_btn.setToolTip("Surface Editor — edit mesh faces and vertices")
        self.surface_edit_btn.clicked.connect(self._open_surface_edit_dialog)
        layout.addWidget(self.surface_edit_btn)
        layout.addSpacing(spacer)

        # Build from TXD
        self.build_from_txd_btn = QPushButton()
        self.build_from_txd_btn.setIcon(self.icon_factory.build_icon(color=icon_color))
        self.build_from_txd_btn.setIconSize(icon_size)
        self.build_from_txd_btn.setFixedHeight(btn_height)
        self.build_from_txd_btn.setMinimumWidth(btn_width)
        self.build_from_txd_btn.setToolTip("Create col surface from txd texture names")
        self.build_from_txd_btn.clicked.connect(self._build_col_from_txd)
        layout.addWidget(self.build_from_txd_btn)

        # Place buttons into grid and do initial reflow
        self._col_icon_buttons_initial_place()
        return icon_frame

    def _col_icon_buttons_initial_place(self): #vers 1
        """Initial single-row placement; reflow_requested signal handles later calls."""
        self._reflow_col_left_toolbar('top')

    def _reflow_col_left_toolbar(self, pos: str): #vers 1
        """Reflow left toolbar icons based on dock position."""
        from apps.components.Col_Editor.dockable_toolbar import SNAP_LEFT, SNAP_RIGHT
        grid  = getattr(self, '_col_icon_grid', None)
        btns  = getattr(self, '_col_icon_buttons', [])
        frame = getattr(self, '_col_icon_frame', None)
        if not grid or not btns:
            return
        n = len(btns)
        btn_w = 28

        if pos == 'float':
            n_cols = n          # single row
        elif pos in (SNAP_LEFT, SNAP_RIGHT):
            n_cols = 1          # single column
        else:
            # Fill available width
            pw = frame.width() if frame else 0
            n_cols = max(1, pw // btn_w) if pw > 0 else n

        # Clear grid
        for i in range(grid.count() - 1, -1, -1):
            item = grid.itemAt(i)
            if item and item.widget():
                grid.removeWidget(item.widget())
        # Place
        for idx, btn in enumerate(btns):
            grid.addWidget(btn, idx // n_cols, idx % n_cols)

        # Resize buttons to compact size
        for btn in btns:
            btn.setFixedSize(26, 26)

        # Constrain frame width when single column
        if frame:
            if n_cols == 1:
                frame.setFixedWidth(26 + 4)
            else:
                frame.setMinimumWidth(0)
                frame.setMaximumWidth(16777215)


    def _create_transform_text_panel(self): #vers 12
        """Create transform panel with text - aligned with icon panel"""
        self.transform_text_panel = QFrame()
        self.transform_text_panel.setFrameStyle(QFrame.Shape.StyledPanel)
        self.transform_text_panel.setMinimumWidth(140)
        self.transform_text_panel.setMaximumWidth(140)

        layout = QVBoxLayout(self.transform_text_panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(1)

        btn_height = 32
        spacer = 3

        layout.addSpacing(2)

        # Flip Vertical
        self.flip_vert_btn = QPushButton("Flip Vertical")
        self.flip_vert_btn.setFont(self.button_font)
        self.flip_vert_btn.setFixedHeight(btn_height)
        self.flip_vert_btn.setEnabled(False)
        self.flip_vert_btn.setToolTip("Flip col vertically")
        self.flip_vert_btn.clicked.connect(lambda: getattr(self,"preview_widget",None) and self.preview_widget.flip_vertical())
        layout.addWidget(self.flip_vert_btn)
        layout.addSpacing(spacer)

        # Flip Horizontal
        self.flip_horz_btn = QPushButton("Flip Horizontal")
        self.flip_horz_btn.setFont(self.button_font)
        self.flip_horz_btn.setFixedHeight(btn_height)
        self.flip_horz_btn.setEnabled(False)
        self.flip_horz_btn.setToolTip("Flip col horizontally")
        self.flip_horz_btn.clicked.connect(lambda: getattr(self,"preview_widget",None) and self.preview_widget.flip_horizontal())
        layout.addWidget(self.flip_horz_btn)
        layout.addSpacing(spacer)

        # Rotate Clockwise
        self.rotate_cw_btn = QPushButton("Rotate 90° CW")
        self.rotate_cw_btn.setFont(self.button_font)
        self.rotate_cw_btn.setFixedHeight(btn_height)
        self.rotate_cw_btn.setEnabled(False)
        self.rotate_cw_btn.setToolTip("Rotate 90 degrees clockwise")
        self.rotate_cw_btn.clicked.connect(lambda: getattr(self,"preview_widget",None) and self.preview_widget.rotate_cw())
        layout.addWidget(self.rotate_cw_btn)
        layout.addSpacing(spacer)

        # Rotate Counter-Clockwise
        self.rotate_ccw_btn = QPushButton("Rotate 90° CCW")
        self.rotate_ccw_btn.setFont(self.button_font)
        self.rotate_ccw_btn.setFixedHeight(btn_height)
        self.rotate_ccw_btn.setEnabled(False)
        self.rotate_ccw_btn.setToolTip("Rotate 90 degrees counter-clockwise")
        self.rotate_ccw_btn.clicked.connect(lambda: getattr(self,"preview_widget",None) and self.preview_widget.rotate_ccw())
        layout.addWidget(self.rotate_ccw_btn)
        layout.addSpacing(spacer)

        # Analyze
        self.analyze_btn = QPushButton("Analyze")
        self.analyze_btn.setFont(self.button_font)
        self.analyze_btn.setFixedHeight(btn_height)
        self.analyze_btn.clicked.connect(self._analyze_collision)
        self.analyze_btn.setEnabled(False)
        self.analyze_btn.setToolTip("Analyze collision data")
        layout.addWidget(self.analyze_btn)
        layout.addSpacing(spacer)

        # Copy
        self.copy_btn = QPushButton("Copy")
        self.copy_btn.setFont(self.button_font)
        self.copy_btn.setFixedHeight(btn_height)
        self.copy_btn.setEnabled(False)
        self.copy_btn.setToolTip("Copy col to clipboard")
        self.copy_btn.clicked.connect(self._copy_surface)
        layout.addWidget(self.copy_btn)
        layout.addSpacing(spacer)

        # Paste
        self.paste_btn = QPushButton("Paste")
        self.paste_btn.setFont(self.button_font)
        self.paste_btn.setFixedHeight(btn_height)
        self.paste_btn.setEnabled(False)
        self.paste_btn.setToolTip("Paste col from clipboard")
        self.paste_btn.clicked.connect(self._paste_surface)
        layout.addWidget(self.paste_btn)
        layout.addSpacing(spacer)

        # Create
        self.create_surface_btn = QPushButton("Create")
        self.create_surface_btn.setFont(self.button_font)
        self.create_surface_btn.setFixedHeight(btn_height)
        self.create_surface_btn.setToolTip("Create new blank Collision")
        self.create_surface_btn.clicked.connect(self._create_new_surface)
        layout.addWidget(self.create_surface_btn)
        layout.addSpacing(spacer)

        # Delete
        self.delete_surface_btn = QPushButton("Delete")
        self.delete_surface_btn.setFont(self.button_font)
        self.delete_surface_btn.setFixedHeight(btn_height)
        self.delete_surface_btn.setEnabled(False)
        self.delete_surface_btn.setToolTip("Remove selected Collision")
        self.delete_surface_btn.clicked.connect(self._delete_surface)
        layout.addWidget(self.delete_surface_btn)
        layout.addSpacing(spacer)

        # Duplicate
        self.duplicate_surface_btn = QPushButton("Duplicate")
        self.duplicate_surface_btn.setFont(self.button_font)
        self.duplicate_surface_btn.setFixedHeight(btn_height)
        self.duplicate_surface_btn.setEnabled(False)
        self.duplicate_surface_btn.setToolTip("Clone selected Collision")
        self.duplicate_surface_btn.clicked.connect(self._duplicate_surface)
        layout.addWidget(self.duplicate_surface_btn)
        layout.addSpacing(spacer)

        # Paint
        self.paint_btn = QPushButton("Paint")
        self.paint_btn.setFont(self.button_font)
        self.paint_btn.setFixedHeight(btn_height)
        self.paint_btn.setEnabled(False)
        self.paint_btn.setToolTip("Paint free hand on surface — assign materials")
        self.paint_btn.clicked.connect(self._open_paint_editor)
        layout.addWidget(self.paint_btn)
        layout.addSpacing(spacer)

        # Surface Type
        self.surface_type_btn = QPushButton("Surface type")
        self.surface_type_btn.setFont(self.button_font)
        self.surface_type_btn.setFixedHeight(btn_height)
        self.surface_type_btn.setToolTip("Surface types")
        self.surface_type_btn.clicked.connect(self._open_surface_type_dialog)
        layout.addWidget(self.surface_type_btn)
        layout.addSpacing(spacer)

        # Surface Edit
        self.surface_edit_btn = QPushButton("Surface Edit")
        self.surface_edit_btn.setFont(self.button_font)
        self.surface_edit_btn.setFixedHeight(btn_height)
        self.surface_edit_btn.setToolTip("Surface Editor — edit mesh faces and vertices")
        self.surface_edit_btn.clicked.connect(self._open_surface_edit_dialog)
        layout.addWidget(self.surface_edit_btn)
        layout.addSpacing(spacer)

        # Build from TXD
        self.build_from_txd_btn = QPushButton("Build col via")
        self.build_from_txd_btn.setFont(self.button_font)
        self.build_from_txd_btn.setFixedHeight(btn_height)
        self.build_from_txd_btn.setToolTip("Create col surface from txd texture names")
        self.build_from_txd_btn.clicked.connect(self._build_col_from_txd)
        layout.addWidget(self.build_from_txd_btn)

        layout.addStretch()
        return self.transform_text_panel


    def _create_left_panel(self): #vers 5
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

        # Header row with search button
        hdr_row = QHBoxLayout()
        header = QLabel("COL Files")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        hdr_row.addWidget(header)
        hdr_row.addStretch()
        self.col_search_btn = QPushButton()
        self.col_search_btn.setFixedSize(24, 24)
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            self.col_search_btn.setIcon(SVGIconFactory.search_icon(16))
            self.col_search_btn.setIconSize(QSize(16, 16))
        except Exception:
            pass  # No icon — button still works
        self.col_search_btn.setToolTip("Search COL files")
        self.col_search_btn.clicked.connect(self._show_col_search)
        hdr_row.addWidget(self.col_search_btn)
        layout.addLayout(hdr_row)

        # Search box (hidden by default)
        self.col_search_box = QLineEdit()
        self.col_search_box.setPlaceholderText("Search COL files...")
        self.col_search_box.setVisible(False)
        self.col_search_box.textChanged.connect(self._filter_col_list)
        layout.addWidget(self.col_search_box)

        self.col_list_widget = QListWidget()
        self.col_list_widget.setAlternatingRowColors(True)
        self.col_list_widget.itemClicked.connect(self._on_col_selected)
        layout.addWidget(self.col_list_widget)
        return panel


    def _create_middle_panel(self): #vers 6
        """Create middle panel with COL models table — mini toolbar + view toggle."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(250)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)

        # ── Header row: title + [T] view-toggle ──────────────────────────
        hdr_row = QHBoxLayout()
        header = QLabel("COL Models")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        hdr_row.addWidget(header)
        hdr_row.addStretch()

        self._col_view_mode = 'detail'   # start in compact thumbnail view
        self.col_view_toggle_btn = QPushButton("[=]")
        self.col_view_toggle_btn.setFont(self.button_font)
        self.col_view_toggle_btn.setFixedWidth(32)
        self.col_view_toggle_btn.setFixedHeight(22)
        self.col_view_toggle_btn.setToolTip(
            "Toggle view: compact list ↔ full details table")
        self.col_view_toggle_btn.clicked.connect(self._toggle_col_view)
        hdr_row.addWidget(self.col_view_toggle_btn)
        layout.addLayout(hdr_row)

        # ── Mini toolbar: Open / Save / Extract / Undo ───────────────────
        icon_color = self._get_icon_color()
        self._middle_btn_row = QFrame()
        btn_layout = QHBoxLayout(self._middle_btn_row)
        btn_layout.setContentsMargins(0, 0, 0, 0)
        btn_layout.setSpacing(3)

        self.open_col_btn = QPushButton("Open")
        self.open_col_btn.setFont(self.button_font)
        self.open_col_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_col_btn.setIconSize(QSize(20, 20))
        self.open_col_btn.setToolTip("Open COL file")
        self.open_col_btn.clicked.connect(self._open_file)
        btn_layout.addWidget(self.open_col_btn)

        self.save_col_btn = QPushButton("Save")
        self.save_col_btn.setFont(self.button_font)
        self.save_col_btn.setIcon(self.icon_factory.save_icon(color=icon_color))
        self.save_col_btn.setIconSize(QSize(20, 20))
        self.save_col_btn.setToolTip("Save COL file")
        self.save_col_btn.clicked.connect(self._save_file)
        self.save_col_btn.setEnabled(True)
        btn_layout.addWidget(self.save_col_btn)

        self.export_col_btn = QPushButton("Extract")
        self.export_col_btn.setFont(self.button_font)
        self.export_col_btn.setIcon(self.icon_factory.package_icon(color=icon_color))
        self.export_col_btn.setIconSize(QSize(20, 20))
        self.export_col_btn.setToolTip("Export all COL models")
        self.export_col_btn.clicked.connect(self._export_col_data)
        self.export_col_btn.setEnabled(True)
        btn_layout.addWidget(self.export_col_btn)

        self.undo_col_btn = QPushButton()
        self.undo_col_btn.setFont(self.button_font)
        self.undo_col_btn.setIcon(self.icon_factory.undo_icon(color=icon_color))
        self.undo_col_btn.setIconSize(QSize(20, 20))
        self.undo_col_btn.setToolTip("Undo last change")
        self.undo_col_btn.clicked.connect(self._undo_last_action)
        self.undo_col_btn.setEnabled(True)
        btn_layout.addWidget(self.undo_col_btn)

        btn_layout.addStretch()
        layout.addWidget(self._middle_btn_row)
        self._middle_btn_row.setVisible(self.is_docked and not self.standalone_mode)

        # ── Model table (detail view) ────────────────────────────────────
        self.collision_list = QTableWidget()

        class _GuiLayout:
            def __init__(self, table):
                self.table = table
        self.gui_layout = _GuiLayout(self.collision_list)

        self.collision_list.setColumnCount(8)
        self.collision_list.setHorizontalHeaderLabels([
            "Model Name", "Type", "Version", "Size",
            "Spheres", "Boxes", "Vertices", "Faces"])
        self.collision_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.collision_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.collision_list.setAlternatingRowColors(True)
        self.collision_list.itemSelectionChanged.connect(self._on_collision_selected)
        self.collision_list.horizontalHeader().setStretchLastSection(True)
        self.collision_list.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.collision_list.customContextMenuRequested.connect(
            self._show_collision_context_menu)
        self.collision_list.setVisible(False)  # hidden at startup — compact view is default
        layout.addWidget(self.collision_list)

        # ── Compact list (thumbnail + name/version/counts, single row) ───
        self.col_compact_list = QTableWidget()
        self.col_compact_list.setColumnCount(2)
        self.col_compact_list.setHorizontalHeaderLabels(["Preview", "Details"])
        self.col_compact_list.horizontalHeader().setStretchLastSection(True)
        self.col_compact_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.col_compact_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.col_compact_list.setAlternatingRowColors(True)
        self.col_compact_list.setIconSize(QSize(64, 64))
        self.col_compact_list.itemSelectionChanged.connect(
            self._on_compact_col_selected)
        self.col_compact_list.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.col_compact_list.customContextMenuRequested.connect(
            self._show_collision_context_menu)
        self.col_compact_list.setVisible(True)    # start in compact view
        self.col_compact_list.setRowCount(0)      # populated on first file load
        self.col_compact_list.setWordWrap(True)
        self.col_compact_list.setItemDelegate(_ColListDelegate(self.col_compact_list))
        layout.addWidget(self.col_compact_list)

        return panel


    def _create_right_panel(self): #vers 12
        """Create right panel — dockable icon toolbars (TXD Workshop style)."""
        from apps.components.Col_Editor.dockable_toolbar import DockableToolbar
        icon_color = self._get_icon_color()
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        self._right_panel_ref = panel
        main_layout = QVBoxLayout(panel)
        main_layout.setContentsMargins(4, 4, 4, 4)
        main_layout.setSpacing(3)

        # ── Top toolbar (dockable, horizontal above viewport) ─────────────
        left_toolbar = DockableToolbar(panel, settings_key='col_left_toolbar')
        left_toolbar.reflow_requested.connect(self._reflow_col_left_toolbar)
        self._col_left_toolbar = left_toolbar
        icon_frame = self._create_transform_icon_panel()  # returns QFrame with grid
        left_toolbar.set_content(icon_frame)
        left_toolbar.set_dock_position('top')
        main_layout.addWidget(left_toolbar, stretch=0)

        # Keep legacy ref so existing code still works
        self._transform_icon_panel_ref = left_toolbar

        # ── Preview row (viewport + right dockable toolbar) ───────────────
        preview_row = QHBoxLayout()
        preview_row.setSpacing(3)

        self.preview_widget = COL3DViewport()
        self.preview_widget._workshop_ref = self
        preview_row.addWidget(self.preview_widget, stretch=1)

        # Floating paint bar — child of the viewport
        self._create_paint_bar()

        right_toolbar = DockableToolbar(panel, settings_key='col_right_toolbar')
        right_toolbar.reflow_requested.connect(self._reflow_col_right_toolbar)
        self._col_right_toolbar = right_toolbar
        ctrl_frame = self._create_preview_controls()  # returns QFrame
        right_toolbar.set_content(ctrl_frame)
        right_toolbar.set_dock_position('right')
        preview_row.addWidget(right_toolbar, stretch=0)

        # Keep legacy ref
        self.preview_controls = ctrl_frame

        main_layout.addLayout(preview_row, stretch=1)

        # Register extra snap panels
        self._col_left_toolbar._extra_panels  = [self.preview_widget]
        self._col_right_toolbar._extra_panels = [self.preview_widget]

        # Information group below
        info_group = QGroupBox("")
        info_group.setFont(self.title_font)
        info_layout = QVBoxLayout(info_group)
        info_group.setMaximumHeight(180)  # extra 40px for paint row

        # === LINE 1: collision name ===
        name_layout = QHBoxLayout()
        name_label = QLabel("COL Name:")
        name_label.setFont(self.panel_font)
        name_layout.addWidget(name_label)

        self.info_name = QLineEdit()
        self.info_name.setText("Click to edit...")
        self.info_name.setFont(self.panel_font)
        self.info_name.setReadOnly(True)
        self.info_name.setStyleSheet("padding: px; border: 1px solid #3a3a3a;")
        #self.info_name.returnPressed.connect(self._save_surface_name)
        #self.info_name.editingFinished.connect(self._save_surface_name)
        self.info_name.mousePressEvent = lambda e: self._enable_name_edit(e, False)
        name_layout.addWidget(self.info_name, stretch=1)
        info_layout.addLayout(name_layout)

        # === LINES 2 & 3: Build BOTH rows, show/hide based on panel width ===
        # ── Text+label row (wide) ────────────────────────────────────────
        self._bottom_text_row = QWidget()
        tr_lay = QVBoxLayout(self._bottom_text_row)
        tr_lay.setContentsMargins(0, 0, 0, 0)
        tr_lay.setSpacing(2)

        fmt_lay = QHBoxLayout()
        fmt_lay.setSpacing(5)
        self.format_combo = QComboBox()
        self.format_combo.setFont(self.panel_font)
        self.format_combo.addItems(["COL", "COL2", "COL3", "COL4"])
        self.format_combo.currentTextChanged.connect(self._change_format)
        self.format_combo.setMaximumWidth(100)
        fmt_lay.addWidget(self.format_combo)
        fmt_lay.addStretch()
        for attr, label, icon_fn, tip, slot in [
            ('switch_btn',     'Mesh',       'flip_vert_icon',  'Cycle render mode',   'switch_surface_view'),
            ('convert_btn',    'Convert',    'convert_icon',    'Convert format',      '_convert_surface'),
            ('compress_btn',   'Compress',   'compress_icon',   'Compress',            '_compress_surface'),
            ('uncompress_btn', 'Uncompress', 'uncompress_icon', 'Uncompress',          '_uncompress_surface'),
            ('import_btn',     'Import',     'import_icon',     'Import col/cst/3ds',  '_import_selected'),
            ('export_btn',     'Export',     'export_icon',     'Export col/cst/3ds',  'export_selected'),
        ]:
            b = QPushButton(label)
            b.setFont(self.button_font)
            b.setIcon(getattr(self.icon_factory, icon_fn)(color=icon_color))
            b.setIconSize(QSize(20, 20))
            b.setToolTip(tip)
            b.clicked.connect(getattr(self, slot))
            b.setEnabled(False)
            setattr(self, attr, b)
            fmt_lay.addWidget(b)
        tr_lay.addLayout(fmt_lay)

        shd_lay = QHBoxLayout()
        shd_lay.setSpacing(5)
        self.info_format = QLabel("Shadow Mesh: ")
        self.info_format.setFont(self.panel_font)
        self.info_format.setMinimumWidth(100)
        shd_lay.addWidget(self.info_format)
        for attr, label, icon_fn, tip, slot in [
            ('show_shadow_btn',   'View',   'view_icon',   'View Shadow Mesh',   '_open_mipmap_manager'),
            ('create_shadow_btn', 'Create', 'add_icon',    'Create Shadow Mesh', 'shadow_dialog'),
            ('remove_shadow_btn', 'Remove', 'delete_icon', 'Remove Shadow Mesh', '_remove_shadow'),
        ]:
            b = QPushButton(label)
            b.setFont(self.button_font)
            b.setIcon(getattr(self.icon_factory, icon_fn)(color=icon_color))
            b.setIconSize(QSize(20, 20))
            b.setToolTip(tip)
            b.clicked.connect(getattr(self, slot))
            b.setEnabled(False)
            setattr(self, attr, b)
            shd_lay.addWidget(b)
        tr_lay.addLayout(shd_lay)
        info_layout.addWidget(self._bottom_text_row)

        # ── Icon-only row (narrow) ─────────────────────────────────────────
        self._bottom_icon_row = QWidget()
        ir_lay = QHBoxLayout(self._bottom_icon_row)
        ir_lay.setContentsMargins(0, 0, 0, 0)
        ir_lay.setSpacing(2)
        fmt_ico = QComboBox()
        fmt_ico.addItems(["COL","COL2","COL3","COL4"])
        fmt_ico.currentTextChanged.connect(self._change_format)
        fmt_ico.setMaximumWidth(65)
        ir_lay.addWidget(fmt_ico)
        for icon_fn, tip, slot in [
            ('flip_vert_icon',  'Cycle render mode',  'switch_surface_view'),
            ('convert_icon',    'Convert',            '_convert_surface'),
            ('compress_icon',   'Compress',           '_compress_surface'),
            ('uncompress_icon', 'Uncompress',         '_uncompress_surface'),
            ('import_icon',     'Import',             '_import_selected'),
            ('export_icon',     'Export',             'export_selected'),
            ('view_icon',       'View Shadow',        '_open_mipmap_manager'),
            ('add_icon',        'Create Shadow',      'shadow_dialog'),
            ('delete_icon',     'Remove Shadow',      '_remove_shadow'),
        ]:
            b = QPushButton()
            b.setIcon(getattr(self.icon_factory, icon_fn)(color=icon_color))
            b.setIconSize(QSize(18, 18))
            b.setFixedSize(30, 30)
            b.setToolTip(tip)
            b.clicked.connect(getattr(self, slot))
            b.setEnabled(False)
            ir_lay.addWidget(b)
        ir_lay.addStretch()
        info_layout.addWidget(self._bottom_icon_row)
        self._bottom_icon_row.setVisible(False)

        # ── Paint mode row (hidden until paint mode active) ───────────────
        main_layout.addWidget(info_group, stretch=0)
        return panel




    def _create_paint_bar(self): #vers 3
        """Floating paint bar — QWidget child of preview_widget, sits at top of viewport.
        Called once from _create_right_panel after preview_widget is created."""
        vp = self.preview_widget
        ic = self._get_icon_color()

        bar = QWidget(vp)
        bar.setObjectName("paint_bar")
        bar.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        bar.setStyleSheet(
            "QWidget#paint_bar { background:#1a1a2e; border-bottom:2px solid #ff8c00; }"
            "QLabel  { color:#ddd; background:transparent; }"
            "QComboBox { background:#252535; color:#eee; border:1px solid #555; }"
            "QPushButton { background:#252535; color:#eee; border:1px solid #555; border-radius:3px; }"
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
            "background:#808080; border:1px solid #aaa; border-radius:2px;")
        lay.addWidget(self.paint_swatch)

        self.paint_mat_combo = QComboBox()
        self.paint_mat_combo.setFixedHeight(26)
        self.paint_mat_combo.setMinimumWidth(160)
        self.paint_mat_combo.setMaximumWidth(260)
        lay.addWidget(self.paint_mat_combo)

        lay.addSpacing(4)

        def _tbtn(attr, icon_fn, tip, tool):
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
            "color:#ff6b35; font-weight:bold; background:#252535; border:1px solid #555; border-radius:3px;")
        self.paint_exit_btn.clicked.connect(self._exit_paint_mode)
        lay.addWidget(self.paint_exit_btn)

        self.paint_toolbar = bar
        bar.setGeometry(0, 0, vp.width(), 34)
        bar.hide()

        # Reposition bar when viewport resizes
        _orig = vp.resizeEvent
        def _on_vp_resize(event, _o=_orig, _bar=bar, _vp=vp):
            _o(event)
            if _bar.isVisible():
                _bar.setGeometry(0, 0, _vp.width(), 34)
                _bar.raise_()
        vp.resizeEvent = _on_vp_resize

    def _create_preview_controls(self): #vers 7
        """Right toolbar icon grid — returns QFrame with buttons registered
        in self._col_ctrl_buttons for DockableToolbar reflow."""
        from PyQt6.QtWidgets import QGridLayout
        icon_color = self._get_icon_color()
        pw = self.preview_widget

        f = QFrame()
        f.setFrameStyle(QFrame.Shape.NoFrame)
        grid = QGridLayout(f)
        grid.setContentsMargins(0, 0, 0, 0)
        grid.setSpacing(2)

        self._col_ctrl_grid    = grid
        self._col_ctrl_buttons = []
        self._col_ctrl_frame   = f

        def btn(tip, icon_fn, callback, checkable=False, checked=False):
            b = QPushButton()
            b.setIcon(icon_fn(color=icon_color))
            b.setIconSize(QSize(16, 16))
            b.setFixedSize(26, 26)
            b.setToolTip(tip)
            if checkable:
                b.setCheckable(True)
                b.setChecked(checked)
            b.clicked.connect(callback)
            self._col_ctrl_buttons.append(b)
            return b

        btn("Zoom In",      self.icon_factory.zoom_in_icon,   pw.zoom_in)
        btn("Zoom Out",     self.icon_factory.zoom_out_icon,  pw.zoom_out)
        btn("Reset View",   self.icon_factory.reset_icon,     pw.reset_view)
        btn("Fit to Window",self.icon_factory.fit_icon,       pw.fit_to_window)

        btn("Pan Up",    self.icon_factory.arrow_up_icon,    lambda: pw.pan( 0,  20))
        btn("Pan Down",  self.icon_factory.arrow_down_icon,  lambda: pw.pan( 0, -20))
        btn("Pan Left",  self.icon_factory.arrow_left_icon,  lambda: pw.pan(-20,  0))
        btn("Pan Right", self.icon_factory.arrow_right_icon, lambda: pw.pan( 20,  0))

        btn("Render / Background Settings",
            self.icon_factory.color_picker_icon, self._open_render_settings_dialog)

        self.view_spheres_btn = btn("Toggle Spheres", self.icon_factory.sphere_icon,
                                    lambda checked: pw.set_show_spheres(checked),
                                    checkable=True, checked=True)
        self.view_boxes_btn   = btn("Toggle Boxes",   self.icon_factory.box_icon,
                                    lambda checked: pw.set_show_boxes(checked),
                                    checkable=True, checked=True)
        self.view_mesh_btn    = btn("Toggle Mesh",    self.icon_factory.mesh_icon,
                                    lambda checked: pw.set_show_mesh(checked),
                                    checkable=True, checked=True)
        self.backface_btn     = btn("Toggle Backface",self.icon_factory.backface_icon,
                                    lambda checked: pw.set_backface(checked),
                                    checkable=True, checked=False)

        # Initial placement (1 col — docked right by default)
        self._reflow_col_right_toolbar('right')
        return f

    def _reflow_col_right_toolbar(self, pos: str): #vers 1
        """Reflow right toolbar icons based on dock position."""
        from apps.components.Col_Editor.dockable_toolbar import SNAP_LEFT, SNAP_RIGHT, SNAP_TOP, SNAP_BOTTOM
        grid  = getattr(self, '_col_ctrl_grid', None)
        btns  = getattr(self, '_col_ctrl_buttons', [])
        frame = getattr(self, '_col_ctrl_frame', None)
        if not grid or not btns:
            return
        n = len(btns)
        btn_w = 28

        if pos == 'float':
            n_cols = 1          # single column while floating
        elif pos in (SNAP_LEFT, SNAP_RIGHT):
            n_cols = 1
        elif pos in (SNAP_TOP, SNAP_BOTTOM):
            n_cols = n          # single row
        else:
            pw = frame.width() if frame else 0
            n_cols = max(1, pw // btn_w) if pw > 0 else 1

        # Clear grid
        for i in range(grid.count() - 1, -1, -1):
            item = grid.itemAt(i)
            if item and item.widget():
                grid.removeWidget(item.widget())
        # Place
        for idx, b in enumerate(btns):
            grid.addWidget(b, idx // n_cols, idx % n_cols)

        # Width constraint
        if frame:
            if n_cols == 1:
                frame.setFixedWidth(26 + 4)
            else:
                frame.setMinimumWidth(0)
                frame.setMaximumWidth(16777215)


    def _update_toolbar_for_docking_state(self): #vers 1
        """Update toolbar visibility based on docking state"""
        # Hide/show drag button based on docking state
        if hasattr(self, 'drag_btn'):
            self.drag_btn.setVisible(not self.is_docked)


# - Rest of the logic for the panels

    def _apply_title_font(self): #vers 1
        """Apply title font to title bar labels"""
        if hasattr(self, 'title_font'):
            # Find all title labels
            for label in self.findChildren(QLabel):
                if label.objectName() == "title_label" or "🗺️" in label.text():
                    label.setFont(self.title_font)


    def _apply_panel_font(self): #vers 1
        """Apply panel font to info panels and labels"""
        if hasattr(self, 'panel_font'):
            # Apply to info labels (Mipmaps, Bumpmaps, status labels)
            for label in self.findChildren(QLabel):
                if any(x in label.text() for x in ["Mipmaps:", "Bumpmaps:", "Status:", "Type:", "Format:"]):
                    label.setFont(self.panel_font)


    def _apply_button_font(self): #vers 1
        """Apply button font to all buttons"""
        if hasattr(self, 'button_font'):
            for button in self.findChildren(QPushButton):
                button.setFont(self.button_font)


    def _apply_infobar_font(self): #vers 1
        """Apply fixed-width font to info bar at bottom"""
        if hasattr(self, 'infobar_font'):
            if hasattr(self, 'info_bar'):
                self.info_bar.setFont(self.infobar_font)


    def _load_img_col_list(self): #vers 2
        """Load COL files from IMG archive"""
        try:
            # Safety check for standalone mode
            if self.standalone_mode or not hasattr(self, 'col_list_widget') or self.col_list_widget is None:
                return

            self.col_list_widget.clear()
            self.col_list = []

            if not self.current_img:
                return

            for entry in self.current_img.entries:
                if entry.name.lower().endswith('.col'):
                    self.col_list.append(entry)
                    item = QListWidgetItem(entry.name)
                    item.setData(Qt.ItemDataRole.UserRole, entry)
                    size_kb = entry.size / 1024
                    item.setToolTip(f"{entry.name}\nSize: {size_kb:.1f} KB")
                    self.col_list_widget.addItem(item)

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"📋 Found {len(self.col_list)} COL files")
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error loading COL list: {str(e)}")


  #  def setup_col_table_structure(workshop): pass
  #  def populate_col_table(workshop, col_file):
  #      for model in col_file.models:
  #          print(f"Model: {model.header.name}")

# - Rest of the logic for the panels

    def _pan_preview(self, dx, dy): #vers 2
        """Pan preview by dx, dy pixels - FIXED"""
        if hasattr(self, 'preview_widget') and self.preview_widget:
            self.preview_widget.pan(dx, dy)

    def _pick_background_color(self): #vers 1
        """Open color picker for background"""
        color = QColorDialog.getColor(self.preview_widget.bg_color, self, "Pick Background Color")
        if color.isValid():
            self.preview_widget.set_background_color(color)

    def _set_checkerboard_bg(self): #vers 1
        """Set checkerboard background"""
        # Create checkerboard pattern
        self.preview_widget.setStyleSheet("""
            border: 1px solid #3a3a3a;
            background-image:
                linear-gradient(45deg, #333 25%, transparent 25%),
                linear-gradient(-45deg, #333 25%, transparent 25%),
                linear-gradient(45deg, transparent 75%, #333 75%),
                linear-gradient(-45deg, transparent 75%, #333 75%);
            background-size: 20px 20px;
            background-position: 0 0, 0 10px, 10px -10px, -10px 0px;
        """)


    def _create_level_card(self, level_data): #vers 2
        """Create modern level card matching mockup"""
        card = QFrame()
        card.setFrameStyle(QFrame.Shape.StyledPanel)
        card.setStyleSheet("""
            QFrame {
                background: #1e1e1e;
                border: 1px solid #3a3a3a;
                border-radius: 5px;
            }
            QFrame:hover {
                border-color: #4a6fa5;
                background: #252525;
            }
        """)
        card.setMinimumHeight(140)

        layout = QHBoxLayout(card)
        layout.setContentsMargins(15, 15, 15, 15)
        layout.setSpacing(15)

        # Preview thumbnail
        preview_widget = self._create_preview_widget(level_data)
        layout.addWidget(preview_widget)

        # Level info section
        info_section = self._create_info_section(level_data)
        layout.addWidget(info_section, stretch=1)

        # Action buttons
        action_section = self._create_action_section(level_data)
        layout.addWidget(action_section)

        return card

    def _create_preview_widget(self, level_data=None): #vers 5
        """Create preview widget - CollisionPreviewWidget for col_workshop"""
        if level_data is None:
            # Return collision preview widget for main preview area
            preview = CollisionPreviewWidget(self)
            print(f"Created CollisionPreviewWidget: {type(preview)}")  # ADD THIS
            return preview

        # Original logic with level_data for mipmap/level cards (if needed)
        level_num = level_data.get('level', 0)
        width = level_data.get('width', 0)
        height = level_data.get('height', 0)
        rgba_data = level_data.get('rgba_data')
        preview_size = max(45, 120 - (level_num * 15))

        preview = QLabel()
        preview.setFixedSize(preview_size, preview_size)
        preview.setStyleSheet("""
            QLabel {
                background: #0a0a0a;
                border: 2px solid #3a3a3a;
                border-radius: 3px;
            }
        """)
        preview.setAlignment(Qt.AlignmentFlag.AlignCenter)

        if rgba_data and width > 0:
            try:
                image = QImage(rgba_data, width, height, width * 4, QImage.Format.Format_RGBA8888)
                if not image.isNull():
                    pixmap = QPixmap.fromImage(image)
                    scaled_pixmap = pixmap.scaled(
                        preview_size - 10, preview_size - 10,
                        Qt.AspectRatioMode.KeepAspectRatio,
                        Qt.TransformationMode.SmoothTransformation
                    )
                    preview.setPixmap(scaled_pixmap)
            except:
                preview.setText("No Data")
        else:
            preview.setText("No Data")

        return preview


    def _create_info_section(self, level_data): #vers 1
        """Create info section with stats grid"""
        info_widget = QWidget()
        layout = QVBoxLayout(info_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(10)

        # Header with level number and dimensions
        header_layout = QHBoxLayout()

        level_num = level_data.get('level', 0)
        level_badge = QLabel(f"Level {level_num}")
        level_badge.setStyleSheet("""
            QLabel {
                background: #0d47a1;
                color: white;
                padding: 4px 12px;
                border-radius: 3px;
                font-weight: bold;
                font-size: 13px;
            }
        """)
        header_layout.addWidget(level_badge)

        width = level_data.get('width', 0)
        height = level_data.get('height', 0)
        dim_label = QLabel(f"{width} x {height}")
        dim_label.setStyleSheet("font-size: 16px; font-weight: bold; color: #4a9eff;")
        header_layout.addWidget(dim_label)

        # Main indicator
        if level_num == 0:
            main_badge = QLabel("Main Surface")
            main_badge.setStyleSheet("color: #4caf50; font-size: 12px;")
            header_layout.addWidget(main_badge)

        header_layout.addStretch()
        layout.addLayout(header_layout)

        # Stats grid
        stats_grid = self._create_stats_grid(level_data)
        layout.addWidget(stats_grid)

        return info_widget


    def _create_stats_grid(self, level_data): #vers 1
        """Create stats grid"""
        grid_widget = QWidget()
        grid_layout = QHBoxLayout(grid_widget)
        grid_layout.setContentsMargins(0, 0, 0, 0)
        grid_layout.setSpacing(8)

        fmt = level_data.get('format', self.collision_data.get('format', 'Unknown'))
        size = level_data.get('compressed_size', 0)
        size_kb = size / 1024

        # Format stat
        format_stat = self._create_stat_box("Format:", fmt)
        grid_layout.addWidget(format_stat)

        # Size stat
        size_stat = self._create_stat_box("Size:", f"{size_kb:.1f} KB")
        grid_layout.addWidget(size_stat)
        grid_layout.addWidget(comp_stat)

        # Status stat
        is_modified = level_data.get('level', 0) in self.modified_levels
        status_text = "⚠ Modified" if is_modified else "✓ Valid"
        status_color = "#ff9800" if is_modified else "#4caf50"
        status_stat = self._create_stat_box("Status:", status_text, status_color)
        grid_layout.addWidget(status_stat)

        return grid_widget


    def _create_stat_box(self, label, value, value_color="#e0e0e0"): #vers 1
        """Create individual stat box"""
        stat = QFrame()
        stat.setStyleSheet("""
            QFrame {
                background: #252525;
                border-radius: 3px;
                padding: 6px 10px;
            }
        """)

        layout = QHBoxLayout(stat)
        layout.setContentsMargins(8, 4, 8, 4)

        label_widget = QLabel(label)
        label_widget.setStyleSheet("color: #888; font-size: 12px;")
        layout.addWidget(label_widget)

        value_widget = QLabel(value)
        value_widget.setStyleSheet(f"color: {value_color}; font-weight: bold; font-size: 12px;")
        layout.addWidget(value_widget)

        return stat


    def _create_action_section(self, level_data): #vers 1
        """Create action buttons section"""
        action_widget = QWidget()
        layout = QVBoxLayout(action_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(5)

        level_num = level_data.get('level', 0)

        # Export button
        export_btn = QPushButton("Export")
        export_btn.setStyleSheet("""
            QPushButton {
                background: #2e5d2e;
                border: 1px solid #3d7d3d;
                color: white;
                padding: 6px 12px;
                border-radius: 3px;
                font-size: 11px;
            }
            QPushButton:hover {
                background: #3d7d3d;
            }
        """)
        export_btn.clicked.connect(lambda: self._export_level(level_num))
        layout.addWidget(export_btn)

        # Import button
        import_btn = QPushButton("Import")
        import_btn.setStyleSheet("""
            QPushButton {
                background: #5d3d2e;
                border: 1px solid #7d4d3d;
                color: white;
                padding: 6px 12px;
                border-radius: 3px;
                font-size: 11px;
            }
            QPushButton:hover {
                background: #7d4d3d;
            }
        """)
        import_btn.clicked.connect(lambda: self._import_level(level_num))
        layout.addWidget(import_btn)

        # Delete button (not for level 0) or Edit button (for level 0)
        if level_num == 0:
            edit_btn = QPushButton("Edit")
            edit_btn.setStyleSheet("""
                QPushButton {
                    background: #3a3a3a;
                    border: 1px solid #4a4a4a;
                    color: white;
                    padding: 6px 12px;
                    border-radius: 3px;
                    font-size: 11px;
                }
                QPushButton:hover {
                    background: #4a4a4a;
                }
            """)
            edit_btn.clicked.connect(self._edit_main_surface)
            layout.addWidget(edit_btn)
        else:
            delete_btn = QPushButton("Delete")
            delete_btn.setStyleSheet("""
                QPushButton {
                    background: #5d2e2e;
                    border: 1px solid #7d3d3d;
                    color: white;
                    padding: 6px 12px;
                    border-radius: 3px;
                    font-size: 11px;
                }
                QPushButton:hover {
                    background: #7d3d3d;
                }
            """)
            delete_btn.clicked.connect(lambda: self._delete_level(level_num))
            layout.addWidget(delete_btn)

        return action_widget


# - Marker 5

    def _toggle_tearoff(self): #vers 2
        """Toggle tear-off state (merge back to IMG Factory) - IMPROVED"""
        try:
            if self.is_docked:
                # Undock from main window
                self._undock_from_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} torn off from main window")
            else:
                # Dock back to main window
                self._dock_to_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} docked back to main window")
                    
        except Exception as e:
            print(f"Error toggling tear-off: {str(e)}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Tear-off Error", f"Could not toggle tear-off state:\n{str(e)}")


# - Marker 6

    def _open_settings_dialog(self): #vers 1
        """Open settings dialog and refresh on save"""
        dialog = SettingsDialog(self.mel_settings, self)
        if dialog.exec():
            # Refresh platform list with new ROM path
            self._scan_platforms()
            self.status_label.setText("Settings saved - platforms refreshed")


    def _launch_theme_settings(self): #vers 2
        """Launch theme engine from app_settings_system"""
        try:
            from apps.utils.app_settings_system import AppSettings, SettingsDialog

            # Get or create app_settings
            if not hasattr(self, 'app_settings') or self.app_settings is None:
                self.app_settings = AppSettings()
                if not hasattr(self.app_settings, 'current_settings'):
                    print("AppSettings failed to initialize")
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "Error", "Could not initialize theme system")
                    return

            # Launch settings dialog
            dialog = SettingsDialog(self.app_settings, self)

            # Connect theme change signal to apply theme
            dialog.themeChanged.connect(lambda theme: self._apply_theme())

            if dialog.exec():
                # Apply theme after dialog closes
                self._apply_theme()
                print("Theme settings applied")
                if hasattr(self, 'main_window') and self.main_window:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Theme settings updated")

        except Exception as e:
            print(f"Theme settings error: {e}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Theme Error", f"Could not load theme system:\n{e}")


    def _setup_settings_button(self): #vers 1
        """Setup settings button in UI"""
        settings_btn = QPushButton("âš™ Settings")
        settings_btn.clicked.connect(self._open_settings_dialog)
        settings_btn.setMaximumWidth(120)
        return settings_btn


    def _show_settings_dialog(self): #vers 5
        """Show comprehensive settings dialog with all tabs including hotkeys"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QPushButton, QGroupBox,
                                    QCheckBox, QSpinBox, QFormLayout, QScrollArea,
                                    QKeySequenceEdit, QComboBox, QMessageBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QKeySequence

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(700)
        dialog.setMinimumHeight(600)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # === DISPLAY TAB ===
        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Thumbnail settings
        thumb_group = QGroupBox("Thumbnail Display")
        thumb_layout = QVBoxLayout()

        thumb_size_layout = QHBoxLayout()
        thumb_size_layout.addWidget(QLabel("Thumbnail size:"))
        thumb_size_spin = QSpinBox()
        thumb_size_spin.setRange(32, 256)
        thumb_size_spin.setValue(self.thumbnail_size if hasattr(self, 'thumbnail_size') else 64)
        thumb_size_spin.setSuffix(" px")
        thumb_size_layout.addWidget(thumb_size_spin)
        thumb_size_layout.addStretch()
        thumb_layout.addLayout(thumb_size_layout)

        thumb_group.setLayout(thumb_layout)
        display_layout.addWidget(thumb_group)

        # Table display settings
        table_group = QGroupBox("Table Display")
        table_layout = QVBoxLayout()

        row_height_layout = QHBoxLayout()
        row_height_layout.addWidget(QLabel("Row height:"))
        row_height_spin = QSpinBox()
        row_height_spin.setRange(50, 200)
        row_height_spin.setValue(getattr(self, 'table_row_height', 100))
        row_height_spin.setSuffix(" px")
        row_height_layout.addWidget(row_height_spin)
        row_height_layout.addStretch()
        table_layout.addLayout(row_height_layout)

        show_grid_check = QCheckBox("Show grid lines")
        show_grid_check.setChecked(getattr(self, 'show_grid_lines', True))
        table_layout.addWidget(show_grid_check)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # === PREVIEW TAB ===
        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Preview window settings
        preview_window_group = QGroupBox("Preview Window")
        preview_window_layout = QVBoxLayout()

        show_preview_check = QCheckBox("Show preview window by default")
        show_preview_check.setChecked(getattr(self, 'show_preview_default', True))
        show_preview_check.setToolTip("Automatically open preview when selecting surface")
        preview_window_layout.addWidget(show_preview_check)

        auto_refresh_check = QCheckBox("Auto-refresh preview on selection")
        auto_refresh_check.setChecked(getattr(self, 'auto_refresh_preview', True))
        auto_refresh_check.setToolTip("Update preview immediately when clicking surface")
        preview_window_layout.addWidget(auto_refresh_check)

        preview_window_group.setLayout(preview_window_layout)
        preview_layout.addWidget(preview_window_group)

        # Preview size settings
        preview_size_group = QGroupBox("Preview Size")
        preview_size_layout = QVBoxLayout()

        preview_width_layout = QHBoxLayout()
        preview_width_layout.addWidget(QLabel("Default width:"))
        preview_width_spin = QSpinBox()
        preview_width_spin.setRange(200, 1920)
        preview_width_spin.setValue(getattr(self, 'preview_width', 512))
        preview_width_spin.setSuffix(" px")
        preview_width_layout.addWidget(preview_width_spin)
        preview_width_layout.addStretch()
        preview_size_layout.addLayout(preview_width_layout)

        preview_height_layout = QHBoxLayout()
        preview_height_layout.addWidget(QLabel("Default height:"))
        preview_height_spin = QSpinBox()
        preview_height_spin.setRange(200, 1080)
        preview_height_spin.setValue(getattr(self, 'preview_height', 512))
        preview_height_spin.setSuffix(" px")
        preview_height_layout.addWidget(preview_height_spin)
        preview_height_layout.addStretch()
        preview_size_layout.addLayout(preview_height_layout)

        preview_size_group.setLayout(preview_size_layout)
        preview_layout.addWidget(preview_size_group)

        # Preview background
        preview_bg_group = QGroupBox("Preview Background")
        preview_bg_layout = QVBoxLayout()

        bg_combo = QComboBox()
        bg_combo.addItems(["Black", "White", "Gray", "Custom Color"])
        bg_combo.setCurrentText(getattr(self, 'preview_background', 'Checkerboard'))
        preview_bg_layout.addWidget(bg_combo)

        preview_bg_group.setLayout(preview_bg_layout)
        preview_layout.addWidget(preview_bg_group)

        # Preview zoom
        preview_zoom_group = QGroupBox("Preview Zoom")
        preview_zoom_layout = QVBoxLayout()

        fit_to_window_check = QCheckBox("Fit to window by default")
        fit_to_window_check.setChecked(getattr(self, 'preview_fit_to_window', True))
        preview_zoom_layout.addWidget(fit_to_window_check)

        smooth_zoom_check = QCheckBox("Use smooth scaling")
        smooth_zoom_check.setChecked(getattr(self, 'preview_smooth_scaling', True))
        smooth_zoom_check.setToolTip("Better quality but slower for large model mesh")
        preview_zoom_layout.addWidget(smooth_zoom_check)

        preview_zoom_group.setLayout(preview_zoom_layout)
        preview_layout.addWidget(preview_zoom_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # === EXPORT TAB ===
        export_tab = QWidget()
        export_layout = QVBoxLayout(export_tab)

        # Export format
        format_group = QGroupBox("Default Collision Export Format")
        format_layout = QVBoxLayout()

        format_combo = QComboBox()
        format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        format_combo.setCurrentText(getattr(self, 'default_export_format', 'COL'))
        format_layout.addWidget(format_combo)
        format_hint = QLabel("COL recommended for GTAIII/VC, COL2 for SA")
        format_hint.setStyleSheet("color: #888; font-style: italic;")
        format_layout.addWidget(format_hint)

        format_group.setLayout(format_layout)
        export_layout.addWidget(format_group)

        # Export options
        export_options_group = QGroupBox("Export Options")
        export_options_layout = QVBoxLayout()

        preserve_shadow_check = QCheckBox("Preserve Shadow Mesh when exporting")
        preserve_shadow_check.setChecked(getattr(self, 'export_preserve_shadow', True))
        export_options_layout.addWidget(preserve_shadow_check)

        export_shadowm_check = QCheckBox("Export shadow as separate files")
        export_shadowm_check.setChecked(getattr(self, 'export_shadow_separate', False))
        export_shadowm_check.setToolTip("Save each shadow map as _shadow.col, etc.")
        export_options_layout.addWidget(export_shadowm_check)

        create_subfolders_check = QCheckBox("Create subfolders when exporting all")
        create_subfolders_check.setChecked(getattr(self, 'export_create_subfolders', False))
        create_subfolders_check.setToolTip("Organize exports into folders by col type")
        export_options_layout.addWidget(create_subfolders_check)

        export_options_group.setLayout(export_options_layout)
        export_layout.addWidget(export_options_group)

        # Compatibility note
        compat_label = QLabel(
            "Note: PLACEholder." #TODO
        )
        compat_label.setWordWrap(True)
        compat_label.setStyleSheet("padding: 10px; background-color: #3a3a3a; border-radius: 4px;")
        export_layout.addWidget(compat_label)

        export_layout.addStretch()
        tabs.addTab(export_tab, "Export")

        # === IMPORT TAB ===
        import_tab = QWidget()
        import_layout = QVBoxLayout(import_tab)

        # Import behavior
        import_behavior_group = QGroupBox("Import Behavior")
        import_behavior_layout = QVBoxLayout()

        replace_check = QCheckBox("Replace existing collision with same name")
        replace_check.setChecked(getattr(self, 'import_replace_existing', False))
        import_behavior_layout.addWidget(replace_check)

        auto_format_check = QCheckBox("Automatically select best format")
        auto_format_check.setChecked(getattr(self, 'import_auto_format', True))
        auto_format_check.setToolTip("Choose COL/COL3 based on collision version")
        import_behavior_layout.addWidget(auto_format_check)

        import_behavior_group.setLayout(import_behavior_layout)
        import_layout.addWidget(import_behavior_group)

        # Import format
        import_format_group = QGroupBox("Default Collision Format")
        import_format_layout = QVBoxLayout()

        import_format_combo = QComboBox()
        import_format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        import_format_combo.setCurrentText(getattr(self, 'default_import_format', 'COL'))
        import_format_layout.addWidget(import_format_combo)

        format_note = QLabel("COL2/COL3: compression\n, COL type 1 always Uncompressed")
        format_note.setStyleSheet("color: #888; font-style: italic;")
        import_format_layout.addWidget(format_note)

        import_format_group.setLayout(import_format_layout)
        import_layout.addWidget(import_format_group)

        import_layout.addStretch()
        tabs.addTab(import_tab, "Import")

        # === Collision CONSTRAINTS TAB ===
        constraints_tab = QWidget()
        constraints_layout = QVBoxLayout(constraints_tab)

        # Collision naming
        naming_group = QGroupBox("Collision Naming")
        naming_layout = QVBoxLayout()

        name_limit_check = QCheckBox("Enable name length limit")
        name_limit_check.setChecked(getattr(self, 'name_limit_enabled', True))
        name_limit_check.setToolTip("Enforce maximum Collision name length")
        naming_layout.addWidget(name_limit_check)

        char_limit_layout = QHBoxLayout()
        char_limit_layout.addWidget(QLabel("Maximum characters:"))
        char_limit_spin = QSpinBox()
        char_limit_spin.setRange(8, 64)
        char_limit_spin.setValue(getattr(self, 'max_collision_name_length', 32))
        char_limit_spin.setToolTip("RenderWare default is 32 characters")
        char_limit_layout.addWidget(char_limit_spin)
        char_limit_layout.addStretch()
        naming_layout.addLayout(char_limit_layout)

        naming_group.setLayout(naming_layout)
        constraints_layout.addWidget(naming_group)

        # Format support
        format_support_group = QGroupBox("Format Support")
        format_support_layout = QVBoxLayout()

        format_support_group.setLayout(format_support_layout)
        constraints_layout.addWidget(format_support_group)

        constraints_layout.addStretch()
        tabs.addTab(constraints_tab, "Constraints")

        # === KEYBOARD SHORTCUTS TAB ===
        hotkeys_tab = QWidget()
        hotkeys_layout = QVBoxLayout(hotkeys_tab)

        # Add scroll area for hotkeys
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)

        # File Operations Group
        file_group = QGroupBox("File Operations")
        file_form = QFormLayout()

        hotkey_edit_open = QKeySequenceEdit(self.hotkey_open.key() if hasattr(self, 'hotkey_open') else QKeySequence.StandardKey.Open)
        file_form.addRow("Open col:", hotkey_edit_open)

        hotkey_edit_save = QKeySequenceEdit(self.hotkey_save.key() if hasattr(self, 'hotkey_save') else QKeySequence.StandardKey.Save)
        file_form.addRow("Save col:", hotkey_edit_save)

        hotkey_edit_force_save = QKeySequenceEdit(self.hotkey_force_save.key() if hasattr(self, 'hotkey_force_save') else QKeySequence("Alt+Shift+S"))
        force_save_layout = QHBoxLayout()
        force_save_layout.addWidget(hotkey_edit_force_save)
        force_save_hint = QLabel("(Force save even if unmodified)")
        force_save_hint.setStyleSheet("color: #888; font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key() if hasattr(self, 'hotkey_save_as') else QKeySequence.StandardKey.SaveAs)
        file_form.addRow("Save As:", hotkey_edit_save_as)

        hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key() if hasattr(self, 'hotkey_close') else QKeySequence.StandardKey.Close)
        file_form.addRow("Close:", hotkey_edit_close)

        file_group.setLayout(file_form)
        scroll_layout.addWidget(file_group)

        # Edit Operations Group
        edit_group = QGroupBox("Edit Operations")
        edit_form = QFormLayout()

        hotkey_edit_undo = QKeySequenceEdit(self.hotkey_undo.key() if hasattr(self, 'hotkey_undo') else QKeySequence.StandardKey.Undo)
        edit_form.addRow("Undo:", hotkey_edit_undo)

        hotkey_edit_copy = QKeySequenceEdit(self.hotkey_copy.key() if hasattr(self, 'hotkey_copy') else QKeySequence.StandardKey.Copy)
        edit_form.addRow("Copy Collision:", hotkey_edit_copy)

        hotkey_edit_paste = QKeySequenceEdit(self.hotkey_paste.key() if hasattr(self, 'hotkey_paste') else QKeySequence.StandardKey.Paste)
        edit_form.addRow("Paste Collision:", hotkey_edit_paste)

        hotkey_edit_delete = QKeySequenceEdit(self.hotkey_delete.key() if hasattr(self, 'hotkey_delete') else QKeySequence.StandardKey.Delete)
        edit_form.addRow("Delete:", hotkey_edit_delete)

        hotkey_edit_duplicate = QKeySequenceEdit(self.hotkey_duplicate.key() if hasattr(self, 'hotkey_duplicate') else QKeySequence("Ctrl+D"))
        edit_form.addRow("Duplicate:", hotkey_edit_duplicate)

        hotkey_edit_rename = QKeySequenceEdit(self.hotkey_rename.key() if hasattr(self, 'hotkey_rename') else QKeySequence("F2"))
        edit_form.addRow("Rename:", hotkey_edit_rename)

        edit_group.setLayout(edit_form)
        scroll_layout.addWidget(edit_group)

        # Collision Operations Group
        coll_group = QGroupBox("Collision Operations")
        coll_group = QFormLayout()

        hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key() if hasattr(self, 'hotkey_import') else QKeySequence("Ctrl+I"))
        coll_form.addRow("Import Collision:", hotkey_edit_import)

        hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key() if hasattr(self, 'hotkey_export') else QKeySequence("Ctrl+E"))
        coll_form.addRow("Export Collision:", hotkey_edit_export)

        hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key() if hasattr(self, 'hotkey_export_all') else QKeySequence("Ctrl+Shift+E"))
        coll_form.addRow("Export All:", hotkey_edit_export_all)

        coll_group.setLayout(coll_form)
        scroll_layout.addWidget(coll_group)

        # View Operations Group
        view_group = QGroupBox("View Operations")
        view_form = QFormLayout()

        hotkey_edit_refresh = QKeySequenceEdit(self.hotkey_refresh.key() if hasattr(self, 'hotkey_refresh') else QKeySequence.StandardKey.Refresh)
        view_form.addRow("Refresh:", hotkey_edit_refresh)

        hotkey_edit_properties = QKeySequenceEdit(self.hotkey_properties.key() if hasattr(self, 'hotkey_properties') else QKeySequence("Alt+Return"))
        view_form.addRow("Properties:", hotkey_edit_properties)

        hotkey_edit_find = QKeySequenceEdit(self.hotkey_find.key() if hasattr(self, 'hotkey_find') else QKeySequence.StandardKey.Find)
        view_form.addRow("Find/Search:", hotkey_edit_find)

        hotkey_edit_help = QKeySequenceEdit(self.hotkey_help.key() if hasattr(self, 'hotkey_help') else QKeySequence.StandardKey.HelpContents)
        view_form.addRow("Help:", hotkey_edit_help)

        view_group.setLayout(view_form)
        scroll_layout.addWidget(view_group)

        scroll_layout.addStretch()

        scroll.setWidget(scroll_widget)
        hotkeys_layout.addWidget(scroll)

        # Reset to defaults button
        reset_layout = QHBoxLayout()
        reset_layout.addStretch()
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")

        def reset_hotkeys():
            reply = QMessageBox.question(dialog, "Reset Hotkeys",
                "Reset all keyboard shortcuts to Plasma6 defaults?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

            if reply == QMessageBox.StandardButton.Yes:
                hotkey_edit_open.setKeySequence(QKeySequence.StandardKey.Open)
                hotkey_edit_save.setKeySequence(QKeySequence.StandardKey.Save)
                hotkey_edit_force_save.setKeySequence(QKeySequence("Alt+Shift+S"))
                hotkey_edit_save_as.setKeySequence(QKeySequence.StandardKey.SaveAs)
                hotkey_edit_close.setKeySequence(QKeySequence.StandardKey.Close)
                hotkey_edit_undo.setKeySequence(QKeySequence.StandardKey.Undo)
                hotkey_edit_copy.setKeySequence(QKeySequence.StandardKey.Copy)
                hotkey_edit_paste.setKeySequence(QKeySequence.StandardKey.Paste)
                hotkey_edit_delete.setKeySequence(QKeySequence.StandardKey.Delete)
                hotkey_edit_duplicate.setKeySequence(QKeySequence("Ctrl+D"))
                hotkey_edit_rename.setKeySequence(QKeySequence("F2"))
                hotkey_edit_import.setKeySequence(QKeySequence("Ctrl+I"))
                hotkey_edit_export.setKeySequence(QKeySequence("Ctrl+E"))
                hotkey_edit_export_all.setKeySequence(QKeySequence("Ctrl+Shift+E"))
                hotkey_edit_refresh.setKeySequence(QKeySequence.StandardKey.Refresh)
                hotkey_edit_properties.setKeySequence(QKeySequence("Alt+Return"))
                hotkey_edit_find.setKeySequence(QKeySequence.StandardKey.Find)
                hotkey_edit_help.setKeySequence(QKeySequence.StandardKey.HelpContents)

        reset_hotkeys_btn.clicked.connect(reset_hotkeys)
        reset_layout.addWidget(reset_hotkeys_btn)
        hotkeys_layout.addLayout(reset_layout)

        tabs.addTab(hotkeys_tab, "Keyboard Shortcuts")

        # Add tabs widget to main layout
        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        def apply_settings(close_dialog=False):
            """Apply all settings"""
            # Apply display settings
            self.thumbnail_size = thumb_size_spin.value()
            self.table_row_height = row_height_spin.value()
            self.show_grid_lines = show_grid_check.isChecked()

            # Apply preview settings
            self.show_preview_default = show_preview_check.isChecked()
            self.auto_refresh_preview = auto_refresh_check.isChecked()
            self.preview_width = preview_width_spin.value()
            self.preview_height = preview_height_spin.value()
            self.preview_background = bg_combo.currentText()
            self.preview_fit_to_window = fit_to_window_check.isChecked()
            self.preview_smooth_scaling = smooth_zoom_check.isChecked()

            # Apply export settings
            self.default_export_format = format_combo.currentText()
            self.export_preserve_alpha = preserve_alpha_check.isChecked()
            self.export_shadow_separate = export_shadow_check.isChecked()
            self.export_create_subfolders = create_subfolders_check.isChecked()

            # Apply import settings
            self.import_auto_name = auto_name_check.isChecked()
            self.import_replace_existing = replace_check.isChecked()
            self.import_auto_format = auto_format_check.isChecked()
            self.default_import_format = import_format_combo.currentText()

            # Apply constraint settings
            self.dimension_limiting_enabled = dimension_check.isChecked()
            self.splash_screen_mode = splash_check.isChecked()
            self.custom_max_dimension = max_dim_spin.value()
            self.name_limit_enabled = name_limit_check.isChecked()
            self.max_surface_name_length = char_limit_spin.value()
            self.iff_import_enabled = iff_check.isChecked()

            # Apply hotkeys
            if hasattr(self, 'hotkey_open'):
                self.hotkey_open.setKey(hotkey_edit_open.keySequence())
            if hasattr(self, 'hotkey_save'):
                self.hotkey_save.setKey(hotkey_edit_save.keySequence())
            if hasattr(self, 'hotkey_force_save'):
                self.hotkey_force_save.setKey(hotkey_edit_force_save.keySequence())
            if hasattr(self, 'hotkey_save_as'):
                self.hotkey_save_as.setKey(hotkey_edit_save_as.keySequence())
            if hasattr(self, 'hotkey_close'):
                self.hotkey_close.setKey(hotkey_edit_close.keySequence())
            if hasattr(self, 'hotkey_undo'):
                self.hotkey_undo.setKey(hotkey_edit_undo.keySequence())
            if hasattr(self, 'hotkey_copy'):
                self.hotkey_copy.setKey(hotkey_edit_copy.keySequence())
            if hasattr(self, 'hotkey_paste'):
                self.hotkey_paste.setKey(hotkey_edit_paste.keySequence())
            if hasattr(self, 'hotkey_delete'):
                self.hotkey_delete.setKey(hotkey_edit_delete.keySequence())
            if hasattr(self, 'hotkey_duplicate'):
                self.hotkey_duplicate.setKey(hotkey_edit_duplicate.keySequence())
            if hasattr(self, 'hotkey_rename'):
                self.hotkey_rename.setKey(hotkey_edit_rename.keySequence())
            if hasattr(self, 'hotkey_import'):
                self.hotkey_import.setKey(hotkey_edit_import.keySequence())
            if hasattr(self, 'hotkey_export'):
                self.hotkey_export.setKey(hotkey_edit_export.keySequence())
            if hasattr(self, 'hotkey_export_all'):
                self.hotkey_export_all.setKey(hotkey_edit_export_all.keySequence())
            if hasattr(self, 'hotkey_refresh'):
                self.hotkey_refresh.setKey(hotkey_edit_refresh.keySequence())
            if hasattr(self, 'hotkey_properties'):
                self.hotkey_properties.setKey(hotkey_edit_properties.keySequence())
            if hasattr(self, 'hotkey_find'):
                self.hotkey_find.setKey(hotkey_edit_find.keySequence())
            if hasattr(self, 'hotkey_help'):
                self.hotkey_help.setKey(hotkey_edit_help.keySequence())

            # Refresh UI with new settings
            if hasattr(self, '_reload_surface_table'):
                self._reload_surface_table()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Settings applied")

            if close_dialog:
                dialog.accept()

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(lambda: apply_settings(close_dialog=False))
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: apply_settings(close_dialog=True))
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        dialog.exec()


    def _show_settings_context_menu(self, pos): #vers 1
        """Show context menu for Settings button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self)

        # Move window action
        move_action = menu.addAction("Move Window")
        move_action.triggered.connect(self._enable_move_mode)

        # Maximize window action
        max_action = menu.addAction("Maximize Window")
        max_action.triggered.connect(self._toggle_maximize)

        # Minimize action
        min_action = menu.addAction("Minimize")
        min_action.triggered.connect(self.showMinimized)

        menu.addSeparator()

        # Upscale Native action
        upscale_action = menu.addAction("Upscale Native")
        upscale_action.setCheckable(True)
        upscale_action.setChecked(False)
        upscale_action.triggered.connect(self._toggle_upscale_native)

        # Shaders action
        shaders_action = menu.addAction("Shaders")
        shaders_action.triggered.connect(self._show_shaders_dialog)

        menu.addSeparator()

        # Icon display mode submenu # TODO icon only system is missing.
        display_menu = menu.addMenu("Platform Display")

        icons_text_action = display_menu.addAction("Icons & Text")
        icons_text_action.setCheckable(True)
        icons_text_action.setChecked(self.icon_display_mode == "icons_and_text")
        icons_text_action.triggered.connect(lambda: self._set_icon_display_mode("icons_and_text"))

        icons_only_action = display_menu.addAction("Icons Only")
        icons_only_action.setCheckable(True)
        icons_only_action.setChecked(self.icon_display_mode == "icons_only")
        icons_only_action.triggered.connect(lambda: self._set_icon_display_mode("icons_only"))

        text_only_action = display_menu.addAction("Text Only")
        text_only_action.setCheckable(True)
        text_only_action.setChecked(self.icon_display_mode == "text_only")
        text_only_action.triggered.connect(lambda: self._set_icon_display_mode("text_only"))

        # Show menu at button position
        menu.exec(self.settings_btn.mapToGlobal(pos))

    def _enable_move_mode(self): #vers 2
        """Enable move window mode using system move"""
        handle = self.windowHandle()
        if handle and hasattr(handle, 'startSystemMove'):
            handle.startSystemMove()

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
            def _set(checked, k=key):
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

    def _show_window_context_menu(self, pos): #vers 1
        """Show context menu for titlebar right-click"""
        from PyQt6.QtWidgets import QMenu


        # Move window action
        move_action = menu.addAction("Move Window")
        move_action.triggered.connect(self._enable_move_mode)

        # Maximize/Restore action
        if self.isMaximized():
            max_action = menu.addAction("Restore Window")
        else:
            max_action = menu.addAction("Maximize Window")
        max_action.triggered.connect(self._toggle_maximize)

        # Minimize action
        min_action = menu.addAction("Minimize")
        min_action.triggered.connect(self.showMinimized)

        menu.addSeparator()

        # Close action
        close_action = menu.addAction("Close")
        close_action.triggered.connect(self.close)

        # Show menu at global position
        menu.exec(self.mapToGlobal(pos))


    def _get_icon_color(self): #vers 2
        """Get icon colour from current theme — returns text_primary."""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            colors = self.app_settings.get_theme_colors()
            return colors.get('text_primary', '#000000')
        return '#000000'


    def _apply_fonts_to_widgets(self): #vers 1
        """Apply fonts from AppSettings to all widgets"""
        if not hasattr(self, 'default_font'):
            return

        print("\n=== Applying Fonts ===")
        print(f"Default font: {self.default_font.family()} {self.default_font.pointSize()}pt")
        print(f"Title font: {self.title_font.family()} {self.title_font.pointSize()}pt")
        print(f"Panel font: {self.panel_font.family()} {self.panel_font.pointSize()}pt")
        print(f"Button font: {self.button_font.family()} {self.button_font.pointSize()}pt")

        # Apply default font to main window
        self.setFont(self.default_font)

        # Apply title font to titlebar
        if hasattr(self, 'title_label'):
            self.title_label.setFont(self.title_font)

        # Apply panel font to lists
        if hasattr(self, 'platform_list'):
            self.platform_list.setFont(self.panel_font)
        if hasattr(self, 'game_list'):
            self.game_list.setFont(self.panel_font)

        # Apply button font to all buttons
        for btn in self.findChildren(QPushButton):
            btn.setFont(self.button_font)

        print("Fonts applied to widgets")
        print("======================\n")


    def _apply_theme(self): #vers 3
        """Apply theme from app_settings"""
        try:
            # Use self.app_settings first, then fall back to main_window
            app_settings = None
            if hasattr(self, 'app_settings') and self.app_settings:
                app_settings = self.app_settings
            elif self.main_window and hasattr(self.main_window, 'app_settings'):
                app_settings = self.main_window.app_settings

            if app_settings:
                # Get current theme
                theme_name = app_settings.current_settings.get('theme', 'App_Factory')
                stylesheet = app_settings.get_stylesheet()

                # Apply stylesheet
                self.setStyleSheet(stylesheet)

                # Force update
                self.update()

                print(f"Theme applied: {theme_name}")
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Theme applied: {theme_name}")
            else:
                # Fallback dark theme
                self.setStyleSheet("""
                    QWidget {
                        background-color: #2b2b2b;
                        color: #e0e0e0;
                    }
                    QListWidget, QTableWidget, QTextEdit {
                        background-color: #1e1e1e;
                        border: 1px solid #3a3a3a;
                    }
                """)
                print("No app_settings found, using fallback theme")
        except Exception as e:
            print(f"Theme application error: {e}")


    def _apply_settings(self, dialog): #vers 5
        """Apply settings from dialog"""
        from PyQt6.QtGui import QFont

        # Store font settings
        self.title_font = QFont(self.title_font_combo.currentFont().family(), self.title_font_size.value())
        self.panel_font = QFont(self.panel_font_combo.currentFont().family(), self.panel_font_size.value())
        self.button_font = QFont(self.button_font_combo.currentFont().family(), self.button_font_size.value())
        self.infobar_font = QFont(self.infobar_font_combo.currentFont().family(), self.infobar_font_size.value())

        # Apply fonts to specific elements
        self._apply_title_font()
        self._apply_panel_font()
        self._apply_button_font()
        self._apply_infobar_font()
        self.default_export_format = format_combo.currentText()

        # Apply button display mode
        mode_map = ["icons", "text", "both"]
        new_mode = mode_map[self.settings_display_combo.currentIndex()]
        if new_mode != self.button_display_mode:
            self.button_display_mode = new_mode
            self._update_all_buttons()

        # Locale setting (would need implementation)
        locale_text = self.settings_locale_combo.currentText()


# - Marker 7

    def _refresh_main_window(self): #vers 1
        """Refresh the main window to show changes"""
        try:
            if self.main_window:
                # Try to refresh the main table
                if hasattr(self.main_window, 'refresh_table'):
                    self.main_window.refresh_table()
                elif hasattr(self.main_window, 'reload_current_file'):
                    self.main_window.reload_current_file()
                elif hasattr(self.main_window, 'update_display'):
                    self.main_window.update_display()

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Refresh error: {str(e)}")


#------ Col functions

    def _open_file(self): #vers 1
        """Open file dialog and load COL file"""
        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self,
                "Open COL File",
                "",
                "COL Files (*.col);;All Files (*)"
            )

            if file_path:
                self.open_col_file(file_path)

        except Exception as e:
            print(f"Error in open file dialog: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to open file:\n{str(e)}")



    def _toggle_col_view(self): #vers 1
        """Toggle between detail table and compact thumbnail+name list."""
        if self._col_view_mode == 'list':
            self._col_view_mode = 'detail'
            self.collision_list.setVisible(False)
            self.col_compact_list.setVisible(True)
            self.col_view_toggle_btn.setText("[=]")
            self.col_view_toggle_btn.setToolTip("Switch to detail table view")
            if (self.col_compact_list.rowCount() == 0
                    and self.collision_list.rowCount() > 0
                    and self.current_col_file):
                self._populate_compact_col_list()
        else:
            self._col_view_mode = 'list'
            self.col_compact_list.setVisible(False)
            self.collision_list.setVisible(True)
            self.col_view_toggle_btn.setText("[T]")
            self.col_view_toggle_btn.setToolTip("Switch to compact thumbnail view")

    def _populate_compact_col_list(self): #vers 1
        """Fill compact two-column list (icon + name/version/counts)."""
        try:
            self.col_compact_list.setRowCount(0)
            models = getattr(self.current_col_file, 'models', [])
            for i, model in enumerate(models):
                self.col_compact_list.insertRow(i)

                # Col 0: real collision thumbnail
                icon_item = QTableWidgetItem()
                pm = self._generate_collision_thumbnail(model, 64, 64,
                                yaw=self._thumb_yaw, pitch=self._thumb_pitch)
                icon_item.setData(Qt.ItemDataRole.DecorationRole, pm)
                self.col_compact_list.setItem(i, 0, icon_item)

                # Col 1: name + stats
                name = getattr(model, 'name', '') or f'model_{i}'
                ver  = getattr(model, 'version', None)
                ver_str = ver.name if hasattr(ver, 'name') else str(ver) if ver else '?'
                spheres = len(getattr(model, 'spheres',  []))
                boxes   = len(getattr(model, 'boxes',    []))
                verts   = len(getattr(model, 'vertices', []))
                faces   = len(getattr(model, 'faces',    []))

                line1 = name
                line2 = "Version: " + ver_str
                line3 = "Spheres: " + str(spheres) + "  Boxes: " + str(boxes)
                line4 = "Verts: "   + str(verts)   + "  Faces: " + str(faces)
                details = line1 + "\n" + line2 + "\n" + line3 + "\n" + line4

                det_item = QTableWidgetItem(details)
                det_item.setToolTip(details)
                self.col_compact_list.setItem(i, 1, det_item)
                self.col_compact_list.setRowHeight(i, 72)

            self.col_compact_list.setColumnWidth(0, 72)
        except Exception as e:
            print("_populate_compact_col_list error: " + str(e))

    def _on_compact_col_selected(self): #vers 3
        """Handle compact [=] list selection."""
        try:
            rows = self.col_compact_list.selectionModel().selectedRows()
            if rows:
                self._select_model_by_row(rows[0].row())
        except Exception as e:
            print("_on_compact_col_selected error: " + str(e))

    def _push_undo(self, model_index, description=""): #vers 1
        """Deep-copy model[model_index] onto undo stack before any edit."""
        import copy
        if not self.current_col_file:
            return
        models = getattr(self.current_col_file, 'models', [])
        if model_index < 0 or model_index >= len(models):
            return
        self.undo_stack.append({
            'description': description,
            'model_index': model_index,
            'model_data':  copy.deepcopy(models[model_index]),
        })
        if len(self.undo_stack) > 50:
            self.undo_stack.pop(0)
        if hasattr(self, 'undo_col_btn'):
            self.undo_col_btn.setEnabled(True)
        # Also enable the paint toolbar undo button if paint mode active
        if hasattr(self, 'paint_undo_btn') and getattr(self, 'paint_toolbar', None)                 and self.paint_toolbar.isVisible():
            self.paint_undo_btn.setEnabled(True)

    def _undo_last_action(self): #vers 2
        """Restore the last deep-copied model from the undo stack."""
        try:
            if not self.undo_stack:
                return
            entry = self.undo_stack.pop()
            idx   = entry['model_index']
            saved = entry['model_data']
            desc  = entry.get('description', '')
            if self.current_col_file:
                models = getattr(self.current_col_file, 'models', [])
                if idx < len(models):
                    models[idx] = saved
                    self._populate_collision_list()
                    self._populate_compact_col_list()
                    if hasattr(self, 'preview_widget'):
                        self.preview_widget.set_current_model(saved, idx)
            if hasattr(self, 'undo_col_btn'):
                self.undo_col_btn.setEnabled(bool(self.undo_stack))
            msg = f"Undo: {desc}" if desc else "Undo applied"
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(msg)
        except Exception as e:
            print(f"Undo error: {e}")

    # ── Selection helpers ─────────────────────────────────────────────────

    def _select_all_models(self): #vers 1
        """Select all entries in the active list (Ctrl+A)."""
        lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
              else self.collision_list)
        lw.selectAll()

    def _invert_selection(self): #vers 1
        """Invert the current selection."""
        lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
              else self.collision_list)
        selected = {i.row() for i in lw.selectionModel().selectedRows()}
        lw.clearSelection()
        lw.setSelectionMode(lw.selectionMode())  # keep mode
        for r in range(lw.rowCount()):
            if r not in selected:
                lw.selectRow(r)  # QTableWidget multi-select needs blockSignals trick
        # Proper invert via selection model
        from PyQt6.QtCore import QItemSelection, QItemSelectionModel
        sel_model = lw.selectionModel()
        full = QItemSelection()
        full.select(lw.model().index(0, 0),
                    lw.model().index(lw.rowCount()-1, lw.columnCount()-1))
        sel_model.select(full, QItemSelectionModel.SelectionFlag.Toggle)

    # ── Sort ──────────────────────────────────────────────────────────────

    def _sort_models(self, key: str = 'name'): #vers 1
        """Sort collision models in place by key: 'name','version','faces','boxes','spheres','vertices'."""
        if not self.current_col_file: return
        models = getattr(self.current_col_file, 'models', [])
        if not models: return

        def sort_key(m):
            if key == 'name':     return (getattr(m,'name','') or '').lower()
            if key == 'version':  return getattr(getattr(m,'version',None),'value',0)
            if key == 'faces':    return len(getattr(m,'faces',[]))
            if key == 'boxes':    return len(getattr(m,'boxes',[]))
            if key == 'spheres':  return len(getattr(m,'spheres',[]))
            if key == 'vertices': return len(getattr(m,'vertices',[]))
            return 0

        models.sort(key=sort_key)
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._set_status(f"Sorted by {key}.")

    def _show_sort_menu(self): #vers 1
        """Show sort options popup."""
        from PyQt6.QtWidgets import QMenu
        m = QMenu(self)
        m.addAction("Sort by Name (A→Z)",     lambda: self._sort_models('name'))
        m.addAction("Sort by Version",         lambda: self._sort_models('version'))
        m.addAction("Sort by Faces (most)",    lambda: self._sort_models_desc('faces'))
        m.addAction("Sort by Boxes (most)",    lambda: self._sort_models_desc('boxes'))
        m.addAction("Sort by Spheres (most)",  lambda: self._sort_models_desc('spheres'))
        m.addAction("Sort by Vertices (most)", lambda: self._sort_models_desc('vertices'))
        m.exec(self.cursor().pos())

    def _sort_models_desc(self, key: str): #vers 1
        """Sort descending (largest first)."""
        if not self.current_col_file: return
        models = getattr(self.current_col_file, 'models', [])
        def k(m):
            return len(getattr(m, key, []))
        models.sort(key=k, reverse=True)
        self._populate_collision_list()
        self._populate_compact_col_list()
        self._set_status(f"Sorted by {key} (descending).")

    # ── Pin / lock entries from editing ───────────────────────────────────

    def _toggle_pin_selected(self): #vers 1
        """Toggle pin (edit-lock) on selected models. Pinned models can't be deleted/renamed."""
        if not self.current_col_file: return
        lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
              else self.collision_list)
        indices = sorted({i.row() for i in lw.selectionModel().selectedRows()})
        cr = lw.currentRow()
        if 0 <= cr: indices = sorted(set(indices) | {cr})
        models = self.current_col_file.models

        if not hasattr(self, '_pinned_models'):
            self._pinned_models = set()

        pinned_now = 0
        for idx in indices:
            if idx < len(models):
                name = getattr(models[idx], 'name', f'model_{idx}')
                if idx in self._pinned_models:
                    self._pinned_models.discard(idx)
                else:
                    self._pinned_models.add(idx)
                    pinned_now += 1

        # Refresh both lists to show pin state
        self._populate_collision_list()
        self._populate_compact_col_list()
        if pinned_now:
            self._set_status(f"Pinned {pinned_now} model(s) — protected from editing.")
        else:
            self._set_status("Unpinned selected model(s).")

    def _is_model_pinned(self, row: int) -> bool: #vers 1
        """Return True if model at row is pinned."""
        return row in getattr(self, '_pinned_models', set())

    # ── IDE-linked operations ─────────────────────────────────────────────

    def _import_via_ide(self): #vers 1
        """Import COL entries referenced by the currently loaded IDE file."""
        from PyQt6.QtWidgets import QMessageBox, QFileDialog
        # Try to get IDE file path from main window DAT browser
        ide_path = None
        if self.main_window and hasattr(self.main_window, 'current_ide_path'):
            ide_path = self.main_window.current_ide_path

        if not ide_path:
            ide_path, _ = QFileDialog.getOpenFileName(
                self, "Select IDE File", "",
                "IDE Files (*.ide);;All Files (*)")
        if not ide_path:
            return

        try:
            # Parse IDE to get model names
            names = []
            with open(ide_path, 'r', errors='ignore') as f:
                in_objs = False
                for line in f:
                    line = line.strip()
                    if line.lower() in ('objs', 'tobj', 'anim'):
                        in_objs = True; continue
                    if line == 'end':
                        in_objs = False; continue
                    if in_objs and line and not line.startswith('#'):
                        parts = line.split(',')
                        if len(parts) >= 2:
                            names.append(parts[1].strip().lower())

            if not names:
                QMessageBox.information(self, "IDE Import",
                    "No model names found in IDE file.")
                return

            # Find matching models in the current COL file
            if not self.current_col_file:
                QMessageBox.warning(self, "No COL File", "Load a COL file first.")
                return

            models = self.current_col_file.models
            matched = [m for m in models
                       if (getattr(m,'name','') or '').lower() in names]

            QMessageBox.information(self, "IDE Import",
                f"IDE has {len(names)} model names.\n"
                f"{len(matched)} matching collision models found in current COL file.\n\n"
                f"Showing matched models in list.")

            # Select matched rows
            lw = (self.col_compact_list if getattr(self,'_col_view_mode','detail')=='detail'
                  else self.collision_list)
            lw.clearSelection()
            name_set = {(getattr(m,'name','') or '').lower() for m in matched}
            for i, model in enumerate(models):
                if (getattr(model,'name','') or '').lower() in name_set:
                    lw.selectRow(i)

            self._set_status(f"IDE: {len(matched)}/{len(names)} models matched.")

        except Exception as e:
            QMessageBox.critical(self, "IDE Import Error", str(e))

    def _remove_via_ide(self): #vers 1
        """Remove collision models NOT referenced by an IDE file (cleanup)."""
        from PyQt6.QtWidgets import QMessageBox, QFileDialog
        if not self.current_col_file:
            QMessageBox.warning(self, "No COL File", "Load a COL file first.")
            return

        ide_path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE to remove unref'd COL models", "",
            "IDE Files (*.ide);;All Files (*)")
        if not ide_path:
            return

        try:
            names = set()
            with open(ide_path, 'r', errors='ignore') as f:
                in_objs = False
                for line in f:
                    line = line.strip()
                    if line.lower() in ('objs', 'tobj', 'anim'):
                        in_objs = True; continue
                    if line == 'end':
                        in_objs = False; continue
                    if in_objs and line and not line.startswith('#'):
                        parts = line.split(',')
                        if len(parts) >= 2:
                            names.add(parts[1].strip().lower())

            models = self.current_col_file.models
            to_remove = [i for i, m in enumerate(models)
                         if (getattr(m,'name','') or '').lower() not in names]

            if not to_remove:
                QMessageBox.information(self, "Remove via IDE",
                    "All COL models are referenced by the IDE — nothing to remove.")
                return

            example_names = [models[i].name for i in to_remove[:5]]
            reply = QMessageBox.question(self, "Remove via IDE",
                f"Remove {len(to_remove)} unreferenced model(s)?\n\n"
                f"Examples: {', '.join(example_names)}"
                + (" ..." if len(to_remove) > 5 else ""),
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if reply != QMessageBox.StandardButton.Yes:
                return

            for i in sorted(to_remove, reverse=True):
                del models[i]
            self._populate_collision_list()
            self._populate_compact_col_list()
            self._set_status(f"Removed {len(to_remove)} unreferenced model(s).")

        except Exception as e:
            QMessageBox.critical(self, "Remove via IDE Error", str(e))

    def _export_via_ide(self): #vers 1
        """Export only COL models referenced by an IDE file."""
        from PyQt6.QtWidgets import QMessageBox, QFileDialog
        import os
        from apps.methods.col_workshop_writer import save_col_file

        if not self.current_col_file:
            QMessageBox.warning(self, "No COL File", "Load a COL file first.")
            return

        ide_path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE File for Export", "",
            "IDE Files (*.ide);;All Files (*)")
        if not ide_path:
            return

        out_path, _ = QFileDialog.getSaveFileName(
            self, "Save filtered COL archive", "",
            "COL Files (*.col);;All Files (*)")
        if not out_path:
            return

        try:
            names = set()
            with open(ide_path, 'r', errors='ignore') as f:
                in_objs = False
                for line in f:
                    line = line.strip()
                    if line.lower() in ('objs', 'tobj', 'anim'):
                        in_objs = True; continue
                    if line == 'end':
                        in_objs = False; continue
                    if in_objs and line and not line.startswith('#'):
                        parts = line.split(',')
                        if len(parts) >= 2:
                            names.add(parts[1].strip().lower())

            matched = [m for m in self.current_col_file.models
                       if (getattr(m,'name','') or '').lower() in names]

            if not matched:
                QMessageBox.warning(self, "No Matches",
                    "No COL models matched the IDE entries.")
                return

            if save_col_file(matched, out_path):
                msg = (f"Exported {len(matched)} IDE-referenced model(s) to:\n"
                       f"{os.path.basename(out_path)}")
                self._set_status(msg)
                QMessageBox.information(self, "Export via IDE", msg)
            else:
                QMessageBox.warning(self, "Export Failed", "Could not write output file.")

        except Exception as e:
            QMessageBox.critical(self, "Export via IDE Error", str(e))

    def _export_col_data(self): #vers 2
        """Extract/export selected COL models (or all) to individual .col files."""
        import os
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        from apps.methods.col_workshop_writer import save_col_file

        if not self.current_col_file:
            QMessageBox.warning(self, "Export", "No COL file loaded.")
            return
        models = getattr(self.current_col_file, 'models', [])
        if not models:
            QMessageBox.warning(self, "Export", "No collision models to export.")
            return

        # Determine selection from the VISIBLE list
        if getattr(self, '_col_view_mode', 'detail') == 'detail':
            lw = getattr(self, 'col_compact_list', None)
        else:
            lw = getattr(self, 'collision_list', None)

        indices = set()
        if lw is not None:
            for idx in lw.selectionModel().selectedRows():
                if idx.row() < len(models):
                    indices.add(idx.row())
            # Also include currentRow() in case selectedRows() missed it
            cr = lw.currentRow()
            if 0 <= cr < len(models):
                indices.add(cr)
        if not indices:
            indices = set(range(len(models)))
        indices = sorted(indices)

        if len(indices) == 1:
            model = models[indices[0]]
            safe = (getattr(model,'name','model') or 'model').replace(' ','_')
            out, _ = QFileDialog.getSaveFileName(
                self, "Export COL Model", safe + '.col', "COL Files (*.col);;All Files (*)")
            if not out: return
            ok = save_col_file([model], out)
            msg = f"Exported {os.path.basename(out)}" if ok else "Export failed."
            ok_count = 1 if ok else 0
        else:
            folder = QFileDialog.getExistingDirectory(
                self, f"Extract {len(indices)} COL models to folder")
            if not folder: return
            ok_count = 0
            for i in indices:
                model = models[i]
                safe = (getattr(model,'name',f'model_{i}') or f'model_{i}').replace(' ','_')
                out  = os.path.join(folder, safe.lower() + '.col')
                base, ext = os.path.splitext(out)
                n = 1
                while os.path.exists(out):
                    out = f"{base}_{n}{ext}"; n += 1
                if save_col_file([model], out):
                    ok_count += 1
            msg = f"Extracted {ok_count} of {len(indices)} model(s) to {folder}"
            ok  = ok_count > 0

        self._set_status(msg)
        if self.main_window and hasattr(self.main_window,'log_message'):
            self.main_window.log_message(msg)
        if ok:
            QMessageBox.information(self, "Extract Complete", msg)
        else:
            QMessageBox.warning(self, "Extract Failed", msg)

    def _save_file(self): #vers 2
        """Save current COL file — serialises all models via COLWriter."""
        if not self.current_col_file:
            QMessageBox.warning(self, "Save", "No COL file loaded to save")
            return

        if not self.current_file_path:
            self._save_file_as()
            return

        models = getattr(self.current_col_file, 'models', [])
        if not models:
            QMessageBox.warning(self, "Save", "No models to save.")
            return

        try:
            from apps.methods.col_workshop_parser import COLWriter
            raw = COLWriter.write_file(models)
        except Exception as e:
            import traceback
            traceback.print_exc()
            QMessageBox.critical(self, "Serialise Error",
                f"Failed to serialise COL data:\n{e}")
            return

        try:
            with open(self.current_file_path, 'wb') as f:
                f.write(raw)
            # Update raw_data so a second Save uses the fresh bytes
            self.current_col_file.raw_data = raw
            fname = os.path.basename(self.current_file_path)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Saved COL: {fname} ({len(models)} models, {len(raw):,} bytes)")
            self._set_status(f"Saved: {fname}")
        except Exception as e:
            QMessageBox.critical(self, "Write Error", str(e))


    def _save_file_as(self): #vers 1
        """Save As dialog"""
        try:
            file_path, _ = QFileDialog.getSaveFileName(
                self,
                "Save COL File As",
                "",
                "COL Files (*.col);;All Files (*)"
            )

            if file_path:
                self.current_file_path = file_path
                self.current_col_file.file_path = file_path
                self._save_file()

        except Exception as e:
            print(f"Error in save as dialog: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to save file:\n{str(e)}")


    def _load_settings(self): #vers 1
        """Load settings from config file"""
        import json

        settings_file = os.path.join(
            os.path.dirname(__file__),
            'col_workshop_settings.json'
        )

        try:
            if os.path.exists(settings_file):
                with open(settings_file, 'r') as f:
                    settings = json.load(f)
                    self.save_to_source_location = settings.get('save_to_source_location', True)
                    self.last_save_directory = settings.get('last_save_directory', None)
        except Exception as e:
            print(f"Failed to load settings: {e}")


    def _save_settings(self): #vers 1
        """Save settings to config file"""
        import json

        settings_file = os.path.join(
            os.path.dirname(__file__),
            'col_workshop_settings.json'
        )

        try:
            settings = {
                'save_to_source_location': self.save_to_source_location,
                'last_save_directory': self.last_save_directory
            }

            with open(settings_file, 'w') as f:
                json.dump(settings, indent=2, fp=f)
        except Exception as e:
            print(f"Failed to save settings: {e}")


    def open_col_file(self, file_path): #vers 3
        """Open standalone COL file - supports COL1, COL2, COL3"""
        try:
            from apps.methods.col_workshop_loader import COLFile

            # Create and load COL file
            # col_file = COLFile()
            # col_file.load_from_file(file_path)

            #from apps.methods.col_workshop_loader import load_col_with_progress
            #col_file = load_col_with_progress(file_path, self)

            #if not col_file:  # Just check if None
            #    return False

            # Large file warning + progress feedback
            import os as _os
            _fsize = _os.path.getsize(file_path)
            _fsize_mb = _fsize / 1024 / 1024
            if _fsize > 512 * 1024 * 1024:  # > 512 MB
                from PyQt6.QtWidgets import QMessageBox
                reply = QMessageBox.question(
                    self, "Large COL File",
                    f"{os.path.basename(file_path)} is {_fsize_mb:.0f} MB.\n\n"
                    "Loading uses memory-mapped I/O to minimise RAM usage, "
                    "but parsing may take 30–60 seconds for very large archives.\n\n"
                    "Continue?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
                if reply != QMessageBox.StandardButton.Yes:
                    return False

            # Show busy cursor for large files
            if _fsize > 32 * 1024 * 1024:
                from PyQt6.QtWidgets import QApplication
                from PyQt6.QtCore import Qt
                QApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)

            col_file = COLFile(debug=(_fsize_mb > 64))
            try:
                if not col_file.load(file_path):
                    return False
            finally:
                if _fsize > 32 * 1024 * 1024:
                    QApplication.restoreOverrideCursor()

            # Store loaded file
            self.current_col_file = col_file
            self.current_file_path = file_path

            # Update window title with model count
            model_count = len(col_file.models) if hasattr(col_file, 'models') else 0
            version_str = f"COL ({model_count} models)"
            self.setWindowTitle(f"{App_name} - {os.path.basename(file_path)} - {version_str}")

            # Populate UI — compact view is default, also populate detail table
            self._populate_compact_col_list()
            self._populate_collision_list()

            # Select first model by default
            active_list = (self.col_compact_list
                          if self._col_view_mode == 'detail'
                          else self.collision_list)
            if active_list.rowCount() > 0:
                active_list.selectRow(0)
                self._select_model_by_row(0)



            # Enable all buttons that require a loaded file
            # Transform buttons: use helper to cover BOTH icon and text panels
            self._set_col_buttons_enabled(True)
            for btn_name in [
                'save_btn', 'save_col_btn', 'saveall_btn',
                'export_col_btn', 'export_all_btn', 'export_btn',
                'import_btn', 'undo_btn', 'undo_col_btn',
                'create_surface_btn', 'paste_btn',
            ]:
                btn = getattr(self, btn_name, None)
                if btn:
                    btn.setEnabled(True)


            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"✅ Loaded COL: {os.path.basename(file_path)} ({model_count} models)")

            print(f"Opened COL file: {file_path} with {model_count} models")
            return True

        except Exception as e:
            print(f"Error opening COL file: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to open COL file:\n{str(e)}")
            return False



    def _find_all_paint_btns(self): #vers 1
        """Return all paint buttons from both icon and text panels."""
        btns = []
        b = getattr(self, 'paint_btn', None)
        if b: btns.append(b)
        # Walk icon panel for any other paint_btn that was overwritten
        ip = getattr(self, '_transform_icon_panel_ref', None)
        if ip:
            from PyQt6.QtWidgets import QPushButton
            for child in ip.findChildren(QPushButton):
                if child.toolTip() and 'paint' in child.toolTip().lower()                    and child not in btns:
                    btns.append(child)
        return btns

    def _set_col_buttons_enabled(self, enabled: bool): #vers 1
        """Enable/disable all transform buttons in BOTH icon and text panels.
        The text panel overwrites self.X refs, so when the icon panel is visible
        (narrow mode) those refs point to hidden buttons. Walk the icon panel too.
        """
        col_btn_attrs = [
            'flip_vert_btn', 'flip_horz_btn', 'rotate_cw_btn', 'rotate_ccw_btn',
            'analyze_btn', 'copy_btn', 'delete_surface_btn', 'duplicate_surface_btn',
            'paint_btn', 'surface_type_btn', 'surface_edit_btn', 'build_from_txd_btn',
            'show_shadow_btn', 'create_shadow_btn', 'remove_shadow_btn',
            'compress_btn', 'uncompress_btn', 'switch_btn', 'convert_btn',
        ]
        for attr in col_btn_attrs:
            btn = getattr(self, attr, None)
            if btn is not None:
                btn.setEnabled(enabled)
        icon_panel = getattr(self, '_transform_icon_panel_ref', None)
        if icon_panel:
            from PyQt6.QtWidgets import QPushButton
            for btn in icon_panel.findChildren(QPushButton):
                btn.setEnabled(enabled)

    def _pick_col_from_current_img(self): #vers 1
        """Pick a COL entry from the IMG currently loaded in IMG Factory and open it."""
        try:
            # Get the main IMG Factory window and its loaded IMG
            mw = self.main_window
            img = getattr(mw, 'current_img', None) if mw else None

            if not img or not getattr(img, 'entries', None):
                QMessageBox.information(self, "No IMG Loaded",
                    "No IMG archive is currently open in IMG Factory.\n"
                    "Open an IMG file first, then use From IMG.")
                return

            col_entries = [e for e in img.entries
                           if getattr(e, 'name', '').lower().endswith('.col')]

            if not col_entries:
                QMessageBox.information(self, "No COL Entries",
                    f"No .col entries found in {os.path.basename(img.file_path)}.")
                return

            # Show a picker dialog
            from PyQt6.QtWidgets import QDialog, QListWidget, QDialogButtonBox, QVBoxLayout, QLabel
            dlg = QDialog(self)
            dlg.setWindowTitle(f"Pick COL — {os.path.basename(img.file_path)}")
            dlg.setMinimumSize(320, 400)
            v = QVBoxLayout(dlg)
            v.addWidget(QLabel(f"{len(col_entries)} COL entries in {os.path.basename(img.file_path)}:"))
            lst = QListWidget()
            for e in col_entries:
                lst.addItem(e.name)
            lst.setCurrentRow(0)
            v.addWidget(lst)
            btns = QDialogButtonBox(
                QDialogButtonBox.StandardButton.Open |
                QDialogButtonBox.StandardButton.Cancel)
            btns.accepted.connect(dlg.accept)
            btns.rejected.connect(dlg.reject)
            v.addWidget(btns)
            lst.doubleClicked.connect(dlg.accept)

            if dlg.exec() != QDialog.DialogCode.Accepted:
                return

            row = lst.currentRow()
            if row < 0:
                return

            entry = col_entries[row]
            self._open_col_from_img_entry(img, entry)

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to pick COL from IMG:\n{e}")

    def _open_col_from_img_entry(self, img, entry): #vers 1
        """Extract a COL entry from an IMGFile and load it into the workshop."""
        try:
            import tempfile
            data = img.read_entry_data(entry)
            if not data:
                QMessageBox.warning(self, "Extract Failed",
                    f"Could not extract {entry.name} from IMG.")
                return

            stem = os.path.splitext(entry.name)[0]
            tmp = tempfile.NamedTemporaryFile(
                delete=False, suffix='.col', prefix=stem + '_')
            tmp.write(data)
            tmp.close()

            self.open_col_file(tmp.name)
            # Retitle with original name
            self.setWindowTitle(
                f"COL Workshop — {entry.name} (from {os.path.basename(img.file_path)})")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"COL Workshop: opened {entry.name} from {os.path.basename(img.file_path)}")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to open {entry.name}:\n{e}")

    def open_img_archive(self): #vers 1
        """Open file dialog to select an IMG archive and load COL entries from it"""
        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self,
                "Open IMG Archive",
                "",
                "IMG Archives (*.img);;All Files (*)"
            )
            if file_path:
                self.load_from_img_archive(file_path)
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to open IMG:\n{str(e)}")


    def load_from_img_archive(self, img_path): #vers 2
        """Load all COL entries from an IMG archive and populate the collision list"""
        try:
            from apps.methods.img_core_classes import IMGFile
            from apps.methods.col_workshop_loader import COLFile

            img = IMGFile(img_path)
            img.open()
            self.current_img = img

            img_name = os.path.basename(img_path)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Scanning {img_name} for COL entries...")

            col_entries = [e for e in img.entries
                           if getattr(e, 'name', '').lower().endswith('.col')]

            if not col_entries:
                QMessageBox.information(self, "No COL Files",
                    f"No .col entries found in {img_name}")
                return False

            # Populate the collision list widget with COL entries
            if hasattr(self, 'collision_list'):
                self.collision_list.clear()
                for entry in col_entries:
                    item = QListWidgetItem(entry.name)
                    item.setData(Qt.ItemDataRole.UserRole, entry)
                    self.collision_list.addItem(item)

            count = len(col_entries)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Loaded {count} COL entr{'ies' if count != 1 else 'y'} from {img_name}")

            self.setWindowTitle(f"COL Workshop: {img_name}")
            return True

        except Exception as e:
            print(f"Error loading from IMG archive: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to load from IMG:\n{str(e)}")
            return False


    def _analyze_collision(self): #vers 1
        """Analyze current COL file"""
        try:
            if not self.current_col_file or not self.current_file_path:
                QMessageBox.warning(self, "Analyze", "No COL file loaded to analyze")
                return

            # Import analysis functions
            from apps.methods.col_operations import get_col_detailed_analysis
            from gui.col_dialogs import show_col_analysis_dialog

            # Get detailed analysis
            analysis_data = get_col_detailed_analysis(self.current_file_path)

            if 'error' in analysis_data:
                QMessageBox.warning(self, "Analysis Error", f"Analysis failed:\n{analysis_data['error']}")
                return

            # Show analysis dialog
            show_col_analysis_dialog(self, analysis_data, os.path.basename(self.current_file_path))

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"✅ Analyzed COL: {os.path.basename(self.current_file_path)}")

        except Exception as e:
            print(f"Error analyzing file: {str(e)}")
            QMessageBox.critical(self, "Error", f"Failed to analyze file:\n{str(e)}")


    def showEvent(self, event): #vers 1
        """When COL workshop becomes visible, try to populate from loaded IMG."""
        super().showEvent(event)
        if (not self.standalone_mode and
                hasattr(self, 'col_list_widget') and
                self.col_list_widget is not None and
                self.col_list_widget.count() == 0):
            # Try to get current IMG from main window
            if self.main_window and hasattr(self.main_window, 'current_img'):
                img = self.main_window.current_img
                if img and img != getattr(self, 'current_img', None):
                    self.current_img = img
                    self._load_img_col_list()

    def _on_col_selected(self, item): #vers 2
        """Handle COL file selection from left panel list."""
        try:
            entry = item.data(Qt.ItemDataRole.UserRole)
            if entry and self.current_img:
                self._open_col_from_img_entry(self.current_img, entry)
        except Exception as e:
            err = str(e)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error selecting COL: {err}")
            else:
                QMessageBox.critical(self, "Error selecting COL", err)


    def _show_col_search(self): #vers 1
        """Toggle COL search box visibility."""
        if hasattr(self, 'col_search_box'):
            visible = not self.col_search_box.isVisible()
            self.col_search_box.setVisible(visible)
            if visible:
                self.col_search_box.setFocus()
            else:
                self.col_search_box.clear()

    def _filter_col_list(self, text: str): #vers 1
        """Filter COL list by search text."""
        if not hasattr(self, 'col_list_widget'): return
        for i in range(self.col_list_widget.count()):
            item = self.col_list_widget.item(i)
            item.setHidden(bool(text) and text.lower() not in item.text().lower())


    def _extract_col_from_img(self, entry): #vers 2
        """Extract TXD data from IMG entry"""
        try:
            if not self.current_img:
                return None
            return self.current_img.read_entry_data(entry)
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Extract error: {str(e)}")
            return None



    def _paint_model_onto(self, painter, model, W, H,
                          yaw, pitch, zoom, pan_x, pan_y,
                          flip_h, flip_v,
                          show_spheres, show_boxes, show_mesh,
                          backface, render_style, bg_color,
                          gizmo_mode='translate', viewport=None): #vers 4
        """Paint COL model: grid → geometry → gizmo → HUD.
        Uses viewport's own projection when available (consistent coords)."""
        from PyQt6.QtGui import QPen, QBrush, QColor, QFont, QPolygonF
        from PyQt6.QtCore import QPointF
        import math

        # ── Use viewport's projection if available ────────────────────────
        if viewport is not None:
            vp = viewport
        else:
            vp = getattr(self, 'preview_widget', None)

        if vp is not None:
            scale, ox, oy = vp._get_scale_origin()
            def proj3(x,y,z): return vp._proj(x,y,z)
            def to_screen(x,y,z):
                px,py = proj3(x,y,z)
                return px*scale+ox, py*scale+oy
        else:
            # Fallback standalone projection
            yr = math.radians(yaw);   cy, sy = math.cos(yr), math.sin(yr)
            pr = math.radians(pitch); cp, sp = math.cos(pr), math.sin(pr)
            def proj3(x,y,z):
                rx=x*cy-y*sy; ry=x*sy+y*cy
                return rx, ry*cp-z*sp
            scale, ox, oy, _ = self._project_model_2d(model, W, H, padding=20,
                                                        yaw=yaw, pitch=pitch)
            def to_screen(x,y,z):
                px,py=proj3(x,y,z); return px*scale+ox, py*scale+oy

        # ── Model geometry extent for grid sizing ─────────────────────────
        verts = getattr(model, 'vertices', [])
        if verts:
            xs=[v.x for v in verts]; ys=[v.y for v in verts]; zs=[v.z for v in verts]
            extent = max(max(abs(x) for x in xs+ys+zs), 1.0)
        else:
            extent = 5.0

        # ── Reference grid (XY plane Z=0) ────────────────────────────────
        raw_step = extent / 4.0
        mag  = 10 ** math.floor(math.log10(max(raw_step, 0.001)))
        step = round(raw_step / mag) * mag; step = max(step, 0.01)
        half = math.ceil(extent / step + 1) * step
        n    = int(half / step)

        painter.setRenderHint(painter.renderHints().__class__.Antialiasing, False)
        for i in range(-n, n+1):
            v2 = i * step
            col = QColor(75, 80, 105) if i == 0 else QColor(50, 55, 72)
            painter.setPen(QPen(col, 1))
            x0,y0 = to_screen(-half, v2, 0); x1,y1 = to_screen(half, v2, 0)
            painter.drawLine(int(x0),int(y0),int(x1),int(y1))
            x0,y0 = to_screen(v2, -half, 0); x1,y1 = to_screen(v2, half, 0)
            painter.drawLine(int(x0),int(y0),int(x1),int(y1))
        painter.setRenderHint(painter.renderHints().__class__.Antialiasing, True)

        # ── Model geometry (uses viewport's to_screen — zoom/pan consistent) ──
        from PyQt6.QtGui import QPolygonF as _PF
        from PyQt6.QtCore import QPointF as _P, QRectF as _R

        def g3(obj):
            """Get (x,y,z) from vertex, sphere centre, or tuple."""
            if hasattr(obj,'x'):        return obj.x, obj.y, obj.z
            if hasattr(obj,'position'): return obj.position.x,obj.position.y,obj.position.z
            return float(obj[0]),float(obj[1]),float(obj[2])

        # Mesh faces
        if show_mesh:
            _verts = getattr(model,'vertices',[])
            _faces = getattr(model,'faces',[])
            if _verts and _faces:
                painter.setPen(QPen(QColor(120,180,120,180), 0.5))
                painter.setBrush(QBrush(QColor(60,120,60,80)))
                for face in _faces:
                    idx = getattr(face,'vertex_indices',None)
                    if idx is None:
                        fa = getattr(face,'a',None)
                        if fa is not None: idx=(fa,face.b,face.c)
                    if idx and len(idx)==3:
                        try:
                            pts=[_P(*to_screen(*g3(_verts[i]))) for i in idx]
                            painter.drawPolygon(_PF(pts))
                        except (IndexError,AttributeError):
                            pass

        # Boxes
        if show_boxes:
            painter.setPen(QPen(QColor(220,180,50),1.5))
            painter.setBrush(QBrush(QColor(220,180,50,40)))
            for box in getattr(model,'boxes',[]):
                bmin = box.min_point if hasattr(box,'min_point') else box.min
                bmax = box.max_point if hasattr(box,'max_point') else box.max
                x1,y1=to_screen(*g3(bmin)); x2,y2=to_screen(*g3(bmax))
                painter.drawRect(_R(min(x1,x2),min(y1,y2),abs(x2-x1) or 2,abs(y2-y1) or 2))

        # Spheres
        if show_spheres:
            painter.setPen(QPen(QColor(80,200,220),1.5))
            painter.setBrush(QBrush(QColor(80,200,220,40)))
            for sph in getattr(model,'spheres',[]):
                cx,cy,cz=g3(sph.center) if hasattr(sph,'center') else (0,0,0)
                r = sph.radius * scale
                sx,sy=to_screen(cx,cy,cz)
                painter.drawEllipse(_R(sx-r,sy-r,r*2 or 2,r*2 or 2))

        # ── Gizmo at model centroid ───────────────────────────────────────
        if verts:
            cx3=sum(v.x for v in verts)/len(verts)
            cy3=sum(v.y for v in verts)/len(verts)
            cz3=sum(v.z for v in verts)/len(verts)
        else:
            cx3=cy3=cz3=0.0
        gx, gy = to_screen(cx3, cy3, cz3)
        arm = max(45, min(W,H) * 0.15)

        axes = [((1,0,0),QColor(220,60,60),'X'),
                ((0,1,0),QColor(60,200,60),'Y'),
                ((0,0,1),QColor(60,120,220),'Z')]
        sorted_axes = sorted(axes, key=lambda a: proj3(*a[0])[1], reverse=True)

        if gizmo_mode == 'translate':
            for (dx,dy,dz), color, label in sorted_axes:
                px,py = proj3(dx,dy,dz)
                tx,ty = gx+px*arm, gy+py*arm
                painter.setPen(QPen(color,2))
                painter.drawLine(int(gx),int(gy),int(tx),int(ty))
                ang = math.atan2(ty-gy, tx-gx); aw,ah = 12,6
                tip  = QPointF(tx,ty)
                lpt  = QPointF(tx-aw*math.cos(ang)+ah*math.sin(ang), ty-aw*math.sin(ang)-ah*math.cos(ang))
                rpt  = QPointF(tx-aw*math.cos(ang)-ah*math.sin(ang), ty-aw*math.sin(ang)+ah*math.cos(ang))
                painter.setBrush(QBrush(color)); painter.setPen(QPen(color,1))
                painter.drawPolygon(QPolygonF([tip,lpt,rpt]))
                lx=tx+(9 if tx>=gx else -14); ly=ty+(5 if ty>=gy else -3)
                painter.setFont(QFont('Arial',8,QFont.Weight.Bold))
                painter.setPen(color); painter.drawText(int(lx),int(ly),label)
        else:
            # Rotate rings
            N = 64
            rings = [((1,0,0),(0,1,0),(0,0,1),QColor(220,60,60),'X'),
                     ((0,1,0),(1,0,0),(0,0,1),QColor(60,200,60),'Y'),
                     ((0,0,1),(1,0,0),(0,1,0),QColor(60,120,220),'Z')]
            for (_,t1,t2,color,label) in sorted(rings, key=lambda r: proj3(*r[0])[1], reverse=True):
                t1x,t1y,t1z=t1; t2x,t2y,t2z=t2
                pts=[]
                for i in range(N+1):
                    a2=2*math.pi*i/N
                    wx=math.cos(a2)*t1x+math.sin(a2)*t2x
                    wy=math.cos(a2)*t1y+math.sin(a2)*t2y
                    wz=math.cos(a2)*t1z+math.sin(a2)*t2z
                    px2,py2=proj3(wx,wy,wz)
                    pts.append(QPointF(gx+px2*arm, gy+py2*arm))
                painter.setPen(QPen(color,2)); painter.setBrush(Qt.BrushStyle.NoBrush)
                for i in range(len(pts)-1): painter.drawLine(pts[i],pts[i+1])
                p45x=math.cos(math.pi/4)*t1x+math.sin(math.pi/4)*t2x
                p45y=math.cos(math.pi/4)*t1y+math.sin(math.pi/4)*t2y
                p45z=math.cos(math.pi/4)*t1z+math.sin(math.pi/4)*t2z
                lp,lq=proj3(p45x,p45y,p45z)
                painter.setFont(QFont('Arial',8,QFont.Weight.Bold)); painter.setPen(color)
                painter.drawText(int(gx+lp*arm+(6 if lp>=0 else -12)),
                                 int(gy+lq*arm+(5 if lq>=0 else -3)), label)

        # Gizmo centre dot
        painter.setBrush(QBrush(QColor(230,230,230))); painter.setPen(QPen(QColor(160,160,160),1))
        painter.drawEllipse(int(gx)-5,int(gy)-5,10,10)

        # ── Toggle button (top-right) ─────────────────────────────────────
        bx,by,bw,bh = W-70,4,66,22
        painter.setBrush(QBrush(QColor(40,44,62)))
        painter.setPen(QPen(QColor(80,90,130),1))
        painter.drawRoundedRect(bx,by,bw,bh,4,4)
        painter.setFont(QFont('Arial',8)); painter.setPen(QColor(200,200,220))
        lbl = '↕ Move [G]' if gizmo_mode=='translate' else '↻ Rotate [R]'
        painter.drawText(bx+4,by+15,lbl)

        # ── HUD ───────────────────────────────────────────────────────────
        painter.setFont(QFont('Arial',8)); painter.setPen(QColor(200,200,200))
        painter.drawText(6,14,getattr(model,'name',''))
        y2=H-54
        spheres=getattr(model,'spheres',[]); boxes=getattr(model,'boxes',[])
        faces=getattr(model,'faces',[])
        for col_c,txt in [(QColor(100,180,100),f"Mesh  F:{len(faces)} V:{len(verts)}"),
                          (QColor(220,180,50), f"Boxes  {len(boxes)}"),
                          (QColor(80,200,220), f"Spheres  {len(spheres)}")]:
            painter.setPen(col_c); painter.drawText(6,y2,txt); y2+=14
        painter.setPen(QColor(120,125,140)); painter.setFont(QFont('Arial',7))
        painter.drawText(6,H-4,f"Y:{yaw:.0f}° P:{pitch:.0f}° Z:{zoom:.2f}x")
        painter.drawText(W-68,H-4,f"grid {step:.3g}")


    def _get_view_coords(self, model, view='xy'): #vers 1
        """Get all geometry points projected to 2D using the selected view axis."""
        def vc(v):
            if hasattr(v, 'position'): return (v.position.x, v.position.y, v.position.z)
            return (v.x, v.y, v.z)
        def sc(s):
            c = s.center
            if hasattr(c, 'x'): return (c.x, c.y, c.z)
            return (c[0], c[1], c[2])
        def bc(b, which):
            pt = b.min_point if which=='min' else b.max_point if hasattr(b,'min_point') else (b.min if which=='min' else b.max)
            if hasattr(pt, 'x'): return (pt.x, pt.y, pt.z)
            return (pt[0], pt[1], pt[2])

        # Map view to (horiz_idx, vert_idx) in 3D coords
        axes = {'xy': (0,1), 'xz': (0,2), 'yz': (1,2)}
        hi, vi = axes.get(view, (0,1))

        pts = []
        for s in getattr(model, 'spheres', []):
            x,y,z = sc(s); r = s.radius
            coords = (x,y,z); px,py = coords[hi], coords[vi]
            pts += [(px-r,py-r),(px+r,py+r)]
        for b in getattr(model, 'boxes', []):
            mn = bc(b,'min'); mx = bc(b,'max')
            pts += [(mn[hi],mn[vi]),(mx[hi],mx[vi])]
        for v in getattr(model, 'vertices', []):
            x,y,z = vc(v)
            coords = (x,y,z)
            pts.append((coords[hi], coords[vi]))
        return pts

    def _project_model_2d(self, model, width, height, padding=8,
                          yaw=0.0, pitch=0.0,
                          flip_h=False, flip_v=False): #vers 3
        """Project COL model geometry to 2D canvas using yaw/pitch rotation."""
        import math
        def _rot(pts3):
            result = []
            yr = math.radians(yaw)
            pr = math.radians(pitch)
            cy, sy = math.cos(yr), math.sin(yr)
            cp, sp = math.cos(pr), math.sin(pr)
            for x, y, z in pts3:
                # yaw around Z axis
                rx = x*cy - y*sy
                ry = x*sy + y*cy
                rz = z
                # pitch around X axis
                rx2 = rx
                ry2 = ry*cp - rz*sp
                rz2 = ry*sp + rz*cp
                result.append((rx2, ry2))  # project onto screen plane
            return result

        def _pts3(model):
            def vc(v):
                if hasattr(v,'position'): return (v.position.x,v.position.y,v.position.z)
                return (v.x,v.y,v.z)
            def sc(s):
                c = s.center
                if hasattr(c,'x'): return (c.x,c.y,c.z)
                return (c[0],c[1],c[2])
            def bc(b, mn):
                pt = (b.min_point if mn else b.max_point) if hasattr(b,'min_point') else (b.min if mn else b.max)
                if hasattr(pt,'x'): return (pt.x,pt.y,pt.z)
                return (pt[0],pt[1],pt[2])
            pts = []
            for s in getattr(model,'spheres',[]): x,y,z=sc(s); r=s.radius; pts+=[(x-r,y-r,z-r),(x+r,y+r,z+r)]
            for b in getattr(model,'boxes',  []): pts+=[bc(b,True),bc(b,False)]
            for v in getattr(model,'vertices',[]): pts.append(vc(v))
            return pts

        pts_3d = _pts3(model)
        pts_2d = _rot(pts_3d) if pts_3d else []
        if not pts_2d:
            return 1.0, width//2, height//2, []
        xs = [p[0] for p in pts_2d]
        ys = [p[1] for p in pts_2d]
        mn_x, mx_x = min(xs), max(xs)
        mn_y, mx_y = min(ys), max(ys)
        rng_x = mx_x - mn_x or 1.0
        rng_y = mx_y - mn_y or 1.0
        scale = min((width - padding*2) / rng_x, (height - padding*2) / rng_y)
        cx = (mn_x + mx_x) / 2
        cy = (mn_y + mx_y) / 2
        ox = width  / 2 - cx * scale
        oy = height / 2 - cy * scale

        result = []
        for px, py in pts_2d:
            sx = px * scale + ox
            sy = py * scale + oy
            if flip_h: sx = width - sx
            if flip_v: sy = height - sy
            result.append((sx, sy))
        return scale, ox, oy, result

    def _draw_col_model(self, painter, model, width, height, padding=4,
                       yaw=0.0, pitch=0.0,
                       flip_h=False, flip_v=False): #vers 3
        """Draw COL model onto a QPainter — used by both thumbnail and preview."""
        from PyQt6.QtGui import QPen, QBrush, QColor
        from PyQt6.QtCore import QRectF, QPointF
        import math

        import math
        scale, ox, oy, _ = self._project_model_2d(
            model, width, height, padding,
            yaw=yaw, pitch=pitch, flip_h=flip_h, flip_v=flip_v)

        yr = math.radians(yaw);  cy, sy = math.cos(yr), math.sin(yr)
        pr = math.radians(pitch); cp, sp = math.cos(pr), math.sin(pr)

        def _to2d(x, y, z):
            rx  = x*cy - y*sy
            ry  = x*sy + y*cy
            rx2 = rx
            ry2 = ry*cp - z*sp
            sx = rx2 * scale + ox
            sy2 = ry2 * scale + oy
            if flip_h: sx  = width  - sx
            if flip_v: sy2 = height - sy2
            return sx, sy2

        def _get3(obj):
            if hasattr(obj,'x'):        return obj.x, obj.y, obj.z
            elif hasattr(obj,'position'): return obj.position.x, obj.position.y, obj.position.z
            else: return float(obj[0]), float(obj[1]), float(obj[2])

        def proj_pt(obj):
            return _to2d(*_get3(obj))

        def wx(v): return (width  - (v*scale+ox)) if flip_h else (v*scale+ox)
        def wy(v): return (height - (v*scale+oy)) if flip_v else (v*scale+oy)

        # Mesh faces — filled triangles (grey)
        verts = getattr(model, 'vertices', [])
        faces = getattr(model, 'faces', [])
        if verts and faces:
            painter.setPen(QPen(QColor(120, 180, 120, 180), 0.5))
            painter.setBrush(QBrush(QColor(60, 120, 60, 80)))
            from PyQt6.QtGui import QPolygonF
            for face in faces:
                idx = getattr(face, 'vertex_indices', None)
                if idx is None:
                    fa = getattr(face, 'a', None)
                    if fa is not None:
                        idx = (fa, face.b, face.c)
                if idx and len(idx) == 3:
                    try:
                        p0x, p0y = proj_pt(verts[idx[0]])
                        p1x, p1y = proj_pt(verts[idx[1]])
                        p2x, p2y = proj_pt(verts[idx[2]])
                        poly = QPolygonF([
                            QPointF(p0x, p0y),
                            QPointF(p1x, p1y),
                            QPointF(p2x, p2y),
                        ])
                        painter.drawPolygon(poly)
                    except (IndexError, AttributeError):
                        pass

        # Boxes — yellow outline
        painter.setPen(QPen(QColor(220, 180, 50), max(1.0, scale * 0.05)))
        painter.setBrush(QBrush(QColor(220, 180, 50, 40)))
        for box in getattr(model, 'boxes', []):
            bmin_obj = box.min_point if hasattr(box, 'min_point') else box.min
            bmax_obj = box.max_point if hasattr(box, 'max_point') else box.max
            x1, y1 = proj_pt(bmin_obj)
            x2, y2 = proj_pt(bmax_obj)
            painter.drawRect(QRectF(min(x1,x2), min(y1,y2),
                                    abs(x2-x1) or 2, abs(y2-y1) or 2))

        # Spheres — cyan outline
        painter.setPen(QPen(QColor(80, 200, 220), max(1.0, scale * 0.05)))
        painter.setBrush(QBrush(QColor(80, 200, 220, 40)))
        for sph in getattr(model, 'spheres', []):
            r = sph.radius * scale
            cx, cy = proj_pt(sph.center)
            painter.drawEllipse(QRectF(cx - r, cy - r, r * 2 or 2, r * 2 or 2))

    def _generate_collision_thumbnail(self, model, width=64, height=64,
                                      yaw=0.0, pitch=0.0): #vers 2
        """Generate a small QPixmap thumbnail of a COL model."""
        from PyQt6.QtGui import QPixmap, QPainter, QColor, QPen
        pixmap = QPixmap(width, height)
        pixmap.fill(QColor(30, 30, 40))
        has_data = (getattr(model, 'spheres', []) or
                    getattr(model, 'boxes', []) or
                    getattr(model, 'vertices', []))
        if not has_data:
            painter = QPainter(pixmap)
            painter.setPen(QPen(QColor(80, 80, 80), 1))
            painter.drawLine(4, 4, width-4, height-4)
            painter.drawLine(width-4, 4, 4, height-4)
            painter.end()
            return pixmap
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        self._draw_col_model(painter, model, width, height, padding=4,
                             yaw=yaw, pitch=pitch)
        painter.end()
        return pixmap

    def _render_collision_preview(self, model, width=400, height=400,
                                  yaw=0.0, pitch=0.0,
                                  flip_h=False, flip_v=False): #vers 3
        """Render a full-size QPixmap preview of a COL model.
        yaw/pitch are Euler angles in degrees for free rotation.
        """
        from PyQt6.QtGui import QPixmap, QPainter, QColor, QFont
        from PyQt6.QtCore import Qt
        pixmap = QPixmap(width, height)
        pixmap.fill(QColor(25, 25, 35))
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        has_data = (getattr(model, 'spheres', []) or
                    getattr(model, 'boxes', []) or
                    getattr(model, 'vertices', []))

        if not has_data:
            painter.setPen(QColor(120, 120, 120))
            painter.setFont(QFont('Arial', 11))
            painter.drawText(pixmap.rect(), Qt.AlignmentFlag.AlignCenter, "No geometry data")
            painter.end()
            return pixmap

        self._draw_col_model(painter, model, width, height, padding=20,
                            yaw=yaw, pitch=pitch,
                            flip_h=flip_h, flip_v=flip_v)

        # Legend
        painter.setFont(QFont('Arial', 8))
        y = height - 52
        for color, label in [
            (QColor(60, 120, 60),   f"Mesh  F:{len(getattr(model,'faces',[]))} V:{len(getattr(model,'vertices',[]))}"),
            (QColor(220, 180, 50),  f"Boxes  {len(getattr(model,'boxes',[]))}"),
            (QColor(80, 200, 220),  f"Spheres  {len(getattr(model,'spheres',[]))}"),
        ]:
            painter.setPen(color)
            painter.drawText(6, y, label)
            y += 14

        # Model name
        name = getattr(model, 'name', '')
        if name:
            painter.setPen(QColor(200, 200, 200))
            painter.setFont(QFont('Arial', 9))
            painter.drawText(6, 14, name)

        painter.end()
        return pixmap

    def _on_collision_selected(self): #vers 8
        """Handle [T] detail table selection."""
        try:
            rows = self.collision_list.selectionModel().selectedRows()
            if rows:
                self._select_model_by_row(rows[0].row())
        except Exception as e:
            print("_on_collision_selected error: " + str(e))

    def _select_model_by_row(self, row): #vers 2
        """Load model by row index into preview — works for both list views."""
        try:
            if not self.current_col_file:
                return
            models = getattr(self.current_col_file, 'models', [])
            if row < 0 or row >= len(models):
                return
            model = models[row]
            model_name = getattr(model, 'name', f'Model_{row}')

            # Debug counts
            nb = len(getattr(model,'boxes',[]));  ns = len(getattr(model,'spheres',[]))
            nv = len(getattr(model,'vertices',[])); nf = len(getattr(model,'faces',[]))
            print(f"SELECT [{row}] {model_name}: V={nv} F={nf} B={nb} S={ns}")

            # Name field
            if hasattr(self, 'info_name'):
                self.info_name.setText(model_name)

            # Push model into viewport
            pw = getattr(self, 'preview_widget', None)
            if pw:
                if VIEWPORT_AVAILABLE and isinstance(pw, COL3DViewport):
                    pw.set_current_model(model, row)
                else:
                    w = max(400, pw.width()); h = max(400, pw.height())
                    pw.setPixmap(self._render_collision_preview(model, w, h))
                    pw.setScaledContents(False)

            # Spin thumbnail in detail list
            self._start_thumbnail_spin(row, model)

        except Exception as e:
            import traceback; traceback.print_exc()
            print(f"_select_model_by_row error: {e}")


    def _show_collision_context_menu(self, position): #vers 4
        """Right-click context menu for both collision model lists."""
        # Work out which list sent the signal and find the row
        sender = self.sender()
        if sender is self.col_compact_list:
            source_list = self.col_compact_list
        else:
            source_list = self.collision_list

        item = source_list.itemAt(position)
        if not item:
            # Still show thumbnail-view submenu even on empty area
            row, model = -1, None
        else:
            row = source_list.row(item)
            if row < 0: row = -1
            models = getattr(self.current_col_file, 'models', []) if self.current_col_file else []
            model = models[row] if 0 <= row < len(models) else None

        menu = QMenu(self)

        # ── Thumbnail view submenu (always shown) ─────────────────────────
        view_menu = menu.addMenu("Thumbnail View")
        axes = [
            ("Top  (XY — Z up)",    0,   0),
            ("Front (XZ — Y fwd)", 0,  90),
            ("Side  (YZ — X right)",90,  0),
            ("Isometric",          45,  35),
            ("Bottom",              0, 180),
            ("Back",              180,  90),
        ]
        for label, yaw, pitch in axes:
            # Tick current selection
            is_current = (abs(self._thumb_yaw - yaw) < 0.5 and
                          abs(self._thumb_pitch - pitch) < 0.5)
            act = view_menu.addAction(("✓ " if is_current else "    ") + label)
            act.triggered.connect(
                lambda _=False, y=yaw, p=pitch, l=label:
                    self._set_thumbnail_view(y, p, l))

        if model is not None:
            menu.addSeparator()

            # ── Info ──────────────────────────────────────────────────────
            details_action = menu.addAction("Show Details")
            details_action.triggered.connect(lambda: self._show_model_details(model, row))

            copy_action = menu.addAction("Copy Info to Clipboard")
            copy_action.triggered.connect(lambda: self._copy_model_info(model, row))

            menu.addSeparator()

            # ── Rename ────────────────────────────────────────────────────
            rename_action = menu.addAction("Rename Model...")
            rename_action.triggered.connect(lambda: self._rename_col_model(model, row))

            menu.addSeparator()

            # ── Export / Replace ──────────────────────────────────────────
            export_action = menu.addAction("Export Model as COL...")
            export_action.triggered.connect(lambda: self._export_col_model(model, row))

            import_action = menu.addAction("Replace with COL file...")
            import_action.triggered.connect(lambda: self._import_replace_col_model(row))

            menu.addSeparator()

            # ── Pin (protect from editing) ─────────────────────────────
            is_pinned = self._is_model_pinned(row)
            pin_action = menu.addAction(
                "📌 Unpin (allow editing)" if is_pinned else "📌 Pin (protect from editing)")
            pin_action.triggered.connect(self._toggle_pin_selected)

        menu.addSeparator()

        # ── Select / Sort ──────────────────────────────────────────────
        menu.addAction("Select All  [Ctrl+A]",  self._select_all_models)
        menu.addAction("Invert Selection  [Ctrl+I]", self._invert_selection)
        menu.addAction("Sort…",                 self._show_sort_menu)

        menu.addSeparator()

        # ── IDE-linked operations ──────────────────────────────────────
        ide_menu = menu.addMenu("IDE Operations")
        ide_menu.addAction("Import matched by IDE…",    self._import_via_ide)
        ide_menu.addAction("Export matched by IDE…",    self._export_via_ide)
        ide_menu.addAction("Remove unreferenced by IDE…", self._remove_via_ide)

        menu.exec(source_list.mapToGlobal(position))

    def _rename_col_model(self, model, row): #vers 1
        """Rename a collision model entry in the list."""
        try:
            from PyQt6.QtWidgets import QInputDialog
            old_name = getattr(model, 'name', f'Model_{row}')
            new_name, ok = QInputDialog.getText(
                self, "Rename Model", "New model name:", text=old_name)
            if not ok or not new_name.strip():
                return
            new_name = new_name.strip()
            model.name = new_name
            # Update table cell
            name_item = self.collision_list.item(row, 0)
            if name_item:
                name_item.setText(new_name)
            # Mark file as modified
            if hasattr(self, 'save_btn'):
                self.save_btn.setEnabled(True)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Renamed model {row}: '{old_name}' → '{new_name}'")
        except Exception as e:
            QMessageBox.critical(self, "Rename Error", str(e))

    def _export_col_model(self, model, row): #vers 1
        """Export a single collision model as a standalone COL file."""
        try:
            model_name = getattr(model, 'name', f'model_{row}')
            default_name = model_name.lower().replace(' ', '_') + '.col'
            file_path, _ = QFileDialog.getSaveFileName(
                self, "Export COL Model",
                default_name, "COL Files (*.col);;All Files (*)")
            if not file_path:
                return

            # Build a minimal COL file containing just this model
            from apps.methods.col_workshop_loader import COLFile
            out = COLFile()
            out.models = [model]
            if hasattr(out, 'save'):
                if not out.save(file_path):
                    QMessageBox.warning(self, "Export Failed",
                        "Could not save COL model — save() returned False.")
                    return
            else:
                # Fallback: write the raw bytes of the model
                raw = getattr(model, '_raw_bytes', None)
                if raw:
                    with open(file_path, 'wb') as f:
                        f.write(raw)
                else:
                    QMessageBox.warning(self, "Export Failed",
                        "No serialisation method available for this model.")
                    return

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Exported model '{model_name}' → {os.path.basename(file_path)}")
            QMessageBox.information(self, "Export OK",
                f"Model '{model_name}' exported to:\n{file_path}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _import_replace_col_model(self, row): #vers 1
        """Replace a collision model entry from an external COL file."""
        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self, "Import COL to Replace Model",
                "", "COL Files (*.col);;All Files (*)")
            if not file_path:
                return

            from apps.methods.col_workshop_loader import COLFile
            new_col = COLFile()
            if not new_col.load(file_path):
                QMessageBox.warning(self, "Import Failed",
                    f"Could not load {os.path.basename(file_path)}")
                return

            new_models = getattr(new_col, 'models', [])
            if not new_models:
                QMessageBox.warning(self, "No Models",
                    f"No collision models found in {os.path.basename(file_path)}")
                return

            # If multiple models in the source, ask which one to use
            src_model = new_models[0]
            if len(new_models) > 1:
                from PyQt6.QtWidgets import QInputDialog
                names = [f"[{i}] {getattr(m,'name',f'Model_{i}')}"
                         for i, m in enumerate(new_models)]
                choice, ok = QInputDialog.getItem(
                    self, "Choose Model",
                    f"{len(new_models)} models in file — pick one to import:",
                    names, 0, False)
                if not ok:
                    return
                idx = names.index(choice)
                src_model = new_models[idx]

            old_name = getattr(
                self.current_col_file.models[row], 'name', f'Model_{row}')
            src_model.name = old_name  # preserve existing name
            self.current_col_file.models[row] = src_model

            # Refresh the table row
            from apps.methods.populate_col_table import populate_col_table
            populate_col_table(self, self.current_col_file)
            self.collision_list.selectRow(row)

            if hasattr(self, 'save_btn'):
                self.save_btn.setEnabled(True)
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Replaced model {row} ('{old_name}') from "
                    f"{os.path.basename(file_path)}")
        except Exception as e:
            QMessageBox.critical(self, "Import Error", str(e))


    def _show_model_details(self, model, index): #vers 1
        """Show detailed model information dialog"""
        from PyQt6.QtWidgets import QDialog, QTextEdit, QVBoxLayout, QPushButton

        dialog = QDialog(self)
        dialog.setWindowTitle(f"Model Details - {model.name}")
        dialog.setMinimumSize(500, 400)

        layout = QVBoxLayout(dialog)

        # Create detailed info text
        info_text = f"""Model: {model.name}
    Index: {index}
    Version: {model.version.name if hasattr(model.version, 'name') else model.version}

    Bounding Box:
    Center: ({model.bounding_box.center.x:.3f}, {model.bounding_box.center.y:.3f}, {model.bounding_box.center.z:.3f})
    Min: ({model.bounding_box.min.x:.3f}, {model.bounding_box.min.y:.3f}, {model.bounding_box.min.z:.3f})
    Max: ({model.bounding_box.max.x:.3f}, {model.bounding_box.max.y:.3f}, {model.bounding_box.max.z:.3f})
    Radius: {model.bounding_box.radius:.3f}

    Collision Data:
    Spheres: {len(model.spheres)}
    Boxes: {len(model.boxes)}
    Vertices: {len(model.vertices)}
    Faces: {len(model.faces)}

    """

        # Add first 3 vertices if available
        if len(model.vertices) > 0:
            info_text += "\nVertices:\n"
            for i in range(min(30000, len(model.vertices))):
                v = model.vertices[i]
                if hasattr(v, 'position'):
                    info_text += f"  [{i}] ({v.position.x:.3f}, {v.position.y:.3f}, {v.position.z:.3f})\n"
                else:
                    info_text += f"  [{i}] ({v.x:.3f}, {v.y:.3f}, {v.z:.3f})\n"

        # Add material info from faces
        if len(model.faces) > 0:
            materials = set()
            for face in model.faces:
                if hasattr(face, 'material'):
                    mat_id = face.material.material_id if hasattr(face.material, 'material_id') else face.material
                    materials.add(mat_id)
            info_text += f"\nUnique Materials: {len(materials)}\n"
            info_text += f"Material IDs: {sorted(materials)}\n"

        text_edit = QTextEdit()
        text_edit.setPlainText(info_text)
        text_edit.setReadOnly(True)
        layout.addWidget(text_edit)

        # Copy button
        copy_btn = QPushButton("Copy to Clipboard")
        copy_btn.clicked.connect(lambda: self._copy_text_to_clipboard(info_text))
        layout.addWidget(copy_btn)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        layout.addWidget(close_btn)

        dialog.exec()


    def _copy_model_info(self, model, index): #vers 1
        """Copy model info to clipboard"""
        info = f"{model.name} | S:{len(model.spheres)} B:{len(model.boxes)} V:{len(model.vertices)} F:{len(model.faces)}"
        self._copy_text_to_clipboard(info)
        if hasattr(self, 'status_bar'):
            self.status_bar.showMessage("Model info copied to clipboard", 2000)


    def _copy_text_to_clipboard(self, text): #vers 1
        """Copy text to system clipboard"""
        from PyQt6.QtWidgets import QApplication
        clipboard = QApplication.clipboard()
        clipboard.setText(text)


    def _populate_collision_list(self): #vers 7
        """Populate [T] detail table — 8 columns, icon badges on counts > 0."""
        try:
            self.collision_list.setRowCount(0)
            if not self.current_col_file or not hasattr(self.current_col_file, 'models'):
                return

            # Build icon pixmaps once (16px, themed colour)
            icon_color = self._get_icon_color()
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
            from PyQt6.QtGui import QPixmap

            def _px(icon_fn, color=None):
                """Get 14px QPixmap from an SVG icon factory method."""
                try:
                    ico = icon_fn(size=14, color=color or icon_color)
                    return ico.pixmap(14, 14)
                except Exception:
                    return QPixmap()

            sphere_px = _px(_SVG.sphere_icon, '#50c8e0')   # cyan
            box_px    = _px(_SVG.box_icon,    '#dcb432')   # yellow
            face_px   = _px(_SVG.mesh_icon,   '#6496dc')   # blue
            # No dedicated vert icon — draw a tiny dot pixmap inline
            vert_px   = QPixmap(14, 14)
            from PyQt6.QtGui import QPainter, QColor, QBrush
            from PyQt6.QtCore import QRectF
            vert_px.fill(QColor(0, 0, 0, 0))
            vp = QPainter(vert_px)
            vp.setRenderHint(QPainter.RenderHint.Antialiasing)
            vp.setBrush(QBrush(QColor(100, 200, 120)))
            vp.setPen(QColor(100, 200, 120))
            for ox, oy in [(2,2),(8,2),(5,9)]:
                vp.drawEllipse(QRectF(ox, oy, 4, 4))
            vp.end()

            icon_map = {4: sphere_px, 5: box_px, 6: vert_px, 7: face_px}

            models = self.current_col_file.models
            self.collision_list.setUpdatesEnabled(False)

            for i, model in enumerate(models):
                name     = getattr(model, 'name', '') or f'model_{i}'
                version  = getattr(model, 'version', None)
                ver_str  = version.name if hasattr(version,'name') else str(version) if version else '?'
                ver_short = ver_str.replace('COL_','COL').replace('COLVersion.','')
                # Type = fourcc string, Version = game target label
                header   = getattr(model, 'header', None)
                fourcc   = getattr(header, 'fourcc', b'') if header else b''
                try:    type_str = fourcc.decode('ascii').rstrip('\x00')
                except Exception: type_str = str(fourcc)
                ver_label = {'COL1':'GTA III/VC','COL2':'SA PS2',
                             'COL3':'SA PC/Xbox','COL4':'SA (unused)'
                             }.get(ver_short, ver_short)
                spheres  = len(getattr(model, 'spheres',  []))
                boxes    = len(getattr(model, 'boxes',    []))
                vertices = len(getattr(model, 'vertices', []))
                faces    = len(getattr(model, 'faces',    []))
                bounds   = getattr(model, 'bounds', None)
                radius   = getattr(bounds, 'radius', 0.0) if bounds else 0.0

                row = self.collision_list.rowCount()
                self.collision_list.insertRow(row)

                def _item(text, col=None, idx=None):
                    it = QTableWidgetItem(str(text))
                    it.setFlags(it.flags() & ~Qt.ItemFlag.ItemIsEditable)
                    it.setTextAlignment(Qt.AlignmentFlag.AlignCenter |
                                        Qt.AlignmentFlag.AlignVCenter)
                    if idx is not None:
                        it.setData(Qt.ItemDataRole.UserRole, idx)
                    # Add icon if count > 0 and we have a pixmap for this col
                    if col in icon_map and isinstance(text, int) and text > 0:
                        it.setIcon(QIcon(icon_map[col]))
                    return it

                from PyQt6.QtGui import QIcon

                name_it = QTableWidgetItem(name)
                name_it.setFlags(name_it.flags() & ~Qt.ItemFlag.ItemIsEditable)
                name_it.setTextAlignment(Qt.AlignmentFlag.AlignLeft |
                                         Qt.AlignmentFlag.AlignVCenter)
                name_it.setData(Qt.ItemDataRole.UserRole, i)

                self.collision_list.setItem(row, 0, name_it)
                self.collision_list.setItem(row, 1, _item(type_str))
                self.collision_list.setItem(row, 2, _item(ver_label))
                self.collision_list.setItem(row, 3, _item(f"{radius:.2f}"))
                self.collision_list.setItem(row, 4, _item(spheres,  col=4))
                self.collision_list.setItem(row, 5, _item(boxes,    col=5))
                self.collision_list.setItem(row, 6, _item(vertices, col=6))
                self.collision_list.setItem(row, 7, _item(faces,    col=7))
                self.collision_list.setRowHeight(row, 22)

            # Column widths
            self.collision_list.setIconSize(QSize(14, 14))
            hdr = self.collision_list.horizontalHeader()
            hdr.resizeSection(0, 160)
            for c in range(1, 8):
                hdr.resizeSection(c, 68)
            hdr.setStretchLastSection(True)

            self.collision_list.setUpdatesEnabled(True)
            self.collision_list.viewport().update()

        except Exception as e:
            import traceback; traceback.print_exc()
            print(f"Error populating collision table: {str(e)}")

    def _create_preview_widget(self, level_data=None): #vers 3
        """Create preview widget - large collision preview like TXD Workshop"""
        if level_data is None:
            # Return preview label for collision display
            preview = QLabel()
            preview.setMinimumSize(400, 400)
            preview.setAlignment(Qt.AlignmentFlag.AlignCenter)
            preview.setStyleSheet("""
                QLabel {
                    background: #0a0a0a;
                    border: 2px solid #3a3a3a;
                    border-radius: 3px;
                    color: #888;
                }
            """)
            preview.setText("Preview Area\n\nSelect a collision model to preview")
            return preview


    def _toggle_spheres(self, checked): #vers 3
        """Toggle sphere visibility"""
        try:
            if hasattr(self, 'viewer_3d'):
                self.viewer_3d.set_view_options(show_spheres=checked)
            print(f"Spheres visibility: {checked}")
        except Exception as e:
            print(f"Error toggling spheres: {str(e)}")


    def _toggle_boxes(self, checked): #vers 3
        """Toggle box visibility"""
        try:
            if hasattr(self, 'viewer_3d'):
                self.viewer_3d.set_view_options(show_boxes=checked)
            print(f"Boxes visibility: {checked}")
        except Exception as e:
            print(f"Error toggling boxes: {str(e)}")


    def _toggle_mesh(self, checked): #vers 3
        """Toggle mesh visibility"""
        try:
            if hasattr(self, 'viewer_3d'):
                self.viewer_3d.set_view_options(show_mesh=checked)
            print(f"Mesh visibility: {checked}")
        except Exception as e:
            print(f"Error toggling mesh: {str(e)}")


# ----- Render functions









    def _setup_hotkeys(self): #vers 3
        """Setup Plasma6-style keyboard shortcuts for this application - checks for existing methods"""
        from PyQt6.QtGui import QShortcut, QKeySequence
        from PyQt6.QtCore import Qt

        # === FILE OPERATIONS ===

        # Open col (Ctrl+O)
        self.hotkey_open = QShortcut(QKeySequence.StandardKey.Open, self)
        if hasattr(self, 'open_col_file'):
            self.hotkey_open.activated.connect(self.open_col_file)
        elif hasattr(self, '_open_col_file'):
            self.hotkey_open.activated.connect(self._open_col_file)

        # Save col (Ctrl+S)
        self.hotkey_save = QShortcut(QKeySequence.StandardKey.Save, self)
        if hasattr(self, '_save_col_file'):
            self.hotkey_save.activated.connect(self._save_col_file)
        elif hasattr(self, 'save_col_file'):
            self.hotkey_save.activated.connect(self.save_col_file)

        # Force Save col (Alt+Shift+S)
        self.hotkey_force_save = QShortcut(QKeySequence("Alt+Shift+S"), self)
        if not hasattr(self, '_force_save_col'):
            # Create force save method inline if it doesn't exist
            def force_save():
                if not self.collision_list:
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "No Collision", "No Collision to save")
                    return
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Force save triggered (Alt+Shift+S)")
                # Call save regardless of modified state
                if hasattr(self, '_save_col_file'):
                    self._save_col_file()

            self.hotkey_force_save.activated.connect(force_save)
        else:
            self.hotkey_force_save.activated.connect(self._force_save_col)

        # Save As (Ctrl+Shift+S)
        self.hotkey_save_as = QShortcut(QKeySequence.StandardKey.SaveAs, self)
        if hasattr(self, '_save_as_col_file'):
            self.hotkey_save_as.activated.connect(self._save_as_col_file)
        elif hasattr(self, '_save_col_file'):
            self.hotkey_save_as.activated.connect(self._save_col_file)

        # Close (Ctrl+W)
        self.hotkey_close = QShortcut(QKeySequence.StandardKey.Close, self)
        self.hotkey_close.activated.connect(self.close)

        # === EDIT OPERATIONS ===

        # Undo (Ctrl+Z)
        self.hotkey_undo = QShortcut(QKeySequence.StandardKey.Undo, self)
        if hasattr(self, '_undo_last_action'):
            self.hotkey_undo.activated.connect(self._undo_last_action)
        # else: not implemented yet, no connection

        # Copy (Ctrl+C)
        self.hotkey_copy = QShortcut(QKeySequence.StandardKey.Copy, self)
        if hasattr(self, '_copy_collision'):
            self.hotkey_copy.activated.connect(self._copy_surface)


        # Paste (Ctrl+V)
        self.hotkey_paste = QShortcut(QKeySequence.StandardKey.Paste, self)
        if hasattr(self, '_paste_collision'):
            self.hotkey_paste.activated.connect(self._paste_surface)


        # Delete (Delete)
        self.hotkey_delete = QShortcut(QKeySequence.StandardKey.Delete, self)
        if hasattr(self, '_delete_collision'):
            self.hotkey_delete.activated.connect(self._delete_surface)


        # Duplicate (Ctrl+D)
        self.hotkey_duplicate = QShortcut(QKeySequence("Ctrl+D"), self)
        if hasattr(self, '_duplicate_collision'):
            self.hotkey_duplicate.activated.connect(self._duplicate_surface)


        # Rename (F2)
        self.hotkey_rename = QShortcut(QKeySequence("F2"), self)
        if not hasattr(self, '_rename_collsion_shortcut'):
            # Create rename shortcut method inline
            def rename_shortcut():
                # Focus the name input field if it exists
                if hasattr(self, 'info_name'):
                    self.info_name.setReadOnly(False)
                    self.info_name.selectAll()
                    self.info_name.setFocus()
            self.hotkey_rename.activated.connect(rename_shortcut)
        else:
            self.hotkey_rename.activated.connect(self._rename_shadow_shortcut)

        # === Collision OPERATIONS ===

        # Import Collision (Ctrl+I)
        self.hotkey_import = QShortcut(QKeySequence("Ctrl+I"), self)
        if hasattr(self, '_import_collision'):
            self.hotkey_import.activated.connect(self._import_surface)

        # Export Collision (Ctrl+E)
        self.hotkey_export = QShortcut(QKeySequence("Ctrl+E"), self)
        if hasattr(self, 'export_selected_collision'):
            self.hotkey_export.activated.connect(self.export_selected_surface)

        # Export All (Ctrl+Shift+E)
        self.hotkey_export_all = QShortcut(QKeySequence("Ctrl+Shift+E"), self)
        if hasattr(self, 'export_all_collision'):
            self.hotkey_export_all.activated.connect(self.export_all_surfaces)


        # === VIEW OPERATIONS ===

        # Refresh (F5)d_btn
        self.hotkey_refresh = QShortcut(QKeySequence.StandardKey.Refresh, self)
        if hasattr(self, '_reload_surface_table'):
            self.hotkey_refresh.activated.connect(self._reload_surface_table)
        elif hasattr(self, 'reload_surface_table'):
            self.hotkey_refresh.activated.connect(self.reload_surface_table)
        elif hasattr(self, 'refresh'):
            self.hotkey_refresh.activated.connect(self.refresh)

        # Properties (Alt+Enter)
        self.hotkey_properties = QShortcut(QKeySequence("Alt+Return"), self)
        if hasattr(self, '_show_detailed_info'):
            self.hotkey_properties.activated.connect(self._show_detailed_info)
        elif hasattr(self, '_show_surface_info'):
            self.hotkey_properties.activated.connect(self._show_surface_info)

        # Settings (Ctrl+,)
        self.hotkey_settings = QShortcut(QKeySequence.StandardKey.Preferences, self)
        if hasattr(self, '_show_settings_dialog'):
            self.hotkey_settings.activated.connect(self._show_settings_dialog)
        elif hasattr(self, 'show_settings_dialog'):
            self.hotkey_settings.activated.connect(self.show_settings_dialog)
        elif hasattr(self, '_show_settings_hotkeys'):
            self.hotkey_settings.activated.connect(self._show_settings_hotkeys)

        # === NAVIGATION ===

        # Select All (Ctrl+A)
        self.hotkey_select_all = QShortcut(QKeySequence.StandardKey.SelectAll, self)
        self.hotkey_select_all.activated.connect(self._select_all_models)

        # Invert Selection (Ctrl+I)
        from PyQt6.QtGui import QKeySequence as _KS
        self.hotkey_invert = QShortcut(_KS("Ctrl+I"), self)
        self.hotkey_invert.activated.connect(self._invert_selection)

        # Find (Ctrl+F)
        self.hotkey_find = QShortcut(QKeySequence.StandardKey.Find, self)
        if not hasattr(self, '_focus_search'):
            # Create focus search method inline
            def focus_search():
                if hasattr(self, 'search_input'):
                    self.search_input.setFocus()
                    self.search_input.selectAll()
            self.hotkey_find.activated.connect(focus_search)
        else:
            self.hotkey_find.activated.connect(self._focus_search)

        # === HELP ===

        # Help (F1)
        self.hotkey_help = QShortcut(QKeySequence.StandardKey.HelpContents, self)

        if hasattr(self, 'show_help'):
            self.hotkey_help.activated.connect(self.show_help)

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys initialized (Plasma6 standard)")


    def _reset_hotkeys_to_defaults(self, parent_dialog): #vers 1
        """Reset all hotkeys to Plasma6 defaults"""
        from PyQt6.QtWidgets import QMessageBox
        from PyQt6.QtGui import QKeySequence

        reply = QMessageBox.question(parent_dialog, "Reset Hotkeys",
            "Reset all keyboard shortcuts to Plasma6 defaults?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

        if reply == QMessageBox.StandardButton.Yes:
            # Reset to defaults
            self.hotkey_edit_open.setKeySequence(QKeySequence.StandardKey.Open)
            self.hotkey_edit_save.setKeySequence(QKeySequence.StandardKey.Save)
            self.hotkey_edit_force_save.setKeySequence(QKeySequence("Alt+Shift+S"))
            self.hotkey_edit_save_as.setKeySequence(QKeySequence.StandardKey.SaveAs)
            self.hotkey_edit_close.setKeySequence(QKeySequence.StandardKey.Close)
            self.hotkey_edit_undo.setKeySequence(QKeySequence.StandardKey.Undo)
            self.hotkey_edit_copy.setKeySequence(QKeySequence.StandardKey.Copy)
            self.hotkey_edit_paste.setKeySequence(QKeySequence.StandardKey.Paste)
            self.hotkey_edit_delete.setKeySequence(QKeySequence.StandardKey.Delete)
            self.hotkey_edit_duplicate.setKeySequence(QKeySequence("Ctrl+D"))
            self.hotkey_edit_rename.setKeySequence(QKeySequence("F2"))
            self.hotkey_edit_import.setKeySequence(QKeySequence("Ctrl+I"))
            self.hotkey_edit_export.setKeySequence(QKeySequence("Ctrl+E"))
            self.hotkey_edit_export_all.setKeySequence(QKeySequence("Ctrl+Shift+E"))
            self.hotkey_edit_refresh.setKeySequence(QKeySequence.StandardKey.Refresh)
            self.hotkey_edit_properties.setKeySequence(QKeySequence("Alt+Return"))
            self.hotkey_edit_find.setKeySequence(QKeySequence.StandardKey.Find)
            self.hotkey_edit_help.setKeySequence(QKeySequence.StandardKey.HelpContents)


    def _apply_hotkey_settings(self, dialog, close=False): #vers 1
        """Apply hotkey changes"""
        # Update all hotkeys with new sequences
        self.hotkey_open.setKey(self.hotkey_edit_open.keySequence())
        self.hotkey_save.setKey(self.hotkey_edit_save.keySequence())
        self.hotkey_force_save.setKey(self.hotkey_edit_force_save.keySequence())
        self.hotkey_save_as.setKey(self.hotkey_edit_save_as.keySequence())
        self.hotkey_close.setKey(self.hotkey_edit_close.keySequence())
        self.hotkey_undo.setKey(self.hotkey_edit_undo.keySequence())
        self.hotkey_copy.setKey(self.hotkey_edit_copy.keySequence())
        self.hotkey_paste.setKey(self.hotkey_edit_paste.keySequence())
        self.hotkey_delete.setKey(self.hotkey_edit_delete.keySequence())
        self.hotkey_duplicate.setKey(self.hotkey_edit_duplicate.keySequence())
        self.hotkey_rename.setKey(self.hotkey_edit_rename.keySequence())
        self.hotkey_import.setKey(self.hotkey_edit_import.keySequence())
        self.hotkey_export.setKey(self.hotkey_edit_export.keySequence())
        self.hotkey_export_all.setKey(self.hotkey_edit_export_all.keySequence())
        self.hotkey_refresh.setKey(self.hotkey_edit_refresh.keySequence())
        self.hotkey_properties.setKey(self.hotkey_edit_properties.keySequence())
        self.hotkey_find.setKey(self.hotkey_edit_find.keySequence())
        self.hotkey_help.setKey(self.hotkey_edit_help.keySequence())

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys updated")

        # TODO: Save to config file for persistence

        if close:
            dialog.accept()


    def _show_settings_hotkeys(self): #vers 1
        """Show settings dialog with hotkey customization"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QLineEdit, QPushButton,
                                    QGroupBox, QFormLayout, QKeySequenceEdit)
        from PyQt6.QtCore import Qt

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(600)
        dialog.setMinimumHeight(500)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # === HOTKEYS TAB ===
        hotkeys_tab = QWidget()
        hotkeys_layout = QVBoxLayout(hotkeys_tab)

        # File Operations Group
        file_group = QGroupBox("File Operations")
        file_form = QFormLayout()

        self.hotkey_edit_open = QKeySequenceEdit(self.hotkey_open.key())
        file_form.addRow("Open col:", self.hotkey_edit_open)

        self.hotkey_edit_save = QKeySequenceEdit(self.hotkey_save.key())
        file_form.addRow("Save col:", self.hotkey_edit_save)

        self.hotkey_edit_force_save = QKeySequenceEdit(self.hotkey_force_save.key())
        force_save_layout = QHBoxLayout()
        force_save_layout.addWidget(self.hotkey_edit_force_save)
        force_save_hint = QLabel("(Force save even if unmodified)")
        force_save_hint.setStyleSheet("color: #888; font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        self.hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key())
        file_form.addRow("Save As:", self.hotkey_edit_save_as)

        self.hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key())
        file_form.addRow("Close:", self.hotkey_edit_close)

        file_group.setLayout(file_form)
        hotkeys_layout.addWidget(file_group)

        # Edit Operations Group
        edit_group = QGroupBox("Edit Operations")
        edit_form = QFormLayout()

        self.hotkey_edit_undo = QKeySequenceEdit(self.hotkey_undo.key())
        edit_form.addRow("Undo:", self.hotkey_edit_undo)

        self.hotkey_edit_copy = QKeySequenceEdit(self.hotkey_copy.key())
        edit_form.addRow("Copy Collision:", self.hotkey_edit_copy)

        self.hotkey_edit_paste = QKeySequenceEdit(self.hotkey_paste.key())
        edit_form.addRow("Paste Collision:", self.hotkey_edit_paste)

        self.hotkey_edit_delete = QKeySequenceEdit(self.hotkey_delete.key())
        edit_form.addRow("Delete:", self.hotkey_edit_delete)

        self.hotkey_edit_duplicate = QKeySequenceEdit(self.hotkey_duplicate.key())
        edit_form.addRow("Duplicate:", self.hotkey_edit_duplicate)

        self.hotkey_edit_rename = QKeySequenceEdit(self.hotkey_rename.key())
        edit_form.addRow("Rename:", self.hotkey_edit_rename)

        edit_group.setLayout(edit_form)
        hotkeys_layout.addWidget(edit_group)

        # Collision Group
        coll_group = QGroupBox("Collision Operations")
        coll_form = QFormLayout()

        self.hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key())
        coll_form.addRow("Import Collision:", self.hotkey_edit_import)

        self.hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key())
        coll_form.addRow("Export Collision:", self.hotkey_edit_export)

        self.hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key())
        coll_form.addRow("Export All:", self.hotkey_edit_export_all)

        coll_group.setLayout(coll_form)
        hotkeys_layout.addWidget(coll_group)

        # View Operations Group
        view_group = QGroupBox("View Operations")
        view_form = QFormLayout()

        self.hotkey_edit_refresh = QKeySequenceEdit(self.hotkey_refresh.key())
        view_form.addRow("Refresh:", self.hotkey_edit_refresh)

        self.hotkey_edit_properties = QKeySequenceEdit(self.hotkey_properties.key())
        view_form.addRow("Properties:", self.hotkey_edit_properties)

        self.hotkey_edit_find = QKeySequenceEdit(self.hotkey_find.key())
        view_form.addRow("Find/Search:", self.hotkey_edit_find)

        self.hotkey_edit_help = QKeySequenceEdit(self.hotkey_help.key())
        view_form.addRow("Help:", self.hotkey_edit_help)

        view_group.setLayout(view_form)
        hotkeys_layout.addWidget(view_group)

        hotkeys_layout.addStretch()

        # Reset to defaults button
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")
        reset_hotkeys_btn.clicked.connect(lambda: self._reset_hotkeys_to_defaults(dialog))
        hotkeys_layout.addWidget(reset_hotkeys_btn)

        tabs.addTab(hotkeys_tab, "Keyboard Shortcuts")

        # === GENERAL TAB (for future settings) ===
        general_tab = QWidget()
        general_layout = QVBoxLayout(general_tab)

        placeholder_label = QLabel("Additional settings will appear here in future versions.")
        placeholder_label.setStyleSheet("color: #888; font-style: italic; padding: 20px;")
        placeholder_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        general_layout.addWidget(placeholder_label)
        general_layout.addStretch()

        tabs.addTab(general_tab, "General")

        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(lambda: self._apply_hotkey_settings(dialog))
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: self._apply_hotkey_settings(dialog, close=True))
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        dialog.exec()

    def _show_col_info(self): #vers 4
        """Show TXD Workshop information dialog - About and capabilities"""
        dialog = QDialog(self)
        dialog.setWindowTitle("About COL Workshop")
        dialog.setMinimumWidth(600)
        dialog.setMinimumHeight(500)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)

        # Header
        header = QLabel(f"COL Workshop - {App_name}")
        header.setFont(QFont("Arial", 14, QFont.Weight.Bold))
        header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(header)

        # Author info
        author_label = QLabel("Author: X-Seti")
        author_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(author_label)

        # Version info
        version_label = QLabel("Version: 1.5 - October 2025")
        version_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(version_label)

        layout.addWidget(QLabel(""))  # Spacer

        # Capabilities section
        capabilities = QTextEdit()
        capabilities.setReadOnly(True)
        capabilities.setMaximumHeight(350)

        info_text = """<b>COL Workshop Capabilities:</b><br><br>

<b>✓ File Operations:</b><br>


<b>✓ Collision Viewing & Editing:</b><br>


<b>✓ Collision Management:</b><br>


<b>✓ Collision Surface Painting:</b><br>


<b>✓ Format Support:</b><br>


<b>✓ Advanced Features:</b><br>"""

        # Add format support dynamically
        formats_available = []

        # Standard formats (always via PIL)

        info_text += "<br>".join(formats_available)
        info_text += "<br><br>"

        # Settings info
        info_text += """<b>✓ Customization:</b><br>
- Adjustable texture name length (8-64 chars)<br>
- Button display modes (Icons/Text/Both)<br>
- Font customization<br>
- Preview zoom and pan offsets<br><br>

<b>Keyboard Shortcuts:</b><br>
- Ctrl+O: Open COL<br>
- Ctrl+S: Save COL<br>
- Ctrl+I: Import Collision col, cst, 3ds<br>
- Ctrl+E: Export Selected col, cst, 3ds<br>
- Ctrl+Z: Undo<br>
- Delete: Remove Collision<br>
- Ctrl+D: Duplicate Collision<br>"""

        capabilities.setHtml(info_text)
        layout.addWidget(capabilities)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        close_btn.setDefault(True)
        layout.addWidget(close_btn)

        dialog.exec()


#class SvgIcons: #vers 1 - Once functions are updated this class will be moved to the bottom
    """SVG icon data to QIcon with theme color support"""

#moved to scg_icon_factory

class ZoomablePreview(QLabel): #vers 2
    """Fixed preview widget with zoom and pan"""

    def __init__(self, parent=None):
        self.icon_factory = SVGIconFactory()
        super().__init__(parent)
        self.main_window = parent
        self.setMinimumSize(400, 400)
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        #self.setStyleSheet("border: 2px solid #3a3a3a; background: #0a0a0a;")
        self.setStyleSheet("border: 1px solid #3a3a3a;")
        self.setMouseTracking(True)

        # Display state
        self.current_model = None
        self.original_pixmap = None
        self.scaled_pixmap = None

        # View controls
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        self.rotation_x = 45  # X-axis rotation (up/down tilt)
        self.rotation_y = 0   # Y-axis rotation (left/right spin)
        self.rotation_z = 0   # Z-axis rotation (roll)

        # View toggles
        self.show_spheres = True
        self.show_boxes = True
        self.show_mesh = True

        # Mouse interaction
        self.dragging = False
        self.drag_start = QPoint(0, 0)
        self.drag_mode = None  # 'pan' or 'rotate'

        # Background
        self.bg_color = QColor(42, 42, 42)

        self.placeholder_text = "Select a collision model to preview"

        self.background_mode = 'solid'
        self._checkerboard_size = 16


    def setPixmap(self, pixmap): #vers 2
        """Set pixmap and update display"""
        if pixmap and not pixmap.isNull():
            self.original_pixmap = pixmap
            self.placeholder_text = None
            self._update_scaled_pixmap()
        else:
            self.original_pixmap = None
            self.scaled_pixmap = None
            self.placeholder_text = "No texture loaded"

        self.update()  # Trigger repaint


    def set_model(self, model): #vers 1
        """Set collision model to display"""
        self.current_model = model
        self.render_collision()


    def render_collision(self): #vers 2
        """Render the collision model with current view settings"""
        if not self.current_model:
            self.setText(self.placeholder_text)
            self.original_pixmap = None
            self.scaled_pixmap = None
            return

        width = max(400, self.width())
        height = max(400, self.height())

        # Use the parent's render method
        if hasattr(self.parent(), '_render_collision_preview'):
            self.original_pixmap = self.parent()._render_collision_preview(
                self.current_model,
                width,
                height
            )
        else:
            # Fallback - just show text for now
            name = getattr(self.current_model, 'name', 'Unknown')
            self.setText(f"Collision Model: {name}\n\nRendering...")
            return

        self._update_scaled_pixmap()
        self.update()


    def _update_scaled_pixmap(self): #vers
        """Update scaled pixmap based on zoom"""
        if not self.original_pixmap:
            self.scaled_pixmap = None
            return

        scaled_width = int(self.original_pixmap.width() * self.zoom_level)
        scaled_height = int(self.original_pixmap.height() * self.zoom_level)

        self.scaled_pixmap = self.original_pixmap.scaled(
            scaled_width, scaled_height,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation
        )


    def paintEvent(self, event): #vers 2
        """Paint the preview with background and image"""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)

        # Draw background
        if self.background_mode == 'checkerboard':
            self._draw_checkerboard(painter)
        else:
            painter.fillRect(self.rect(), self.bg_color)

        # Draw image if available
        if self.scaled_pixmap and not self.scaled_pixmap.isNull():
            # Calculate centered position with pan offset
            x = (self.width() - self.scaled_pixmap.width()) // 2 + self.pan_offset.x()
            y = (self.height() - self.scaled_pixmap.height()) // 2 + self.pan_offset.y()
            painter.drawPixmap(x, y, self.scaled_pixmap)
        elif self.placeholder_text:
            # Draw placeholder text
            painter.setPen(QColor(150, 150, 150))
            painter.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, self.placeholder_text)


    def set_checkerboard_background(self): #vers 1
        """Enable checkerboard background"""
        self.background_mode = 'checkerboard'
        self.update()


    def set_background_color(self, color): #vers 1
        """Set solid background color"""
        self.background_mode = 'solid'
        self.bg_color = color
        self.update()


    def _draw_checkerboard(self, painter): #vers 1
        """Draw checkerboard background pattern"""
        size = self._checkerboard_size
        color1 = QColor(200, 200, 200)
        color2 = QColor(150, 150, 150)

        for y in range(0, self.height(), size):
            for x in range(0, self.width(), size):
                color = color1 if ((x // size) + (y // size)) % 2 == 0 else color2
                painter.fillRect(x, y, size, size, color)


    # Zoom controls
    def zoom_in(self): #vers 1
        """Zoom in"""
        self.zoom_level = min(5.0, self.zoom_level * 1.2)
        self._update_scaled_pixmap()
        self.update()


    def zoom_out(self): #vers 1
        """Zoom out"""
        self.zoom_level = max(0.1, self.zoom_level / 1.2)
        self._update_scaled_pixmap()
        self.update()


    def reset_view(self): #vers 1
        """Reset to default view"""
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        self.rotation_x = 45
        self.rotation_y = 0
        self.rotation_z = 0
        self.render_collision()


    def fit_to_window(self): #vers 2
        """Fit image to window size"""
        if not self.original_pixmap:
            return

        img_size = self.original_pixmap.size()
        widget_size = self.size()

        zoom_w = widget_size.width() / img_size.width()
        zoom_h = widget_size.height() / img_size.height()

        self.zoom_level = min(zoom_w, zoom_h) * 0.95
        self.pan_offset = QPoint(0, 0)
        self._update_scaled_pixmap()
        self.update()


    def pan(self, dx, dy): #vers 1
        """Pan the view by dx, dy pixels"""
        self.pan_offset += QPoint(dx, dy)
        self.update()


    # Rotation controls
    def rotate_x(self, degrees): #vers 1
        """Rotate around X axis"""
        self.rotation_x = (self.rotation_x + degrees) % 360
        self.render_collision()


    def rotate_y(self, degrees): #vers 1
        """Rotate around Y axis"""
        self.rotation_y = (self.rotation_y + degrees) % 360
        self.render_collision()


    def rotate_z(self, degrees): #vers 1
        """Rotate around Z axis"""
        self.rotation_z = (self.rotation_z + degrees) % 360
        self.render_collision()


    # Mouse events
    def mousePressEvent(self, event): #vers 1
        """Handle mouse press"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = True
            self.drag_start = event.pos()
            self.drag_mode = 'rotate' if event.modifiers() & Qt.KeyboardModifier.ControlModifier else 'pan'


    def mouseMoveEvent(self, event): #vers 1
        """Handle mouse drag"""
        if self.dragging:
            delta = event.pos() - self.drag_start

            if self.drag_mode == 'rotate':
                # Rotate based on drag
                self.rotate_y(delta.x() * 0.5)
                self.rotate_x(-delta.y() * 0.5)
            else:
                # Pan
                self.pan_offset += delta
                self.update()

            self.drag_start = event.pos()


    def mouseReleaseEvent(self, event): #vers 1
        """Handle mouse release"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = False
            self.drag_mode = None


    def wheelEvent(self, event): #vers 1
        """Handle mouse wheel for zoom"""
        delta = event.angleDelta().y()
        if delta > 0:
            self.zoom_in()
        else:
            self.zoom_out()


class COLEditorDialog(QDialog): #vers 3
    """Enhanced COL Editor Dialog"""


    def __init__(self, parent=None):
        self.icon_factory = SVGIconFactory()
        super().__init__(parent)
        self.setWindowTitle(App_name)
        self.setModal(False)  # Allow non-modal operation
        self.resize(1000, 700)

        self.current_file = None
        self.current_model = None
        self.file_path = None
        self.is_modified = False

        self.setup_ui()
        self.connect_signals()

        print(App_name + " dialog created")


    def setup_ui(self): #vers 1
        """Setup editor UI"""
        layout = QVBoxLayout(self)

        # Toolbar
        self.toolbar = COLToolbar(self)
        layout.addWidget(self.toolbar)

        # Main splitter
        main_splitter = QSplitter(Qt.Orientation.Horizontal)
        layout.addWidget(main_splitter)

        # Left panel - Model list and properties
        left_panel = QSplitter(Qt.Orientation.Vertical)
        left_panel.setFixedWidth(350)

        # Model list
        models_group = QGroupBox("Models")
        models_layout = QVBoxLayout(models_group)

        self.model_list = COLModelListWidget()
        models_layout.addWidget(self.model_list)

        left_panel.addWidget(models_group)

        # Properties
        properties_group = QGroupBox("Properties")
        properties_layout = QVBoxLayout(properties_group)

        self.properties_widget = COLPropertiesWidget()
        properties_layout.addWidget(self.properties_widget)

        left_panel.addWidget(properties_group)

        # Set left panel sizes
        left_panel.setSizes([200, 400])

        main_splitter.addWidget(left_panel)

        # Right panel - 3D viewer
        viewer_group = QGroupBox("3D Viewer")
        viewer_layout = QVBoxLayout(viewer_group)

        if VIEWPORT_AVAILABLE:
            self.viewer_3d = COL3DViewport()
            viewer_layout.addWidget(self.viewer_3d)

            # Add 3DS Max style controls at bottom
            controls = self._create_viewport_controls()
            viewer_layout.addWidget(controls)
        else:
            self.viewer_3d = QLabel("3D Viewport unavailable\nInstall: pip install PyOpenGL")
            self.viewer_3d.setAlignment(Qt.AlignmentFlag.AlignCenter)
            viewer_layout.addWidget(self.viewer_3d)

        # Set main splitter sizes
        main_splitter.setSizes([350, 650])

        # Status bar
        self.status_bar = QStatusBar()
        self.status_bar.showMessage("Ready")
        layout.addWidget(self.status_bar)

        # Progress bar (hidden by default)
        self.progress_bar = QProgressBar()
        self.progress_bar.setVisible(False)
        layout.addWidget(self.progress_bar)


    def connect_signals(self): #vers 1
        """Connect UI signals"""
        # Toolbar actions
        self.toolbar.open_action.triggered.connect(self.open_file)
        self.toolbar.save_action.triggered.connect(self.save_file)
        self.toolbar.analyze_action.triggered.connect(self.analyze_file)

        # View options
        self.toolbar.view_spheres_action.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_spheres=checked)
        )
        self.toolbar.view_boxes_action.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_boxes=checked)
        )
        self.toolbar.view_mesh_action.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_mesh=checked)
        )

        # Model selection
        self.model_list.model_selected.connect(self.on_model_selected)
        self.viewer_3d.model_selected.connect(self.on_model_selected)

        # Properties changes
        self.properties_widget.property_changed.connect(self.on_property_changed)


    def load_col_file(self, file_path: str) -> bool: #vers 2
        """Load COL file - ENHANCED VERSION"""
        try:
            self.file_path = file_path
            self.status_bar.showMessage("Loading COL file...")
            self.progress_bar.setVisible(True)

            # Load the file
            self.current_file = COLFile(file_path)

            if not self.current_file.load():
                error_msg = getattr(self.current_file, 'load_error', 'Unknown error')
                QMessageBox.critical(self, "Load Error", f"Failed to load COL file:\n{error_msg}")
                self.progress_bar.setVisible(False)
                self.status_bar.showMessage("Ready")
                return False

            # Update UI
            self.model_list.set_col_file(self.current_file)
            self.viewer_3d.set_current_file(self.current_file)

            # Select first model if available
            if hasattr(self.current_file, 'models') and self.current_file.models:
                self.model_list.setCurrentRow(0)

            model_count = len(getattr(self.current_file, 'models', []))
            self.status_bar.showMessage(f"Loaded: {os.path.basename(file_path)} ({model_count} models)")
            self.progress_bar.setVisible(False)

            self.setWindowTitle(f"COL Editor - {os.path.basename(file_path)}")
            self.is_modified = False

            print(f"COL file loaded: {file_path}")
            return True

        except Exception as e:
            self.progress_bar.setVisible(False)
            self.status_bar.showMessage("Ready")
            error_msg = f"Error loading COL file: {str(e)}"
            QMessageBox.critical(self, "Error", error_msg)
            print(error_msg)
            return False


    def open_file(self): #vers 1
        """Open file dialog"""
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Open COL File", "", "COL Files (*.col);;All Files (*)"
        )

        if file_path:
            self.load_col_file(file_path)


    def save_file(self): #vers 1
        """Save current file"""
        if not self.current_file:
            QMessageBox.warning(self, "Save", "No file loaded to save")
            return

        if not self.file_path:
            self.save_file_as()
            return

        try:
            self.status_bar.showMessage("Saving COL file...")

            # TODO: Implement actual saving
            # For now, just show a message
            QMessageBox.information(self, "Save",
                "COL file saving will be implemented in a future version.\n"
                "Currently the editor is in view-only mode.")

            self.status_bar.showMessage("Ready")

        except Exception as e:
            error_msg = f"Error saving COL file: {str(e)}"
            QMessageBox.critical(self, "Save Error", error_msg)
            print(error_msg)


    def save_file_as(self): #vers 1
        """Save file as dialog"""
        file_path, _ = QFileDialog.getSaveFileName(
            self, "Save COL File", "", "COL Files (*.col);;All Files (*)"
        )

        if file_path:
            self.file_path = file_path
            self.save_file()


    def analyze_file(self): #vers 1
        """Analyze current COL file"""
        if not self.current_file or not self.file_path:
            QMessageBox.warning(self, "Analyze", "No file loaded to analyze")
            return

        try:
            self.status_bar.showMessage("Analyzing COL file...")

            # Import locally when needed
            from apps.methods.col_operations import get_col_detailed_analysis
            from gui.col_dialogs import show_col_analysis_dialog

            self.status_bar.showMessage("Analyzing COL file...")

            # Get detailed analysis
            analysis_data = get_col_detailed_analysis(self.file_path)

            if 'error' in analysis_data:
                QMessageBox.warning(self, "Analysis Error", f"Analysis failed: {analysis_data['error']}")
                return

            # Show analysis dialog
            show_col_analysis_dialog(self, analysis_data, os.path.basename(self.file_path))

            self.status_bar.showMessage("Ready")

        except Exception as e:
            error_msg = f"Error analyzing COL file: {str(e)}"
            QMessageBox.critical(self, "Analysis Error", error_msg)
            print(error_msg)


    def on_model_selected(self, model_index: int): #vers 1
        """Handle model selection"""
        try:
            if not self.current_file or not hasattr(self.current_file, 'models'):
                return

            if model_index < 0 or model_index >= len(self.current_file.models):
                return

            # Update current model
            self.current_model = self.current_file.models[model_index]

            # Update viewer
            self.viewer_3d.set_current_model(self.current_model, model_index)

            # Update properties
            self.properties_widget.set_current_model(self.current_model)

            # Update list selection if needed
            if self.model_list.currentRow() != model_index:
                self.model_list.setCurrentRow(model_index)

            model_name = getattr(self.current_model, 'name', f'Model_{model_index}')
            self.status_bar.showMessage(f"Selected: {model_name}")

            print(f"Model selected: {model_name} (index {model_index})")

        except Exception as e:
            print(f"Error selecting model: {str(e)}")


    def _create_viewport_controls(self): #vers 1
        icon_color = self._get_icon_color()
        """Create 3D viewport controls - 3DS Max style toolbar at bottom"""
        if not VIEWPORT_AVAILABLE:
            return QWidget()

        controls_widget = QFrame()
        controls_widget.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        controls_widget.setStyleSheet("""
            QFrame {
                background-color: #3a3a3a;
                border: 1px solid #555555;
                border-radius: 3px;
                padding: 3px;
            }
            QPushButton {
                background-color: #4a4a4a;
                border: 1px solid #666666;
                border-radius: 2px;
                padding: 4px;
                min-width: 28px;
                min-height: 28px;
            }
            QPushButton:hover {
                background-color: #5a5a5a;
                border: 1px solid #888888;
            }
            QPushButton:pressed {
                background-color: #2a2a2a;
            }
            QPushButton:checked {
                background-color: #006699;
                border: 1px solid #0088cc;
            }
        """)

        layout = QHBoxLayout(controls_widget)
        layout.setContentsMargins(4, 2, 4, 2)
        layout.setSpacing(2)

        # View mode buttons
        btn_spheres = QPushButton()
        btn_spheres.setIcon(self.icon_factory.sphere_icon(color=icon_color))
        btn_spheres.setCheckable(True)
        btn_spheres.setChecked(True)
        btn_spheres.setToolTip("Toggle Spheres")
        btn_spheres.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_spheres=checked)
        )

        btn_boxes = QPushButton()
        btn_boxes.setIcon(self.icon_factory.box_icon(color=icon_color))
        btn_boxes.setCheckable(True)
        btn_boxes.setChecked(True)
        btn_boxes.setToolTip("Toggle Boxes")
        btn_boxes.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_boxes=checked)
        )

        btn_mesh = QPushButton()
        btn_mesh.setIcon(self.icon_factory.mesh_icon(color=icon_color))
        btn_mesh.setCheckable(True)
        btn_mesh.setChecked(True)
        btn_mesh.setToolTip("Toggle Mesh")
        btn_mesh.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_mesh=checked)
        )

        btn_wireframe = QPushButton()
        btn_wireframe.setIcon(self.icon_factory.wireframe_icon(color=icon_color))
        btn_wireframe.setCheckable(True)
        btn_wireframe.setChecked(True)
        btn_wireframe.setToolTip("Toggle Wireframe")
        btn_wireframe.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_wireframe=checked)
        )

        btn_bounds = QPushButton()
        btn_bounds.setIcon(self.icon_factory.bounds_icon(color=icon_color))
        btn_bounds.setCheckable(True)
        btn_bounds.setChecked(True)
        btn_bounds.setToolTip("Toggle Bounding Box")
        btn_bounds.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_bounds=checked)
        )

        # Separator
        separator1 = QFrame()
        separator1.setFrameShape(QFrame.Shape.VLine)
        separator1.setFrameShadow(QFrame.Shadow.Sunken)
        separator1.setStyleSheet("color: #666666;")

        # Camera controls
        btn_reset = QPushButton()
        btn_reset.setIcon(self.icon_factory.reset_view_icon(color=icon_color))
        btn_reset.setToolTip("Reset View")
        btn_reset.clicked.connect(self.viewer_3d.reset_view)

        btn_top = QPushButton("T")
        btn_top.setToolTip("Top View")
        btn_top.clicked.connect(lambda: self._set_camera_view('top'))

        btn_front = QPushButton("F")
        btn_front.setToolTip("Front View")
        btn_front.clicked.connect(lambda: self._set_camera_view('front'))

        btn_side = QPushButton("S")
        btn_side.setToolTip("Side View")
        btn_side.clicked.connect(lambda: self._set_camera_view('side'))

        # Add widgets to layout
        layout.addWidget(btn_spheres)
        layout.addWidget(btn_boxes)
        layout.addWidget(btn_mesh)
        layout.addWidget(btn_wireframe)
        layout.addWidget(btn_bounds)
        layout.addWidget(separator1)
        layout.addWidget(btn_reset)
        layout.addWidget(btn_top)
        layout.addWidget(btn_front)
        layout.addWidget(btn_side)
        layout.addStretch()

        return controls_widget


    def _set_camera_view(self, view_type): #vers 1
        """Set predefined camera view"""
        if not VIEWPORT_AVAILABLE or not hasattr(self, 'viewer_3d'):
            return

        if view_type == 'top':
            self.viewer_3d.rotation_x = 0.0
            self.viewer_3d.rotation_y = 0.0
        elif view_type == 'front':
            self.viewer_3d.rotation_x = 90.0
            self.viewer_3d.rotation_y = 0.0
        elif view_type == 'side':
            self.viewer_3d.rotation_x = 90.0
            self.viewer_3d.rotation_y = 90.0

        self.viewer_3d.update()


    def _svg_to_icon(self, svg_data, size=24): #vers 1
        """Convert SVG to QIcon"""
        from PyQt6.QtGui import QIcon, QPixmap, QPainter, QColor
        from PyQt6.QtSvg import QSvgRenderer
        from PyQt6.QtCore import QByteArray

        try:
            text_color = self.palette().color(self.foregroundRole())
            svg_str = svg_data.decode('utf-8')
            svg_str = svg_str.replace('currentColor', text_color.name())
            svg_data = svg_str.encode('utf-8')

            renderer = QSvgRenderer(QByteArray(svg_data))
            if not renderer.isValid():
                print(f"Invalid SVG data in col_workshop")
                return QIcon()

            pixmap = QPixmap(size, size)
            pixmap.fill(QColor(0, 0, 0, 0))

            painter = QPainter(pixmap)
            renderer.render(painter)
            painter.end()

            return QIcon(pixmap)
        except:
            return QIcon()


    def on_property_changed(self, property_name: str, new_value): #vers 2
        """Handle property changes from properties widget"""
        try:
            if not self.current_file or not hasattr(self.current_file, 'models'):
                return

            selected_index = self.model_list.currentRow()
            if selected_index < 0 or selected_index >= len(self.current_file.models):
                return

            current_model = self.current_file.models[selected_index]

            # Update model properties
            if property_name == 'name':
                current_model.name = str(new_value)
                self.model_list.item(selected_index).setText(new_value)
            elif property_name == 'version':
                current_model.version = new_value
            elif property_name == 'material':
                if hasattr(current_model, 'material'):
                    current_model.material = new_value

            # Mark as modified
            self.is_modified = True
            self.status_bar.showMessage(f"Modified: {property_name} changed")

            # Update viewer if needed
            if hasattr(self, 'viewer_3d') and VIEWPORT_AVAILABLE:
                self.viewer_3d.set_current_model(current_model, selected_index)

            print(f"Property changed: {property_name} = {new_value}")

        except Exception as e:
            print(f"Error handling property change: {str(e)}")
            self.status_bar.showMessage(f"Error: {str(e)}")


        def _set_camera_view(self, view_type): #vers 1
            """Set predefined camera view"""
            if view_type == 'top':
                self.viewer_3d.rotation_x = 0.0
                self.viewer_3d.rotation_y = 0.0
            elif view_type == 'front':
                self.viewer_3d.rotation_x = 90.0
                self.viewer_3d.rotation_y = 0.0
            elif view_type == 'side':
                self.viewer_3d.rotation_x = 90.0
                self.viewer_3d.rotation_y = 90.0

            self.viewer_3d.update()



    def closeEvent(self, event): #vers 1
        """Handle close event"""
        if self.is_modified:
            reply = QMessageBox.question(
                self, "Unsaved Changes",
                "The file has unsaved changes. Do you want to save before closing?",
                QMessageBox.StandardButton.Save |
                QMessageBox.StandardButton.Discard |
                QMessageBox.StandardButton.Cancel
            )

            if reply == QMessageBox.StandardButton.Save:
                self.save_file()
                event.accept()
            elif reply == QMessageBox.StandardButton.Discard:
                event.accept()
            else:
                event.ignore()
        else:
            event.accept()

        print("COL Editor dialog closed")


    # Add import/export functionality when docked
    def _add_import_export_functionality(self): #vers 1
        """Add import/export functionality when docked to img factory"""
        try:
            # Only add these when docked to img factory
            if self.main_window and hasattr(self.main_window, 'log_message'):
                # Add import button to toolbar if not already present
                if not hasattr(self, 'import_btn'):
                    # Import button would be added to the toolbar in _create_toolbar
                    pass
                    
                # Add export button to toolbar if not already present
                if not hasattr(self, 'export_btn'):
                    # Export button would be added to the toolbar in _create_toolbar
                    pass
                    
                self.main_window.log_message(f"{App_name} import/export functionality ready")
                
        except Exception as e:
            print(f"Error adding import/export functionality: {str(e)}")


    def _import_col_data(self): #vers 2
        """Import one or more COL models from .col file(s) into the current archive."""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        if not self.current_col_file:
            # No file loaded yet — open the files directly
            self._open_file()
            return

        paths, _ = QFileDialog.getOpenFileNames(
            self, "Import COL File(s)", "",
            "COL Files (*.col);;All Files (*)")
        if not paths:
            return

        from apps.methods.col_workshop_loader import COLFile
        added = 0
        for path in paths:
            cf = COLFile()
            if cf.load_from_file(path):
                for model in cf.models:
                    self.current_col_file.models.append(model)
                    added += 1
            else:
                print(f"Import failed: {path}")

        if added:
            self._populate_collision_list()
            self._populate_compact_col_list()
            # Select last added
            last = len(self.current_col_file.models) - 1
            active = (self.col_compact_list
                      if getattr(self,'_col_view_mode','list')=='detail'
                      else self.collision_list)
            if active.rowCount() > last:
                active.selectRow(last)
            msg = f"Imported {added} model(s) from {len(paths)} file(s)."
            self._set_status(msg)
            if self.main_window and hasattr(self.main_window,'log_message'):
                self.main_window.log_message(msg)
        else:
            QMessageBox.warning(self, "Import", "No models could be imported.")


    # _export_col_data implemented above (line ~5136) — this stub removed


# Convenience functions
def open_col_editor(parent=None, file_path: str = None) -> COLEditorDialog: #vers 2
    """Open COL editor dialog - ENHANCED VERSION"""
    try:
        editor = COLEditorDialog(parent)

        if file_path:
            if editor.load_col_file(file_path):
                print(f"COL editor opened with file: {file_path}")
            else:
                print(f"Failed to load file in COL editor: {file_path}")

        editor.show()
        return editor

    except Exception as e:
        print(f"Error opening COL editor: {str(e)}")
        if parent:
            QMessageBox.critical(parent, "COL Editor Error", f"Failed to open COL editor:\n{str(e)}")
        return None


def create_new_model(model_name: str = "New Model") -> COLModel: #vers 1

    try:
        model = COLModel()
        model.name = model_name
        model.version = COLVersion.COL_2  # Default to COL2
        model.spheres = []
        model.boxes = []
        model.vertices = []
        model.faces = []

        # Initialize bounding box
        if hasattr(model, 'calculate_bounding_box'):
            model.calculate_bounding_box()

        print(f"Created new COL model: {model_name}")
        return model

    except Exception as e:
        print(f"Error creating new COL model: {str(e)}")
        return None


def delete_model(col_file: COLFile, model_index: int) -> bool: #vers 1
    """Delete model from COL file"""
    try:
        if not hasattr(col_file, 'models') or not col_file.models:
            return False

        if model_index < 0 or model_index >= len(col_file.models):
            return False

        model_name = getattr(col_file.models[model_index], 'name', f'Model_{model_index}')
        del col_file.models[model_index]

        print(f"Deleted COL model: {model_name}")
        return True

    except Exception as e:
        print(f"Error deleting COL model: {str(e)}")
        return False


def export_model(model: COLModel, file_path: str) -> bool: #vers 1
    """Export single model to file"""
    try:
        # TODO: Implement model export
        print(f"Model export to {file_path} - not yet implemented")
        return False

    except Exception as e:
        print(f"Error exporting model: {str(e)}")
        return False


def import_elements(model: COLModel, file_path: str) -> bool: #vers 1
    """Import collision elements from file"""
    try:
        # TODO: Implement element import
        print(f"Element import from {file_path} - not yet implemented")
        return False

    except Exception as e:
        print(f"Error importing elements: {str(e)}")
        return False


def refresh_model_list(list_widget: COLModelListWidget, col_file: COLFile): #vers 1
    """Refresh model list widget"""
    try:
        list_widget.set_col_file(col_file)
        print("Model list refreshed")

    except Exception as e:
        print(f"Error refreshing model list: {str(e)}")


def update_view_options(viewer: 'COL3DViewport', **options): #vers 1
    """Update 3D viewer options"""
    try:
        viewer.set_view_options(**options)
        print(f"View options updated: {options}")

        def _ensure_standalone_functionality(self): #vers 1
            """Ensure popped-out windows work independently of img factory"""
            try:
                # When popped out, ensure all necessary functionality is available
                if not self.is_docked or getattr(self, 'is_overlay', False) == False:
                    # Enable all UI elements that might be disabled when docked
                    if hasattr(self, 'toolbar') and self.toolbar:
                        self.toolbar.setEnabled(True)

                    # Ensure all buttons and controls work independently
                    if hasattr(self, 'main_window') and self.main_window is None:
                        # This is truly standalone, enable all features
                        if hasattr(self, 'dock_btn'):
                            self.dock_btn.setText("X")  # Change to close button when standalone
                            self.dock_btn.setToolTip("Close window")

                    # Set proper window flags for standalone operation
                    if not getattr(self, 'is_overlay', False):
                        self.setWindowFlags(Qt.WindowType.Window)

            except Exception as e:
                print(f"Error ensuring standalone functionality: {str(e)}")

    except Exception as e:
        print(f"Error updating view options: {str(e)}")



def apply_changes(editor: COLEditorDialog) -> bool: #vers 1
    """Apply all pending changes"""
    try:
        # TODO: Implement change application
        print("Apply changes - not yet implemented")
        return True

    except Exception as e:
        print(f"Error applying changes: {str(e)}")
        return False


# --- External AI upscaler integration helper ---
import subprocess
import tempfile
import shutil
import sys


def open_workshop(main_window, img_path=None): #vers 3
    """Open Workshop from main window - works with or without IMG"""
    try:
        workshop = COLWorkshop(main_window, main_window)

        if img_path:
            # Check if it's a col file or IMG file
            if img_path.lower().endswith('.col'):
                # Load standalone col file
                workshop.open_col_file(img_path)
            else:
                # Load from IMG archive
                workshop.load_from_img_archive(img_path)
        else:
            # Open in standalone mode (no IMG loaded)
            if main_window and hasattr(main_window, 'log_message'):
                main_window.log_message(App_name + " opened in standalone mode")

        workshop.show()
        return workshop
    except Exception as e:
        QMessageBox.critical(main_window, App_name * " Error", f"Failed to open: {str(e)}")
        return None


# Compatibility alias for imports
COLEditorDialog = COLWorkshop  #vers 1


def open_col_workshop(main_window, img_path=None): #vers 2
    """Open COL Workshop - embedded in tab if main_window has tab widget, standalone otherwise"""
    try:
        from PyQt6.QtWidgets import QVBoxLayout, QWidget

        # Standalone mode
        if not main_window or not hasattr(main_window, 'main_tab_widget'):
            workshop = COLWorkshop(None, main_window)
            workshop.setWindowFlags(Qt.WindowType.Window)
            if img_path and img_path.lower().endswith('.col'):
                if hasattr(workshop, 'open_col_file'):
                    workshop.open_col_file(img_path)
                elif hasattr(workshop, 'load_col_file'):
                    workshop.load_col_file(img_path)
            workshop.setWindowTitle(f"COL Workshop - {App_name}")
            workshop.resize(1200, 800)
            workshop.show()
            return workshop

        # Embedded mode - add as tab
        import os
        tab_container = QWidget()
        tab_layout = QVBoxLayout(tab_container)
        tab_layout.setContentsMargins(0, 0, 0, 0)

        workshop = COLWorkshop(tab_container, main_window)
        workshop.setWindowFlags(Qt.WindowType.Widget)
        tab_layout.addWidget(workshop)

        if img_path and img_path.lower().endswith('.col'):
            if hasattr(workshop, 'open_col_file'):
                workshop.open_col_file(img_path)
            elif hasattr(workshop, 'load_col_file'):
                workshop.load_col_file(img_path)

        tab_label = os.path.splitext(os.path.basename(img_path))[0] if img_path else "COL Workshop"
        try:
            from apps.methods.imgfactory_svg_icons import get_col_file_icon
            icon = get_col_file_icon()
            idx = main_window.main_tab_widget.addTab(tab_container, icon, tab_label)
        except Exception:
            idx = main_window.main_tab_widget.addTab(tab_container, tab_label)
        main_window.main_tab_widget.setCurrentIndex(idx)

        workshop.show()
        return workshop

    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error opening COL Workshop: {str(e)}")
        return None

COLEditorDialog = COLWorkshop

if __name__ == "__main__":
    import sys
    import traceback

    print(App_name + " Starting.")

    try:
        app = QApplication(sys.argv)
        print("QApplication created")

        workshop = COLWorkshop()
        print(App_name + " instance created")

        workshop.setWindowTitle(App_name + " - Standalone")
        workshop.resize(1200, 800)
        workshop.show()
        print("Window shown, entering event loop")
        print(f"Window visible: {workshop.isVisible()}")
        print(f"Window geometry: {workshop.geometry()}")

        sys.exit(app.exec())

    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)

