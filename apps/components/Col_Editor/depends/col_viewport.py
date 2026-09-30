#this belongs in apps/components/Col_Editor/depends/col_viewport.py - Version: 9
# X-Seti - Sept 29 2026 - IMG Factory 1.6 - COL Workshop 3D viewport

"""
COL 3D Viewport - QPainter preview of collision models.
"""

##class COL3DViewport: -
# contextMenuEvent
# _cycle_render_style
# _edit_sets
# _end_gizmo_drag
# _find_workshop
# fit_to_window
# flip_horizontal
# flip_vertical
# gamepad_step
# _get_scale_origin
# _get_ui_color
# _gizmo_arm
# _gizmo_centre
# _gizmo_pivot
# _hit_gizmo
# __init__
# keyPressEvent
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# _pad_begin_grab
# _pad_end_grab
# paintEvent
# pan
# _pick_face
# _pick_vertex
# _select_verts_in_box
# _proj
# reset_view
# resizeEvent
# rotate_ccw
# rotate_cw
# _set_angles
# set_backface
# set_background_color
# set_current_model
# set_gamepad
# _set_gizmo
# set_paint_mode
# set_render_style
# set_select_mode
# set_show_boxes
# set_show_mesh
# set_show_shadow
# set_show_spheres
# _set_theme_bg
# _show_face_context_menu
# _show_vertex_context_menu
# _start_gizmo_drag
# _to_screen
# toggle_gizmo_mode
# wheelEvent
# zoom_in
# zoom_out


from PyQt6.QtCore import Qt
from PyQt6.QtGui import QAction
from PyQt6.QtWidgets import QWidget

# Temporary 3D viewport placeholder
class COL3DViewport(QWidget): #vers 2
    """COL preview viewport.
    Left-drag = pan, Right-drag = free rotate, Scroll = zoom, Middle = pan.
    G key / button = translate gizmo, R key / button = rotate gizmo.
    """

    def __init__(self, parent=None):  #vers 3
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
        self._show_shadow  = False  # COL3 shadow mesh overlay
        self._backface     = False
        self._render_style = 'semi'
        # Sphere/box display colours (R,G,B) and fill alpha (0-255)
        self._sphere_color = (80, 200, 220)   # cyan
        self._sphere_alpha = 70               # ghost fill transparency
        self._box_color    = (220, 180, 50)   # yellow
        self._box_alpha    = 60               # ghost fill transparency
        self._bg_color     = (25, 25, 35)  # overridden on first paint
        self._theme_bg_set = False
        # drag state
        self._left_drag    = None
        self._right_drag   = None
        self._mid_drag     = None
        # gizmo
        self._gizmo_mode   = 'translate'  # 'translate' | 'rotate' | 'scale'
        self._gizmo_drag   = None         # 'X'|'Y'|'Z'|'U'(uniform scale) while dragging
        self._gizmo_start  = None
        self._gizmo_pivot3 = None         # pivot fixed for the current drag
        self._view_lock    = None         # (scale, ox, oy) frozen while dragging
        self._select_mode  = 'face'       # 'face' | 'vertex'
        self._selected_verts = set()
        self._box_sel      = None         # (start, end, add) vertex box select
        # game controller (methods/gamepad_input.GamepadPoller)
        self._gamepad      = None
        self._pad_grab     = False
        self._pad_fine     = False
        self._pad_axis     = 'free'       # 'free' | 'X' | 'Y' | 'Z' (L1/R1)
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


    #    public API                                                         
    def set_current_model(self, model, index=0):  #vers 2
        self._model = model
        self._selected_faces = set()
        self._selected_verts = set()
        # Reset view so the new model is centred and visible
        self._pan_x = 0.0
        self._pan_y = 0.0
        self._zoom  = 1.0
        self.update()


    def zoom_in(self):  #vers 1
        self._zoom = min(20.0, self._zoom * 1.25); self.update()


    def zoom_out(self):  #vers 1
        self._zoom = max(0.05, self._zoom / 1.25); self.update()


    def reset_view(self):  #vers 1
        self._yaw = 30.0; self._pitch = 20.0
        self._zoom = 1.0; self._pan_x = self._pan_y = 0.0
        self._flip_h = self._flip_v = False
        self.update()

    def fit_to_window(self):  #vers 1
        self._pan_x = self._pan_y = 0.0; self._zoom = 1.0; self.update()


    def pan(self, dx, dy):  #vers 1
        self._pan_x += dx; self._pan_y += dy; self.update()


    def rotate_cw(self):  #vers 1
        self._yaw = (self._yaw + 90) % 360; self.update()


    def rotate_ccw(self):  #vers 1
        self._yaw = (self._yaw - 90) % 360; self.update()


    def flip_horizontal(self):  #vers 1
        self._flip_h = not self._flip_h; self.update()


    def flip_vertical(self):  #vers 1
        self._flip_v = not self._flip_v; self.update()


    def set_background_color(self, rgb):  #vers 1
        self._bg_color = rgb; self._theme_bg_set = True; self.update()


    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)


    def _set_theme_bg(self, palette): #vers 2
        """Set background from palette — light theme=white, dark=near-black."""
        if self._theme_bg_set:
            return
        win = palette.color(palette.ColorRole.Window)
        if win.lightness() > 128:
            self._bg_color = (245, 245, 245)
        else:
            self._bg_color = (25, 25, 35)
        self.update()


    def set_show_spheres(self, v): self._show_spheres = v; self.update()  #vers 1
    def set_show_boxes(self,   v): self._show_boxes   = v; self.update()  #vers 1
    def set_show_mesh(self,    v): self._show_mesh     = v; self.update()  #vers 1
    def set_show_shadow(self,  v): self._show_shadow   = v; self.update()  #vers 1
    def set_backface(self,     v): self._backface      = v; self.update()  #vers 1
    def set_render_style(self, s): self._render_style  = s; self.update()  #vers 1


    def toggle_gizmo_mode(self):  #vers 1
        self._gizmo_mode = 'rotate' if self._gizmo_mode == 'translate' else 'translate'
        self._gizmo_drag = None
        self.update()


    def _set_gizmo(self, mode):  #vers 1
        self._gizmo_mode = mode; self._gizmo_drag = None; self.update()


    #    projection (self-contained, no workshop needed)                   
    def _proj(self, x, y, z):  #vers 1
        """Project 3D world point → 2D screen pixel using current view state."""
        import math
        yr = math.radians(self._yaw);   cy, sy = math.cos(yr), math.sin(yr)
        pr = math.radians(self._pitch); cp, sp = math.cos(pr), math.sin(pr)
        rx = x*cy - y*sy
        ry = x*sy + y*cy
        ry2 = ry*cp - z*sp
        return rx, ry2


    def _get_scale_origin(self):  #vers 2
        """Return (scale, ox, oy) mapping 3D projected coords to screen pixels."""
        if self._view_lock:
            return self._view_lock
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


    def _to_screen(self, x, y, z):  #vers 1
        scale, ox, oy = self._get_scale_origin()
        px, py = self._proj(x, y, z)
        return px*scale + ox, py*scale + oy


    #    gizmo hit test                                                    
    def _gizmo_pivot(self):  #vers 2
        """3D pivot: centre of selected faces/vertices, else whole model."""
        from apps.methods.col_mesh_ops import selection_centre
        if not self._model: return (0.0, 0.0, 0.0)
        return selection_centre(self._model, *self._edit_sets())


    def _edit_sets(self):  #vers 1
        """(face ids, vertex ids) the gizmo edits; vertices only in vertex mode."""
        if self._select_mode == 'vertex':
            return set(), set(self._selected_verts)
        return self._selected_faces, None


    def _gizmo_centre(self):  #vers 2
        """Screen coords of gizmo origin (selection or model centre)."""
        if not self._model: return None
        return self._to_screen(*self._gizmo_pivot())


    def _gizmo_arm(self):  #vers 1
        return max(45, min(self.width(), self.height()) * 0.15)


    def _hit_gizmo(self, mx, my):  #vers 2
        """Axis 'X'/'Y'/'Z' under the click (whole arrow or ring), 'U' for the
        scale centre, else None."""
        import math
        ctr = self._gizmo_centre()
        if not ctr: return None
        gx, gy = ctr
        arm = self._gizmo_arm()
        if self._gizmo_mode == 'scale' and math.hypot(mx - gx, my - gy) < 9:
            return 'U'

        def seg_d(ax, ay, bx, by):  #vers 1
            vx, vy = bx - ax, by - ay
            L = vx * vx + vy * vy or 1.0
            t = max(0.0, min(1.0, ((mx - ax) * vx + (my - ay) * vy) / L))
            return math.hypot(mx - (ax + t * vx), my - (ay + t * vy))

        best, best_d = None, 9.0
        if self._gizmo_mode == 'rotate':
            rings = {'X': ((0,1,0),(0,0,1)), 'Y': ((1,0,0),(0,0,1)), 'Z': ((1,0,0),(0,1,0))}
            for name, (t1, t2) in rings.items():
                pts = []
                for k in range(49):
                    a = 2 * math.pi * k / 48
                    px, py = self._proj(*(math.cos(a) * t1[n] + math.sin(a) * t2[n] for n in range(3)))
                    pts.append((gx + px * arm, gy + py * arm))
                d = min(seg_d(*pts[k], *pts[k + 1]) for k in range(48))
                if d < best_d: best_d, best = d, name
            return best
        for (dx, dy, dz), name in [((1,0,0),'X'),((0,1,0),'Y'),((0,0,1),'Z')]:
            px, py = self._proj(dx, dy, dz)
            d = seg_d(gx, gy, gx + px * arm, gy + py * arm)
            if d < best_d: best_d, best = d, name
        return best


    def set_paint_mode(self, enabled: bool, material_id: int = 0):  #vers 1
        """Enable/disable paint mode. In paint mode LMB click paints a face."""
        self._paint_mode     = enabled
        self._paint_material = material_id
        self._selected_faces = set()
        self.setCursor(Qt.CursorShape.CrossCursor if enabled else Qt.CursorShape.OpenHandCursor)
        self.update()


    def _pick_face(self, mx, my):  #vers 2
        """Return (face_index, face) whose projected triangle contains the click,
        topmost (drawn last) first, or (None, None)."""
        model = self._model
        if not model: return None, None
        verts = getattr(model, 'vertices', [])
        faces = getattr(model, 'faces',   [])
        if not verts or not faces: return None, None

        scale, ox, oy = self._get_scale_origin()

        def ts(v):  #vers 2
            x, y, z = (v.x, v.y, v.z) if hasattr(v, 'x') else (float(v[0]), float(v[1]), float(v[2]))
            px, py = self._proj(x, y, z)
            return px * scale + ox, py * scale + oy

        def side(ax, ay, bx, by, cx, cy):  #vers 1
            return (ax - cx) * (by - cy) - (bx - cx) * (ay - cy)

        n = len(verts)
        for i in range(len(faces) - 1, -1, -1):   # faces are painted in order
            face = faces[i]
            a, b, c = getattr(face, 'a', -1), getattr(face, 'b', -1), getattr(face, 'c', -1)
            if not (0 <= a < n and 0 <= b < n and 0 <= c < n): continue
            (ax, ay), (bx, by), (cx, cy) = ts(verts[a]), ts(verts[b]), ts(verts[c])
            d1 = side(mx, my, ax, ay, bx, by)
            d2 = side(mx, my, bx, by, cx, cy)
            d3 = side(mx, my, cx, cy, ax, ay)
            if not ((d1 < 0 or d2 < 0 or d3 < 0) and (d1 > 0 or d2 > 0 or d3 > 0)):
                return i, face
        return None, None


    #    mouse                                                             
    def mousePressEvent(self, event):  #vers 4
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
                        if mx <= rx + _ARW:           # prev arrow
                            ws._paint_cycle_mat(-1)
                        elif mx >= rx + _MAT_W - _ARW: # next arrow
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
                            # Sync _paint_mat_idx so prev/next arrows start from picked mat
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
                                ws._set_status(f"Filled {count} faces (mat {src_id} to {self._paint_material})")

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

            # Gizmo first: arrows / rings / scale handles take the click
            axis = self._hit_gizmo(mx, my)
            if axis:
                self._start_gizmo_drag(axis, event.position())
                return

            if self._select_mode == 'vertex':
                vi = self._pick_vertex(mx, my)
                if vi is not None:
                    if event.modifiers() & Qt.KeyboardModifier.ControlModifier:
                        self._selected_verts ^= {vi}
                    else:
                        self._selected_verts = {vi}
                    self.update()
                    return
                ctrl = bool(event.modifiers() & Qt.KeyboardModifier.ControlModifier)
                self._box_sel = (event.position(), event.position(), ctrl)
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

            self._left_drag = event.position()
            self.setCursor(Qt.CursorShape.ClosedHandCursor)
        elif event.button() == Qt.MouseButton.RightButton:
            # Try to pick a face at click pos; if hit → context menu, else → rotate drag
            mx2, my2 = event.position().x(), event.position().y()
            if self._select_mode == 'vertex':
                vi = self._pick_vertex(mx2, my2)
                if vi is not None:
                    if vi not in self._selected_verts:
                        self._selected_verts = {vi}
                    self.update()
                    self._show_vertex_context_menu(event.globalPosition().toPoint())
                    return
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


    def mouseMoveEvent(self, event):  #vers 3
        import math

        if self._box_sel and (event.buttons() & Qt.MouseButton.LeftButton):
            self._box_sel = (self._box_sel[0], event.position(), self._box_sel[2])
            self.update()
            return

        #    Gizmo drag: selected faces, or whole model when none selected
        if self._gizmo_drag and (event.buttons() & Qt.MouseButton.LeftButton):
            from apps.methods import col_mesh_ops as ops
            d  = event.position() - self._gizmo_start
            self._gizmo_start = event.position()
            axis = self._gizmo_drag
            scale, _, _ = self._get_scale_origin()
            sel, vs = self._edit_sets()
            if axis == 'U':
                f = max(0.05, 1.0 + (d.x() - d.y()) / 200.0)
                ops.scale(self._model, sel, f, f, f, self._gizmo_pivot3, vs)
            else:
                ax3 = {'X':(1,0,0),'Y':(0,1,0),'Z':(0,0,1)}[axis]
                px, py = self._proj(*ax3)
                screen_len = math.hypot(px, py) or 1.0
                along = (d.x()*px + d.y()*py) / screen_len
                if self._gizmo_mode == 'rotate':
                    deg = (d.x()*-py + d.y()*px) / screen_len * 0.8
                    ops.rotate(self._model, sel, axis, deg, self._gizmo_pivot3, vs)
                elif self._gizmo_mode == 'scale':
                    f = max(0.05, 1.0 + along / 150.0)
                    fx, fy, fz = (f if axis == 'X' else 1.0, f if axis == 'Y' else 1.0, f if axis == 'Z' else 1.0)
                    ops.scale(self._model, sel, fx, fy, fz, self._gizmo_pivot3, vs)
                else:
                    delta = along / scale
                    ops.translate(self._model, sel, *(delta * c for c in ax3), vert_ids=vs)
            self.update()
            return

        #    Pan (left drag on background)                                 
        #    Drag-select (LMB held after face click — paint-brush selection)   
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

        #    Free rotate (right drag)                                       
        if self._right_drag and (event.buttons() & Qt.MouseButton.RightButton):
            d = event.position() - self._right_drag
            self._yaw   = (self._yaw + d.x() * 0.4) % 360
            self._pitch = max(-89.0, min(89.0, self._pitch + d.y() * 0.4))
            self._right_drag = event.position(); self.update()

        #    Free rotate (middle drag)                                      
        if self._mid_drag and (event.buttons() & Qt.MouseButton.MiddleButton):
            d = event.position() - self._mid_drag
            self._yaw   = (self._yaw + d.x() * 0.4) % 360
            self._pitch = max(-89.0, min(89.0, self._pitch + d.y() * 0.4))
            self._mid_drag = event.position(); self.update()

    def mouseReleaseEvent(self, event):  #vers 3
        if event.button() == Qt.MouseButton.LeftButton:
            self._left_drag = None
            if self._box_sel:
                self._select_verts_in_box()
            if self._gizmo_drag:
                self._end_gizmo_drag()
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


    def _start_gizmo_drag(self, axis, pos):  #vers 1
        """Begin a gizmo drag: fix pivot and view, push undo."""
        self._gizmo_drag   = axis
        self._gizmo_start  = pos
        self._gizmo_pivot3 = self._gizmo_pivot()
        self._view_lock    = self._get_scale_origin()
        ws = self._find_workshop()
        if ws and ws.current_col_file and self._model in ws.current_col_file.models:
            ws._push_undo(ws.current_col_file.models.index(self._model), f"Gizmo {self._gizmo_mode}")
        self.setCursor(Qt.CursorShape.SizeAllCursor)


    def _end_gizmo_drag(self):  #vers 2
        """Finish a gizmo drag: rebuild bounds, tell the workshop."""
        from apps.methods.col_mesh_ops import recalc_bounds
        self._gizmo_drag = None
        self._view_lock  = None
        if self._model:
            recalc_bounds(self._model)
        ws = self._find_workshop()
        if ws and hasattr(ws, '_mesh_edited'):
            sel, vs = self._edit_sets()
            what = (f"{len(vs)} vertex(es)" if vs else f"{len(sel)} face(s)" if sel else "model")
            ws._mesh_edited(self._model, f"{self._gizmo_mode.title()} {what}", reselect=False)
        self.update()


    def _pick_vertex(self, mx, my):  #vers 1
        """Nearest vertex within 10px of the click, or None."""
        import math
        verts = getattr(self._model, 'vertices', []) if self._model else []
        best, best_d = None, 10.0
        for i, v in enumerate(verts):
            sx, sy = self._to_screen(v.x, v.y, v.z)
            d = math.hypot(mx - sx, my - sy)
            if d < best_d: best_d, best = d, i
        return best


    def _select_verts_in_box(self):  #vers 1
        """Select vertices inside the drag box; a click clears unless Ctrl."""
        a, b, add = self._box_sel
        self._box_sel = None
        x0, x1 = sorted((a.x(), b.x())); y0, y1 = sorted((a.y(), b.y()))
        hit = set()
        if x1 - x0 > 3 or y1 - y0 > 3:
            for i, v in enumerate(getattr(self._model, 'vertices', []) if self._model else []):
                sx, sy = self._to_screen(v.x, v.y, v.z)
                if x0 <= sx <= x1 and y0 <= sy <= y1:
                    hit.add(i)
        self._selected_verts = (self._selected_verts | hit) if add else hit
        self.update()


    def set_select_mode(self, mode):  #vers 1
        """'face' or 'vertex' selection."""
        self._select_mode = mode
        self._selected_verts = set()
        self.update()


    def set_gamepad(self, poller):  #vers 1
        """Attach a GamepadPoller (None detaches); its state drives view and edits."""
        if self._gamepad is not None:
            try:
                self._gamepad.state.disconnect(self.gamepad_step)
            except TypeError:
                pass
        self._gamepad = poller
        if poller is not None:
            poller.state.connect(self.gamepad_step)
        self.update()


    def _pad_begin_grab(self):  #vers 1
        """Start moving the selection (or whole model) with the left stick."""
        ws = self._find_workshop()
        if not self._model or not ws: return
        ws._push_undo(ws.current_col_file.models.index(self._model), f"Controller {self._gizmo_mode}")
        self._gizmo_pivot3 = self._gizmo_pivot()
        self._view_lock = self._get_scale_origin()
        self._pad_grab = True
        self._gamepad.rumble(0.2, 0.2, 40)


    def _pad_end_grab(self, commit):  #vers 1
        """Drop (commit) or cancel (undo) the controller grab."""
        self._pad_grab = False
        self._view_lock = None
        ws = self._find_workshop()
        if commit:
            from apps.methods.col_mesh_ops import recalc_bounds
            recalc_bounds(self._model)
            ws._mesh_edited(self._model, f"Controller {self._gizmo_mode}", reselect=False)
        else:
            keep = set(self._selected_faces)
            ws._undo_last_action()
            self._selected_faces = keep
        self.update()


    def gamepad_step(self, st):  #vers 1
        """One controller frame (same layout as Map Workshop).
        Right stick orbit, L2/R2 zoom, left stick pan or move the grab.
        Cross select / grab / drop, Square add face, Circle cancel / clear,
        Triangle Move/Rotate/Scale, L1/R1 axis, D-pad models or Z / 15 deg,
        Options render style, Create undo, touchpad fit, L3 fine, R3 vertex mode."""
        import math
        from apps.methods import col_mesh_ops as ops
        if not self._model:
            return
        ws = self._find_workshop()
        dt, pr, held = st['dt'], st['pressed'], st['held']
        if 'l3' in pr:
            self._pad_fine = not self._pad_fine
        fine = 0.2 if self._pad_fine else 1.0
        if st['rx'] or st['ry']:
            self._yaw = (self._yaw + st['rx'] * 120.0 * dt) % 360
            self._pitch = max(-89.0, min(89.0, self._pitch + st['ry'] * 90.0 * dt))
        if (st['lt'] or st['rt']) and not self._view_lock:
            self._zoom = max(0.02, min(40.0, self._zoom * (1.0 + (st['rt'] - st['lt']) * 1.5 * dt)))
        if 'y' in pr:
            modes = ['translate', 'rotate', 'scale']
            self._set_gizmo(modes[(modes.index(self._gizmo_mode) + 1) % 3])
        if 'l1' in pr or 'r1' in pr:
            axes = ['free', 'X', 'Y', 'Z']
            self._pad_axis = axes[(axes.index(self._pad_axis) + (-1 if 'l1' in pr else 1)) % 4]
        if 'start' in pr:
            self._cycle_render_style()
        if 'b' in pr:
            if self._pad_grab: self._pad_end_grab(False)
            else: self._selected_faces = set()
        centre = self._pick_face(self.width() / 2, self.height() / 2)[0]
        if 'a' in pr:
            if self._pad_grab:
                self._pad_end_grab(True)
            elif centre is None or centre in self._selected_faces:
                self._pad_begin_grab()
            else:
                self._selected_faces = {centre}
        if 'x' in pr and not self._pad_grab and centre is not None:
            self._selected_faces ^= {centre}
        if not self._pad_grab and ws:
            if 'back' in pr:
                keep = set(self._selected_faces)
                ws._undo_last_action(); self._selected_faces = keep
            if 'touchpad' in pr:
                self.fit_to_window()
            if 'r3' in pr and hasattr(ws, 'vertex_mode_btn'):
                ws.vertex_mode_btn.toggle()
            step = (-1 if 'up' in pr else 1 if 'down' in pr else 0)
            if step:
                n = len(ws.current_col_file.models)
                ws._select_model_by_row((ws.current_col_file.models.index(self._model) + step) % n)
                return
        if self._pad_grab:
            (sel, vs), piv, ax = self._edit_sets(), self._gizmo_pivot3, self._pad_axis
            if self._gizmo_mode == 'rotate':
                deg = st['lx'] * 90.0 * dt * fine + (-15.0 if 'left' in pr else 15.0 if 'right' in pr else 0.0)
                if deg:
                    ops.rotate(self._model, sel, 'Z' if ax == 'free' else ax, deg, piv, vs)
            elif self._gizmo_mode == 'scale':
                f = 1.0 - st['ly'] * 0.8 * dt * fine
                if f != 1.0:
                    fx, fy, fz = [(f if ax in ('free', a) else 1.0) for a in 'XYZ']
                    ops.scale(self._model, sel, fx, fy, fz, piv, vs)
            else:
                spd = max(0.5, float(self._model.bounds.radius)) * 0.6 * dt * fine
                yr = math.radians(self._yaw)
                dx = (st['lx'] * math.cos(yr) + st['ly'] * math.sin(yr)) * spd
                dy = (-st['lx'] * math.sin(yr) + st['ly'] * math.cos(yr)) * spd
                dz = (spd if 'up' in held else -spd if 'down' in held else 0.0)
                if ax == 'X': dy = dz = 0.0
                elif ax == 'Y': dx = dz = 0.0
                elif ax == 'Z': dx = dy = 0.0; dz += -st['ly'] * spd
                if dx or dy or dz:
                    ops.translate(self._model, sel, dx, dy, dz, vs)
        elif st['lx'] or st['ly']:
            self._pan_x -= st['lx'] * 400.0 * dt * fine
            self._pan_y -= st['ly'] * 400.0 * dt * fine
        self.update()


    def resizeEvent(self, event):  #vers 1
        super().resizeEvent(event)
        ws = self._find_workshop()
        if ws and hasattr(ws, 'paint_toolbar') and ws.paint_toolbar and ws.paint_toolbar.isVisible():
            vp = getattr(ws, 'preview_widget', None)
            if vp: ws.paint_toolbar.setGeometry(0, 0, vp.width(), 34)


    def wheelEvent(self, event):  #vers 1
        factor = 1.18 if event.angleDelta().y() > 0 else 1/1.18
        self._zoom = max(0.02, min(40.0, self._zoom * factor))
        self.update()


    def keyPressEvent(self, event):  #vers 3
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
        elif event.key() == Qt.Key.Key_S: self._set_gizmo('scale')
        elif event.key() == Qt.Key.Key_F: self.fit_to_window()
        elif event.key() == Qt.Key.Key_V: self._cycle_render_style()
        elif event.key() == Qt.Key.Key_Delete and self._find_workshop():
            ws = self._find_workshop()
            if self._select_mode == 'vertex': ws._edit_delete_vertices()
            else: ws._edit_delete_faces()
        else: super().keyPressEvent(event)


    def _cycle_render_style(self):  #vers 1
        modes = ['wireframe','semi','solid']
        self._render_style = modes[(modes.index(self._render_style)+1) % 3]                              if self._render_style in modes else 'semi'
        self.update()


    def contextMenuEvent(self, event):  #vers 2
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
            act = m.addAction(label, lambda s=style: self.set_render_style(s))
            act.setCheckable(True)
            act.setChecked(self._render_style == style)
        m.exec(event.globalPos())


    def _set_angles(self, yaw, pitch):  #vers 1
        self._yaw, self._pitch = float(yaw), float(pitch); self.update()


    #    paint                                                              
    def paintEvent(self, event):  #vers 7
        """Fully self-contained paint — grid, mesh, boxes, spheres, bounds, gizmo, HUD."""
        from PyQt6.QtGui import (QPainter, QColor, QFont, QPen, QBrush, QRadialGradient,
                                  QPolygonF, QLinearGradient)
        from PyQt6.QtCore import QPointF, QRectF, QRect
        import math

        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        W, H = self.width(), self.height()
        self._set_theme_bg(self.palette())
        r2, g2, b2 = self._bg_color
        p.fillRect(self.rect(), QColor(r2, g2, b2))

        if not self._model:
            p.setPen(self._get_ui_color('viewport_text'))
            p.setFont(QFont('Arial', 11))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No model selected")
            return

        scale, ox, oy = self._get_scale_origin()


        def to_screen(x, y, z):  #vers 1
            px, py = self._proj(x, y, z)
            return px * scale + ox, py * scale + oy


        def g3(obj):  #vers 1
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

        #    Extent from ALL geometry (verts + boxes + spheres)            
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

        #    Reference grid (XY plane, Z=0)                                
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

        #    Material colours — from col_materials group palette            
        try:
            from apps.methods.col_materials import get_material_qcolor, COLGame
            _game = COLGame.VC if getattr(getattr(model,'version',None),'value',3)==1 else COLGame.SA
            def mat_col(mat_id):  #vers 1
                c = get_material_qcolor(mat_id, _game)
                return c if c else self._get_ui_color('viewport_text')
        except Exception:
            def mat_col(mat_id):  #vers 1
                return self._get_ui_color('viewport_text')

        #    Mesh faces                                                     
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

        #    Shadow mesh overlay (COL3) - magenta, dashed
        s_verts = getattr(model, 'shadow_vertices', None) or []
        s_faces = getattr(model, 'shadow_faces', None) or []
        if self._show_shadow and s_verts and s_faces:
            p.setPen(QPen(QColor(230, 80, 230, 220), 1, Qt.PenStyle.DashLine))
            p.setBrush(QBrush(QColor(230, 80, 230, 40)))
            n_sv = len(s_verts)
            for sf in s_faces:
                if not (0 <= sf.a < n_sv and 0 <= sf.b < n_sv and 0 <= sf.c < n_sv): continue
                p.drawPolygon(QPolygonF([QPointF(*to_screen(*g3(s_verts[k]))) for k in (sf.a, sf.b, sf.c)]))

        #    Boxes — draw all 12 edges of AABB                              
        if self._show_boxes:
            _bc = getattr(self, '_box_color', (220, 180, 50))
            _ba = getattr(self, '_box_alpha', 30)
            _quads = [(0,1,3,2),(4,5,7,6),(0,1,5,4),(2,3,7,6),(0,2,6,4),(1,3,7,5)]
            for box in boxes:
                mn_obj = getattr(box,'min_point',getattr(box,'min',None))
                mx_obj = getattr(box,'max_point',getattr(box,'max',None))
                if mn_obj is None or mx_obj is None: continue
                x0,y0,z0 = g3(mn_obj)
                x1,y1,z1 = g3(mx_obj)
                # 8 corners
                corners=[(xa,ya,za) for xa in(x0,x1) for ya in(y0,y1) for za in(z0,z1)]
                sc=[to_screen(*c) for c in corners]
                # Ghost fill: 6 translucent sides, then 12 edges
                p.setPen(Qt.PenStyle.NoPen)
                p.setBrush(QBrush(QColor(*_bc, max(8, _ba // 3))))
                for q in _quads:
                    p.drawPolygon(QPolygonF([QPointF(*sc[k]) for k in q]))
                p.setPen(QPen(QColor(*_bc), 1.5))
                p.setBrush(Qt.BrushStyle.NoBrush)
                edges=[(0,1),(0,2),(0,4),(1,3),(1,5),(2,3),(2,6),(3,7),(4,5),(4,6),(5,7),(6,7)]
                for a2,b2 in edges:
                    ax,ay=sc[a2]; bx,by=sc[b2]
                    p.drawLine(int(ax),int(ay),int(bx),int(by))

        #    Spheres — draw 3 projected rings (equator + 2 meridians)       
        if self._show_spheres:
            _sc = getattr(self, '_sphere_color', (80, 200, 220))
            _sa = getattr(self, '_sphere_alpha', 25)
            N = 48
            _scale0 = math.hypot(*(a - b for a, b in zip(to_screen(1, 0, 0), to_screen(0, 0, 0))))
            for sph in spheres:
                cx,cy3,cz = g3(getattr(sph,'center',sph))
                r = getattr(sph,'radius',1.0)
                # Ghost fill: shaded disc (orthographic sphere outline)
                scx, scy = to_screen(cx, cy3, cz)
                rr = r * _scale0
                grad = QRadialGradient(QPointF(scx - rr * 0.35, scy - rr * 0.35), rr * 1.3)
                grad.setColorAt(0.0, QColor(min(255, _sc[0] + 90), min(255, _sc[1] + 90), min(255, _sc[2] + 90), min(255, _sa + 40)))
                grad.setColorAt(1.0, QColor(*_sc, max(10, _sa // 2)))
                p.setPen(Qt.PenStyle.NoPen)
                p.setBrush(QBrush(grad))
                p.drawEllipse(QPointF(scx, scy), rr, rr)
                p.setPen(QPen(QColor(*_sc), 1.5))
                p.setBrush(Qt.BrushStyle.NoBrush)
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

        #    Bounding box (model.bounds)                                    
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

        #    Vertex mode: all vertices as dots, selected in yellow
        if self._select_mode == 'vertex' and verts:
            p.setPen(Qt.PenStyle.NoPen)
            for vi, v in enumerate(verts):
                sx, sy = to_screen(*g3(v))
                on = vi in self._selected_verts
                p.setBrush(QBrush(QColor(255, 210, 60) if on else QColor(200, 200, 210, 170)))
                r0 = 4 if on else 2.5
                p.drawEllipse(QPointF(sx, sy), r0, r0)
            if self._box_sel:
                a, b, _ = self._box_sel
                p.setBrush(QBrush(QColor(255, 210, 60, 40)))
                p.setPen(QPen(QColor(255, 210, 60), 1, Qt.PenStyle.DashLine))
                p.drawRect(QRectF(a, b).normalized())

        #    Gizmo at selection centre (whole model when nothing selected)
        gx,gy=to_screen(*self._gizmo_pivot())
        arm=max(45,min(W,H)*0.15)
        axes=[((1,0,0),QColor(220,60,60),'X'),((0,1,0),QColor(60,200,60),'Y'),((0,0,1),QColor(60,120,220),'Z')]
        sorted_axes=sorted(axes,key=lambda a:self._proj(*a[0])[1],reverse=True)
        if self._gizmo_mode=='scale':
            for (dx,dy,dz),color,label in sorted_axes:
                px2,py2=self._proj(dx,dy,dz)
                tx,ty=gx+px2*arm,gy+py2*arm
                p.setPen(QPen(color,2)); p.drawLine(int(gx),int(gy),int(tx),int(ty))
                p.setBrush(QBrush(color)); p.drawRect(int(tx)-5,int(ty)-5,10,10)
                p.setFont(QFont('Arial',8,QFont.Weight.Bold)); p.setPen(color)
                p.drawText(int(tx+(9 if tx>=gx else -14)),int(ty+(5 if ty>=gy else -3)),label)
            p.setPen(QPen(QColor(230,230,230),1)); p.setBrush(QBrush(QColor(230,230,230,120)))
            p.drawRect(int(gx)-8,int(gy)-8,16,16)
        elif self._gizmo_mode=='translate':
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
        p.setBrush(QBrush(self._get_ui_color('border'))); p.setPen(QPen(self._get_ui_color('viewport_text'),1))
        p.drawEllipse(int(gx)-5,int(gy)-5,10,10)

        #    Top-right overlay — normal mode: Move/Rotate + Render chips   
        #                         paint mode:  material + tool chips
        if self._paint_mode:
            #    Tunable layout constants                                 
            _CHIP_H   = 26   # height of each chip row  (px)
            _BTN_W    = 32   # width  of each tool button
            _BTN_GAP  = 3    # gap between buttons
            _ICON_SZ  = 18   # SVG icon size inside button
            _MAT_W    = 200  # width of the material name chip
            _MARGIN   = 8    # right margin from viewport edge
            _ROW1_Y   = 4    # y of material chip
            _ROW2_Y   = _ROW1_Y + _CHIP_H + 2   # y of tool buttons row
            #                                                             

            mat_id = self._paint_material

            # Use cached list if available, else fall back to direct lookup
            _mat_cache = getattr(self._find_workshop(), '_paint_mat_list', []) if self._find_workshop() else []
            _mat_entry = next((m for m in _mat_cache if m[0] == mat_id), None)
            if _mat_entry:
                _, mat_name, hex_col = _mat_entry
            else:
                from apps.methods.col_materials import get_material_name, get_material_colour, COLGame
                mat_name = get_material_name(mat_id, COLGame.SA)
                hex_col  = get_material_colour(mat_id, COLGame.SA)
            mc = QColor(f"#{hex_col}")

            # Row 1: [prev] [swatch | id - name] [next]
            _ARW = 22   # arrow button width
            rx = W - _MAT_W - _MARGIN
            # prev material button
            # Theme-aware paint bar arrows
            _pal    = self.palette()
            _btn_bg = _pal.color(_pal.ColorRole.Button)
            _btn_tx = _pal.color(_pal.ColorRole.ButtonText)
            _acc    = _pal.color(_pal.ColorRole.Highlight)
            _chip_c = _pal.color(_pal.ColorRole.Base)
            _txt_c  = _pal.color(_pal.ColorRole.WindowText)
            p.setBrush(QBrush(_btn_bg)); p.setPen(QPen(_acc, 1))
            p.drawRoundedRect(rx, _ROW1_Y, _ARW, _CHIP_H, 3, 3)
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as icon_fac
            _asz = min(_ARW, _CHIP_H) - 6
            icon_fac.arrow_left_icon(color=_btn_tx.name()).paint(
                p, QRect(rx + (_ARW - _asz)//2, _ROW1_Y + (_CHIP_H - _asz)//2, _asz, _asz))
            # material name chip
            p.setBrush(QBrush(_chip_c)); p.setPen(QPen(_acc, 1))
            p.drawRoundedRect(rx+_ARW+2, _ROW1_Y, _MAT_W-_ARW*2-4, _CHIP_H, 4, 4)
            p.setBrush(QBrush(mc)); p.setPen(Qt.PenStyle.NoPen)
            p.drawRoundedRect(rx+_ARW+6, _ROW1_Y+4, _CHIP_H-8, _CHIP_H-8, 2, 2)
            p.setPen(_txt_c); p.setFont(QFont('Arial',8,QFont.Weight.Bold))
            p.drawText(rx+_ARW+_CHIP_H+4, _ROW1_Y+17, f"{mat_id} — {mat_name[:20]}")
            # next material button
            p.setBrush(QBrush(_btn_bg)); p.setPen(QPen(_acc, 1))
            p.drawRoundedRect(rx+_MAT_W-_ARW, _ROW1_Y, _ARW, _CHIP_H, 3, 3)
            icon_fac.arrow_right_icon(color=_btn_tx.name()).paint(
                p, QRect(rx+_MAT_W-_ARW + (_ARW - _asz)//2, _ROW1_Y + (_CHIP_H - _asz)//2, _asz, _asz))

            # Row 2: tool buttons using SVG icons via QIcon.paint()
            tool = getattr(self, '_tool_mode', 'paint')
            tx = W - _MAT_W - _MARGIN
            ws = self._find_workshop()

            tool_defs = [
                ('paint',   'paint_icon',   '#ff8c00'),   # orange when active
                ('dropper', 'dropper_icon', '#4fc3f7'),   # blue
                ('fill',    'fill_icon',    '#a5d6a7'),   # green
            ]
            for t_name, icon_fn, active_col in tool_defs:
                active = (tool == t_name)
                _pal3 = self.palette()
                bg  = QColor(active_col) if active else _pal3.color(_pal3.ColorRole.Button)
                bdr = QColor(active_col) if active else _pal3.color(_pal3.ColorRole.Mid)
                p.setBrush(QBrush(bg)); p.setPen(QPen(bdr, 1))
                p.drawRoundedRect(tx, _ROW2_Y, _BTN_W, _CHIP_H, 3, 3)
                # Draw SVG icon centred in button
                icon_col = '#000000' if active else active_col
                icon = getattr(icon_fac, icon_fn)(color=icon_col)
                icon_x = tx + (_BTN_W - _ICON_SZ) // 2
                icon_y = _ROW2_Y + (_CHIP_H - _ICON_SZ) // 2
                icon.paint(p, QRect(icon_x, icon_y, _ICON_SZ, _ICON_SZ))
                tx += _BTN_W + _BTN_GAP

            # Tool name label below row 2 (acts as tooltip)
            tool_labels = {'paint':'Paint', 'dropper':'Pick', 'fill':'Fill'}
            _pal4 = self.palette()
            _lbl  = _pal4.color(_pal4.ColorRole.PlaceholderText)
            p.setPen(_lbl); p.setFont(QFont('Arial',7))
            p.drawText(W - _MAT_W - _MARGIN, _ROW2_Y + _CHIP_H + 10,
                       f"Tool: {tool_labels.get(tool, tool)}")

            # Undo button
            _pal5 = self.palette()
            p.setBrush(QBrush(_pal5.color(_pal5.ColorRole.Button)))
            p.setPen(QPen(_pal5.color(_pal5.ColorRole.Mid), 1))
            p.drawRoundedRect(tx, _ROW2_Y, _BTN_W, _CHIP_H, 3, 3)
            icon = icon_fac.undo_paint_icon(color=_pal5.color(_pal5.ColorRole.ButtonText).name())
            icon_x = tx + (_BTN_W - _ICON_SZ)//2
            icon_y = _ROW2_Y + (_CHIP_H - _ICON_SZ)//2
            icon.paint(p, QRect(icon_x, icon_y, _ICON_SZ, _ICON_SZ))
            tx += _BTN_W + _BTN_GAP

            # Save button (between undo and exit)
            p.setBrush(QBrush(self.palette().color(self.palette().ColorRole.Button)))
            p.setPen(QPen(QColor(80,200,100), 1))
            p.drawRoundedRect(tx, _ROW2_Y, _BTN_W, _CHIP_H, 3, 3)
            icon = icon_fac.save_icon(color='#66bb6a')
            icon_x = tx + (_BTN_W - _ICON_SZ)//2
            icon_y = _ROW2_Y + (_CHIP_H - _ICON_SZ)//2
            icon.paint(p, QRect(icon_x, icon_y, _ICON_SZ, _ICON_SZ))
            tx += _BTN_W + _BTN_GAP

            # Exit button (close icon)
            _pal6 = self.palette()
            p.setBrush(QBrush(_pal6.color(_pal6.ColorRole.Button)))
            p.setPen(QPen(QColor(200,80,60), 1))
            p.drawRoundedRect(tx, _ROW2_Y, _BTN_W, _CHIP_H, 3, 3)
            icon = icon_fac.close_icon(color='#ef5350')
            icon_x = tx + (_BTN_W - _ICON_SZ)//2
            icon_y = _ROW2_Y + (_CHIP_H - _ICON_SZ)//2
            icon.paint(p, QRect(icon_x, icon_y, _ICON_SZ, _ICON_SZ))
        else:
            bx,by,bw,bh=W-88,4,84,22
            _pal7 = self.palette()
            p.setBrush(QBrush(_pal7.color(_pal7.ColorRole.Button)))
            p.setPen(QPen(_pal7.color(_pal7.ColorRole.Mid), 1))
            p.drawRoundedRect(bx,by,bw,bh,4,4)
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as icon_fac
            _gi = {'translate': icon_fac.arrow_up_icon, 'rotate': icon_fac.rotate_cw_icon,
                   'scale': icon_fac.dp_resize_icon}[self._gizmo_mode](color='#c8c8dc')
            _gi.paint(p, QRect(bx+3, by+3, 16, 16))
            p.setFont(QFont('Arial',8)); p.setPen(QColor(200,200,220))
            lbl={'translate': 'Move [G]', 'rotate': 'Rotate [R]', 'scale': 'Scale [S]'}[self._gizmo_mode]
            p.drawText(bx+21,by+15,lbl)
            mode_lbl={'wireframe':'Wire','semi':'Semi','solid':'Solid'}.get(rs,'?')
            mode_col={'wireframe':QColor(100,180,100),'semi':QColor(180,180,100),'solid':QColor(100,140,220)}.get(rs,self._get_ui_color('border'))
            p.setBrush(QBrush(QColor(40,44,62))); p.setPen(QPen(mode_col,1))
            p.drawRoundedRect(W-70,28,66,18,3,3)
            p.setPen(mode_col); p.setFont(QFont('Arial',7))
            p.drawText(W-66,41,f"[V] {mode_lbl}")

        #    Controller reticle: Cross / Square act on the face under it
        if self._gamepad is not None:
            cx0, cy0 = W / 2, H / 2
            p.setPen(QPen(QColor(150, 240, 255), 2))
            for a0, b0 in (((cx0-12, cy0), (cx0-4, cy0)), ((cx0+4, cy0), (cx0+12, cy0)),
                           ((cx0, cy0-12), (cx0, cy0-4)), ((cx0, cy0+4), (cx0, cy0+12))):
                p.drawLine(QPointF(*a0), QPointF(*b0))
            p.setFont(QFont('Arial', 7))
            tag = ("GRAB " if self._pad_grab else "") + f"{self._gizmo_mode} {self._pad_axis}" + (" fine" if self._pad_fine else "")
            p.drawText(int(cx0) + 14, int(cy0) + 18, tag)

        #    HUD                                                            
        p.setFont(QFont('Arial',8)); p.setPen(self._get_ui_color('border'))
        p.drawText(6,14,getattr(model,'name','') or '')
        y2=H-54-(14 if s_faces else 0)
        for col_c,txt in [(QColor(100,180,100),f"Mesh  F:{len(faces)} V:{len(verts)}"),
                          (QColor(220,180,50), f"Boxes  {len(boxes)}"),
                          (QColor(80,200,220), f"Spheres  {len(spheres)}")] + \
                         ([(QColor(230,80,230), f"Shadow  F:{len(s_faces)} V:{len(s_verts)}")] if s_faces else []):
            p.setPen(col_c); p.drawText(6,y2,txt); y2+=14
        p.setPen(QColor(120,125,140)); p.setFont(QFont('Arial',7))
        p.drawText(6,H-4,f"Y:{self._yaw:.0f}° P:{self._pitch:.0f}° Z:{self._zoom:.2f}x")

        # Paint mode indicator now shown in paint_toolbar above viewport (not drawn here)
        p.drawText(W-68,H-4,f"grid {step:.3g}")


    def _show_vertex_context_menu(self, global_pos):  #vers 1
        """Vertex-mode right-click: selection and vertex edit tools."""
        from PyQt6.QtWidgets import QMenu
        from apps.methods.imgfactory_svg_icons import (SVGIconFactory as IF,
                                                       get_select_all_icon, get_select_inverse_icon)
        ws = self._find_workshop()
        if not ws: return
        ic = ws._get_icon_color()
        menu = QMenu(self)
        menu.addAction(f"{len(self._selected_verts)} vertex(es) selected").setEnabled(False)
        menu.addSeparator()
        for label, icon, fn in [
                ("Select All",              get_select_all_icon,        ws._edit_verts_all),
                ("Select None",             IF.select_none_icon,              ws._edit_verts_none),
                ("Invert Selection",        get_select_inverse_icon,    ws._edit_verts_invert),
                (None, None, None),
                ("Set Position...",         IF.vertex_position_icon,             ws._edit_vertex_position),
                ("Create Face",             IF.create_face_icon,        ws._edit_add_face),
                ("Weld",                    IF.converge_to_center_icon, ws._edit_weld),
                ("Delete  [Del]",           IF.delete_vertex_icon,             ws._edit_delete_vertices),
                ("Mirror...",               IF.mirror_icon,             ws._edit_mirror),
                ("Select Faces Inside",     IF.select_inside_icon,        ws._edit_verts_to_faces)]:
            if label is None:
                menu.addSeparator(); continue
            menu.addAction(icon(20, ic), label, fn)
        menu.exec(global_pos)


    def _show_face_context_menu(self, global_pos, face_index, face): #vers 5
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

        #    Build menu                                                    
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
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        ic = ws._get_icon_color() if ws else None
        sel_label = (f"Apply to {n_sel} selected face(s)"
                     if n_sel > 1 else "Apply to selection")
        act_apply_sel = menu.addAction(SVGIconFactory.check_icon(20, ic), sel_label)
        act_apply_sel.setEnabled(n_sel > 1)

        # Clear material on this face (→ material 0)
        act_clear_face = menu.addAction(SVGIconFactory.close_icon(20, ic), "Clear material on this face")
        # Clear material on ALL faces in model
        model = self._model
        n_faces = len(getattr(model, 'faces', []))
        act_clear_all = menu.addAction(
            SVGIconFactory.trash_icon(20, ic), f"Clear material on all {n_faces} faces")

        menu.addSeparator()
        # Open full paint editor
        act_paint = menu.addAction("Paint — open material editor…")

        # Edit tools on the current selection (COLEditMixin)
        if ws:
            menu.addSeparator()
            edit_menu = menu.addMenu(f"Edit {len(self._selected_faces)} selected face(s)")
            for label, icon, fn in [
                    ("Detach",               SVGIconFactory.detach_faces_icon,   ws._edit_detach),
                    ("To New Model...",      SVGIconFactory.new_icon,            ws._edit_selection_to_model),
                    ("Save as COL...",       SVGIconFactory.save_selection_icon,         ws._edit_selection_to_file),
                    ("Delete",               SVGIconFactory.delete_face_icon,    ws._edit_delete_faces),
                    ("Fill Hole",            SVGIconFactory.fill_icon,           ws._edit_fill_hole),
                    ("To Box",               SVGIconFactory.faces_to_box_icon,           ws._edit_faces_to_box),
                    ("To Sphere",            SVGIconFactory.shading_sphere_icon, ws._edit_faces_to_sphere),
                    ("Scale...",             SVGIconFactory.bounds_icon,         ws._edit_scale_dialog),
                    ("Optimise Mesh...",     SVGIconFactory.filter_icon,         ws._edit_optimise)]:
                edit_menu.addAction(icon(20, ic), label, fn)

        #    Execute                                                       
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
                    ws._push_undo(mi, f"Paste material {_clip} to face {face_index}")
            if hasattr(mat, 'material_id'):
                mat.material_id = _clip
            else:
                face.material = _clip
            self.update()
            if ws and hasattr(ws, '_set_status'):
                ws._set_status(f"Pasted material {_clip} to face {face_index}")

        elif chosen == act_apply_sel:
            if ws and model:
                models = getattr(getattr(ws, 'current_col_file', None), 'models', [])
                mi = models.index(model) if model in models else -1
                if mi >= 0 and hasattr(ws, '_push_undo'):
                    ws._push_undo(mi, f"Apply material {mat_id} to {n_sel} faces")
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
                    ws._push_undo(mi, f"Clear material on face {face_index}")
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

    def _find_workshop(self):  #vers 2
        ref = getattr(self, '_workshop_ref', None)
        if ref is not None: return ref
        p = self.parent()
        while p:
            if any(c.__name__ == 'COLWorkshop' for c in type(p).__mro__): return p  # no circular import
            p = p.parent() if callable(getattr(p, 'parent', None)) else None
        return None
