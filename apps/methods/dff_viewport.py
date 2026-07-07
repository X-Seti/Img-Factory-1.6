# X-Seti - Jul07 2026 - IMG Factory 1.6 - DFF OpenGL Viewport
# this belongs in apps/methods/dff_viewport.py - Version: 13
"""
DFFViewport - Shared OpenGL viewport for DFF model rendering.
Used by Model Viewer, Model Workshop, Vehicle Workshop (docked).
Standalone tools import from their own methods/dff_viewport.py.

##Methods list -
# DFFViewport.__init__
# DFFViewport._anim_tick
# DFFViewport._apply_selection_click
# DFFViewport._auto_fit
# DFFViewport._calc_world_matrix
# DFFViewport._closest_point_on_ray
# DFFViewport._draw_assembly
# DFFViewport._draw_axes
# DFFViewport._draw_grid
# DFFViewport._draw_selection_overlay
# DFFViewport._draw_solid
# DFFViewport._draw_textured
# DFFViewport._draw_wireframe
# DFFViewport._emit_verts
# DFFViewport._face_color
# DFFViewport._flush_pending_textures
# DFFViewport._geom_flags
# DFFViewport._get_anim_rotation
# DFFViewport._get_bg_color
# DFFViewport._get_selection_count
# DFFViewport._get_ui_color
# DFFViewport._get_wheel_geom_data
# DFFViewport._notify_selection_changed
# DFFViewport._pick_edge
# DFFViewport._pick_face
# DFFViewport._pick_ray
# DFFViewport._pick_vertex
# DFFViewport._point_seg_dist2
# DFFViewport._ray_triangle_intersect
# DFFViewport._rebuild_anim_geoms
# DFFViewport._refresh
# DFFViewport._rw_wrap_to_gl
# DFFViewport._selected_set_for_mode
# DFFViewport._setup_lighting
# DFFViewport._strip_tex_suffix
# DFFViewport._upload_textures
# DFFViewport.clear_textures
# DFFViewport.fit_to_window
# DFFViewport.flip_horizontal
# DFFViewport.flip_vertical
# DFFViewport.initializeGL
# DFFViewport.load_all_geometries
# DFFViewport.load_geometry
# DFFViewport.load_wheels_dff
# DFFViewport.mouseMoveEvent
# DFFViewport.mousePressEvent
# DFFViewport.mouseReleaseEvent
# DFFViewport.paintGL
# DFFViewport.pan
# DFFViewport.reset_camera
# DFFViewport.reset_view
# DFFViewport.resizeGL
# DFFViewport.rotate_ccw
# DFFViewport.rotate_cw
# DFFViewport.set_ambient
# DFFViewport.set_animation
# DFFViewport.set_animation_speed
# DFFViewport.set_assembly_mode
# DFFViewport.set_backface
# DFFViewport.set_backface_cull
# DFFViewport.set_background_color
# DFFViewport.set_checkerboard_background
# DFFViewport.set_current_model
# DFFViewport.set_diffuse
# DFFViewport.set_light_dir
# DFFViewport.set_prelight
# DFFViewport.set_render_mode
# DFFViewport.set_show_grid
# DFFViewport.set_show_lod
# DFFViewport.set_show_mesh
# DFFViewport.set_view_lock
# DFFViewport.set_wheel_heading
# DFFViewport.toggle_door
# DFFViewport.toggle_snap_axis_constraint
# DFFViewport.toggle_snap_target
# DFFViewport.wheelEvent
# DFFViewport.zoom_in
# DFFViewport.zoom_out
# DFFViewport.set_show_grid
# DFFViewport.set_show_lod
# DFFViewport.set_view_lock
# DFFViewport.wheelEvent
# DFFViewport._upload_textures
"""

import math
import struct
from typing import Dict, List, Optional

from PyQt6.QtCore import Qt, QPoint
from PyQt6.QtWidgets import QWidget, QLabel
from PyQt6.QtGui import QColor

try:
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    from PyQt6.QtGui import QSurfaceFormat
    from OpenGL.GL import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
    _fmt = QSurfaceFormat()
    _fmt.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
    _fmt.setVersion(2, 1)
    QSurfaceFormat.setDefaultFormat(_fmt)
except Exception:
    QOpenGLWidget = QWidget
    OPENGL_AVAILABLE = False


class DFFViewport(QOpenGLWidget if OPENGL_AVAILABLE else QWidget):
    """OpenGL viewport for RenderWare DFF model rendering.
    Supports wireframe, solid, and textured modes.
    Shared base for Model Viewer, Model Workshop, Vehicle Workshop.
    """

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.setMinimumSize(200, 200)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

        # Geometry data
        self._vertices:  List  = []
        self._normals:   List  = []
        self._uvs:       List  = []
        self._triangles: List  = []
        self._materials: List  = []
        self._prelit:    List  = []
        self._tex_ids:   Dict[str,int] = {}
        self._tex_wrap:  Dict[str,tuple] = {}

        # Camera
        self._dist  = 10.0
        self._yaw   = 45.0
        self._pitch = 25.0
        self._pan_x = 0.0
        self._pan_y = 0.0
        self._last_pos = QPoint()

        # Render state
        self._mode          = 'solid'
        self._backface_cull = False  # GTA models are often 2-sided; off by default
        self._show_grid     = True
        self._use_prelight  = False
        self._ambient       = 0.4
        self._diffuse       = 0.9
        self._light_dir     = (0.5, 1.0, 0.7, 0.0)
        self._paint1        = (1.0, 0.0, 0.0)
        self._paint2        = (0.0, 0.0, 1.0)

        # Assembly / LOD
        self._all_geoms     = []
        self._assembly_mode = False
        self._show_lod      = False

        # Wheels
        self._wheels_model      = None
        self._wheels_model_path = ''
        self._wheel_type        = 'wheel_saloon_l0'

        # App settings ref (optional — set by host tool)
        self.app_settings = None
        # Explicit background override (R,G,B 0-255) — None means use theme colour
        self._bg_color_override = None

        # Sub-object selection state (vertex / edge / face / poly / object)
        self._selected_verts = set()    # set of vertex indices
        self._selected_edges = set()    # set of (vi, vj) tuples, vi < vj
        self._selected_faces = set()    # set of triangle indices
        self._select_mode    = 'object'  # 'vertex'|'edge'|'face'|'poly'|'object'

        # Snap target toggles — 3ds Max style, independently toggleable
        # (not single-select like _select_mode). All off by default; this
        # is state only for now, the actual snap-during-drag math that
        # reads these flags is a follow-up task, not wired yet.
        self._snap_targets = {
            'grid': False, 'pivot': False, 'vertex': False,
            'endpoint': False, 'midpoint': False, 'edge': False, 'face': False,
        }
        self._snap_axis_constraint = False   # "Enable Axis Constraints in Snaps"

        # Multi-pane view lock (3ds Max style Top/Front/Side/Perspective panes)
        self._view_locked = False
        self._view_label  = ""
        self._projection  = 'perspective'   # 'perspective' or 'ortho'
        self._on_geometry_loaded = None     # optional callback, set by host tool

        self._label_widget = QLabel(self)
        self._label_widget.setStyleSheet(
            "color: rgba(255,255,255,190); background: transparent; font-size: 10px;")
        self._label_widget.move(4, 2)
        self._label_widget.hide()

    def _get_ui_color(self, key): #vers 2
        """Get theme color — tries app_settings, falls back to defaults."""
        defaults = {
            'bg_panel': (25, 25, 35),
            'text_primary': (220, 220, 220),
            'border': (60, 60, 80),
        }
        if self.app_settings:
            try:
                colors = self.app_settings.get_theme_colors()
                val = colors.get(key, '')
                if val and val.startswith('#'):
                    r = int(val[1:3], 16)
                    g = int(val[3:5], 16)
                    b = int(val[5:7], 16)
                    return QColor(r, g, b)
            except Exception:
                pass
        rgb = defaults.get(key, (40, 40, 50))
        return QColor(*rgb)

    def _get_bg_color(self): #vers 1
        """Resolve actual background colour — explicit override takes priority over theme."""
        if self._bg_color_override is not None:
            return QColor(*self._bg_color_override)
        return self._get_ui_color('bg_panel')

    # - Sub-object picking (vertex / edge / face)
    # Replicates paintGL's camera transform so a ray can be cast from a
    # mouse click even though picking happens outside the paint cycle.

    def _pick_ray(self, mx: float, my: float): #vers 1
        """Return (origin, direction) as two (x,y,z) tuples for a world-space
        ray through the given widget-space pixel, or None if GL/picking
        isn't available right now (e.g. widget not yet shown)."""
        if not OPENGL_AVAILABLE or not self.isValid():
            return None
        try:
            self.makeCurrent()
            glMatrixMode(GL_PROJECTION); glLoadIdentity()
            w = max(1, self.width()); h = max(1, self.height())
            gluPerspective(45.0, w / h, 0.01, 100000.0)
            glMatrixMode(GL_MODELVIEW); glLoadIdentity()
            gluLookAt(0, 0, self._dist, 0, 0, 0, 0, 1, 0)
            glRotatef(-self._pitch, 1, 0, 0)
            glRotatef(self._yaw, 0, 0, 1)
            glTranslatef(self._pan_x, self._pan_y, 0)

            model_mat = glGetDoublev(GL_MODELVIEW_MATRIX)
            proj_mat  = glGetDoublev(GL_PROJECTION_MATRIX)
            viewport  = glGetIntegerv(GL_VIEWPORT)
            # Qt widget Y is top-down; GL viewport Y is bottom-up
            wy = h - my

            near = gluUnProject(mx, wy, 0.0, model_mat, proj_mat, viewport)
            far  = gluUnProject(mx, wy, 1.0, model_mat, proj_mat, viewport)
            self.doneCurrent()
        except Exception:
            try: self.doneCurrent()
            except Exception: pass
            return None

        ox, oy, oz = near
        dx, dy, dz = far[0]-near[0], far[1]-near[1], far[2]-near[2]
        ln = math.sqrt(dx*dx + dy*dy + dz*dz) or 1.0
        return (ox, oy, oz), (dx/ln, dy/ln, dz/ln)

    @staticmethod
    def _point_seg_dist2(p, a, b): #vers 1
        """Squared distance from point p to segment a-b (3D)."""
        abx, aby, abz = b[0]-a[0], b[1]-a[1], b[2]-a[2]
        apx, apy, apz = p[0]-a[0], p[1]-a[1], p[2]-a[2]
        ab2 = abx*abx + aby*aby + abz*abz
        t = 0.0 if ab2 < 1e-12 else max(0.0, min(1.0, (apx*abx+apy*aby+apz*abz)/ab2))
        cx, cy, cz = a[0]+abx*t, a[1]+aby*t, a[2]+abz*t
        ddx, ddy, ddz = p[0]-cx, p[1]-cy, p[2]-cz
        return ddx*ddx + ddy*ddy + ddz*ddz

    def _closest_point_on_ray(self, origin, direction, point): #vers 1
        """Param t (distance along ray) of the closest approach to `point`,
        and the squared distance from the ray to that point at that t."""
        ox, oy, oz = origin; dx, dy, dz = direction
        px, py, pz = point[0]-ox, point[1]-oy, point[2]-oz
        t = px*dx + py*dy + pz*dz
        cx, cy, cz = ox+dx*t, oy+dy*t, oz+dz*t
        ddx, ddy, ddz = point[0]-cx, point[1]-cy, point[2]-cz
        return t, ddx*ddx + ddy*ddy + ddz*ddz

    def _pick_vertex(self, mx: float, my: float): #vers 1
        """Return index of the closest vertex to the ray through (mx,my)
        within a small screen-space-equivalent tolerance, or None."""
        ray = self._pick_ray(mx, my)
        if ray is None or not self._vertices:
            return None
        origin, direction = ray
        # Tolerance scales with camera distance so it stays roughly
        # constant in screen pixels regardless of zoom.
        tol2 = (self._dist * 0.02) ** 2
        best_i, best_t, best_d2 = None, None, tol2
        for i, v in enumerate(self._vertices):
            t, d2 = self._closest_point_on_ray(origin, direction, v)
            if t < 0:
                continue
            if d2 < best_d2 or (best_i is not None and d2 <= best_d2 and t < best_t):
                best_i, best_t, best_d2 = i, t, d2
        return best_i

    def _pick_edge(self, mx: float, my: float): #vers 1
        """Return (vi, vj) (vi<vj) of the closest triangle edge to the ray,
        or None. Edges are derived from triangle sides, deduplicated."""
        ray = self._pick_ray(mx, my)
        if ray is None or not self._vertices or not self._triangles:
            return None
        origin, direction = ray
        tol2 = (self._dist * 0.02) ** 2
        edges = set()
        for tri in self._triangles:
            a, b, c = tri[0], tri[1], tri[2]
            for i, j in ((a, b), (b, c), (c, a)):
                edges.add((i, j) if i < j else (j, i))
        best_key, best_t, best_d2 = None, None, tol2
        verts = self._vertices
        for (i, j) in edges:
            try:
                va, vb = verts[i], verts[j]
            except IndexError:
                continue
            mid = ((va[0]+vb[0])/2, (va[1]+vb[1])/2, (va[2]+vb[2])/2)
            t, d2 = self._closest_point_on_ray(origin, direction, mid)
            if t < 0:
                continue
            if d2 < best_d2 or (best_key is not None and d2 <= best_d2 and t < best_t):
                best_key, best_t, best_d2 = (i, j), t, d2
        return best_key

    def _ray_triangle_intersect(self, origin, direction, v0, v1, v2): #vers 1
        """Möller–Trumbore ray/triangle test. Returns t (distance along the
        ray) on hit, or None. Backface-tolerant (tests both winding orders)."""
        eps = 1e-9
        e1 = (v1[0]-v0[0], v1[1]-v0[1], v1[2]-v0[2])
        e2 = (v2[0]-v0[0], v2[1]-v0[1], v2[2]-v0[2])
        dx, dy, dz = direction
        px = dy*e2[2] - dz*e2[1]
        py = dz*e2[0] - dx*e2[2]
        pz = dx*e2[1] - dy*e2[0]
        det = e1[0]*px + e1[1]*py + e1[2]*pz
        if -eps < det < eps:
            return None
        inv_det = 1.0 / det
        tx, ty, tz = origin[0]-v0[0], origin[1]-v0[1], origin[2]-v0[2]
        u = (tx*px + ty*py + tz*pz) * inv_det
        if u < -1e-6 or u > 1 + 1e-6:
            return None
        qx = ty*e1[2] - tz*e1[1]
        qy = tz*e1[0] - tx*e1[2]
        qz = tx*e1[1] - ty*e1[0]
        v = (dx*qx + dy*qy + dz*qz) * inv_det
        if v < -1e-6 or u + v > 1 + 1e-6:
            return None
        t = (e1[0]*qx + e1[1]*qy + e1[2]*qz) * inv_det
        if t < 1e-6:
            return None
        return t

    def _pick_face(self, mx: float, my: float): #vers 1
        """Return index of the closest triangle hit by the ray through
        (mx,my), or None. Picks the nearest intersection along the ray
        (i.e. respects depth — front-most triangle wins)."""
        ray = self._pick_ray(mx, my)
        if ray is None or not self._vertices or not self._triangles:
            return None
        origin, direction = ray
        verts = self._vertices
        best_i, best_t = None, None
        for i, tri in enumerate(self._triangles):
            a, b, c = tri[0], tri[1], tri[2]
            try:
                v0, v1, v2 = verts[a], verts[b], verts[c]
            except IndexError:
                continue
            t = self._ray_triangle_intersect(origin, direction, v0, v1, v2)
            if t is not None and (best_t is None or t < best_t):
                best_i, best_t = i, t
        return best_i

    def _selected_set_for_mode(self, mode=None): #vers 1
        """Return the live selection set for the given (or current) mode."""
        mode = mode or getattr(self, '_select_mode', 'object')
        if mode == 'vertex':
            return self._selected_verts
        if mode == 'edge':
            return self._selected_edges
        return self._selected_faces   # 'face' and 'poly' share one set

    def _apply_selection_click(self, mode, key, modifiers): #vers 1
        """Apply a single click selection. Ctrl+click toggles the item;
        Shift+click adds without replacing; plain click replaces selection."""
        sel = self._selected_set_for_mode(mode)
        if modifiers & Qt.KeyboardModifier.ControlModifier:
            if key in sel:
                sel.discard(key)
            else:
                sel.add(key)
        elif modifiers & Qt.KeyboardModifier.ShiftModifier:
            sel.add(key)
        else:
            sel.clear()
            sel.add(key)
        self._notify_selection_changed()

    def toggle_snap_target(self, target: str): #vers 1
        """Flip one snap target on/off (independently toggleable, not
        single-select - matches 3ds Max's snap ribbon where Vertex+Endpoint+
        Midpoint etc. can all be active together). No-op for unknown keys
        rather than raising, since this is called from button clicks."""
        if target in self._snap_targets:
            self._snap_targets[target] = not self._snap_targets[target]

    def toggle_snap_axis_constraint(self): #vers 1
        self._snap_axis_constraint = not self._snap_axis_constraint

    def _get_selection_count(self): #vers 1
        """Count of currently selected items in the active sub-object mode.
        Mirrors COL3DViewport's identically-named method - see note in
        toolbar_layout_manager.py session about consolidating the two
        viewport classes' duplicated selection logic into methods/."""
        mode = getattr(self, '_select_mode', 'face')
        if mode == 'vertex':
            return len(self._selected_verts)
        if mode == 'edge':
            return len(self._selected_edges)
        return len(self._selected_faces)   # 'face' and 'poly' both live here

    def _notify_selection_changed(self): #vers 3
        """Tell the parent ModelWorkshop panel the selection set changed,
        so it can refresh the 'N Vertices/Edges/Faces/Polygons Selected'
        label. Uses _workshop_ref (set at construction in model_workshop.py)
        rather than walking the Qt parent chain - more reliable since this
        widget is set up with a direct back-reference already."""
        ws = getattr(self, '_workshop_ref', None)
        if ws is not None and hasattr(ws, '_update_selection_count_label'):
            ws._update_selection_count_label()
        if ws is not None and hasattr(ws, '_sync_selection_to_other_viewports'):
            ws._sync_selection_to_other_viewports(self)

    def initializeGL(self): #vers 2
        if not OPENGL_AVAILABLE: return
        bg = self._get_bg_color()
        glClearColor(bg.redF(), bg.greenF(), bg.blueF(), 1.0)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        self._setup_lighting()

    def _setup_lighting(self): #vers 6
        if not OPENGL_AVAILABLE: return
        import ctypes

        _GL_LIGHTING             = 0x0B50
        _GL_LIGHT0               = 0x4000
        _GL_POSITION             = 0x1203
        _GL_AMBIENT              = 0x1200
        _GL_DIFFUSE              = 0x1201
        _GL_SPECULAR             = 0x1202
        _GL_COLOR_MATERIAL       = 0x0B57
        _GL_FRONT_AND_BACK       = 0x0408
        _GL_AMBIENT_AND_DIFFUSE  = 0x1602

        _libGL = None
        for _name in ('libGL.so.1', 'libGL.so', '/usr/lib/libGL.so.1',
                      '/usr/lib/libGL.so', '/usr/lib64/libGL.so.1',
                      'libOpenGL.so.0', '/usr/lib/x86_64-linux-gnu/libGL.so.1'):
            try:
                _libGL = ctypes.CDLL(_name)
                break
            except OSError:
                _libGL = None

        if _libGL is None:
            import glob
            for _p in glob.glob('/usr/lib*/**/libGL.so*', recursive=True):
                try:
                    _libGL = ctypes.CDLL(_p)
                    break
                except OSError:
                    _libGL = None

        if _libGL is None:
            print("[DFFViewport] libGL not found — lighting disabled")
            return

        _f4 = ctypes.c_float * 4
        _libGL.glEnable(_GL_LIGHTING)
        _libGL.glEnable(_GL_LIGHT0)
        ld = self._light_dir
        a, d = self._ambient, self._diffuse
        _libGL.glLightfv(_GL_LIGHT0, _GL_POSITION, _f4(ld[0], ld[1], ld[2], ld[3]))
        _libGL.glLightfv(_GL_LIGHT0, _GL_AMBIENT,  _f4(a, a, a, 1.0))
        _libGL.glLightfv(_GL_LIGHT0, _GL_DIFFUSE,  _f4(d, d, d, 1.0))
        _libGL.glLightfv(_GL_LIGHT0, _GL_SPECULAR, _f4(0.3, 0.3, 0.3, 1.0))
        _libGL.glEnable(_GL_COLOR_MATERIAL)
        _libGL.glColorMaterial(_GL_FRONT_AND_BACK, _GL_AMBIENT_AND_DIFFUSE)

    def resizeGL(self, w, h): #vers 2
        if not OPENGL_AVAILABLE: return
        glViewport(0, 0, max(1, w), max(1, h))
        glMatrixMode(GL_PROJECTION); glLoadIdentity()
        aspect = max(1, w) / max(1, h)
        if self._projection == 'ortho':
            half_h = max(0.01, self._dist * 0.5)
            glOrtho(-half_h*aspect, half_h*aspect, -half_h, half_h, -100000.0, 100000.0)
        else:
            gluPerspective(45.0, aspect, 0.01, 100000.0)
        glMatrixMode(GL_MODELVIEW)
        self._label_widget.move(4, 2)

    def paintGL(self): #vers 4
        if not OPENGL_AVAILABLE: return
        bg = self._get_bg_color()
        glClearColor(bg.redF(), bg.greenF(), bg.blueF(), 1.0)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity()
        gluLookAt(0, 0, self._dist, 0, 0, 0, 0, 1, 0)
        glRotatef(-self._pitch, 1, 0, 0)
        glRotatef(self._yaw, 0, 0, 1)
        glTranslatef(self._pan_x, self._pan_y, 0)
        if self._backface_cull:
            glEnable(GL_CULL_FACE); glCullFace(GL_BACK)
        else:
            glDisable(GL_CULL_FACE)
        self._setup_lighting()
        has_geoms = bool(getattr(self, '_all_geoms', None))
        has_verts = bool(self._vertices)
        if not has_geoms and not has_verts:
            if self._show_grid: self._draw_grid()
            self._draw_axes()
            return
        if has_geoms:
            self._draw_assembly()
        elif has_verts:
            if   self._mode == 'wireframe': self._draw_wireframe()
            elif self._mode == 'solid':     self._draw_solid()
            elif self._mode == 'textured':  self._draw_textured()
            self._draw_selection_overlay()
        if self._show_grid: self._draw_grid()
        self._draw_axes()

    def _draw_grid(self): #vers 1
        if not OPENGL_AVAILABLE: return
        glDisable(GL_LIGHTING)
        glLineWidth(0.5)
        step = max(1, int(self._dist / 5))
        rng  = step * 10
        glBegin(GL_LINES)
        for i in range(-rng, rng + 1, step):
            glColor4f(0.3, 0.3, 0.4, 0.4)
            glVertex3f(i, -rng, 0); glVertex3f(i, rng, 0)
            glVertex3f(-rng, i, 0); glVertex3f(rng, i, 0)
        glEnd()
        glEnable(GL_LIGHTING)

    def _draw_axes(self): #vers 1
        if not OPENGL_AVAILABLE: return
        glDisable(GL_LIGHTING); glLineWidth(1.5)
        s = max(1.0, self._dist * 0.1)
        glBegin(GL_LINES)
        glColor3f(1,0,0); glVertex3f(0,0,0); glVertex3f(s,0,0)
        glColor3f(0,1,0); glVertex3f(0,0,0); glVertex3f(0,s,0)
        glColor3f(0,0,1); glVertex3f(0,0,0); glVertex3f(0,0,s)
        glEnd()
        glEnable(GL_LIGHTING)

    def _face_color(self, mat_id): #vers 5
        """Return (r,g,b,a) 0-1 for a material including alpha."""
        mats = self._materials
        if mats and 0 <= mat_id < len(mats):
            mat  = mats[mat_id]
            c    = mat.colour
            r = getattr(c,'r',180); g = getattr(c,'g',180)
            b = getattr(c,'b',180); a = getattr(c,'a',255)
            has_tex = bool(getattr(mat,'texture_name',''))
            if r==0 and g==0 and b==0 and not has_tex:
                return 0.55, 0.55, 0.55, 1.0
            if g==255 and r<100 and b<50:
                return self._paint1[0], self._paint1[1], self._paint1[2], a/255
            if r==255 and g<50 and b>100:
                return self._paint2[0], self._paint2[1], self._paint2[2], a/255
            return r/255, g/255, b/255, a/255
        return 0.7, 0.7, 0.7, 1.0

    def _emit_verts(self, v1, v2, v3, use_prelit=False, use_uv=False): #vers 1
        verts = self._vertices; norms = self._normals
        uvs   = self._uvs;      prelit = self._prelit
        has_n = len(norms)  == len(verts)
        has_u = len(uvs)    == len(verts) and use_uv
        has_p = len(prelit) == len(verts) and use_prelit
        for vi in (v1, v2, v3):
            if vi >= len(verts): continue
            if has_p:
                p = prelit[vi]
                glColor3f(p[0]/255, p[1]/255, p[2]/255)
            if has_n:
                n = norms[vi]; glNormal3f(n[0], n[1], n[2])
            if has_u:
                u = uvs[vi]; glTexCoord2f(u[0], u[1])
            v = verts[vi]; glVertex3f(v[0], v[1], v[2])

    def _draw_wireframe(self): #vers 1
        if not OPENGL_AVAILABLE: return
        glDisable(GL_LIGHTING)
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        glColor3f(0.65, 0.75, 1.0); glLineWidth(0.8)
        glBegin(GL_TRIANGLES)
        for v1,v2,v3,mid in self._triangles:
            self._emit_verts(v1,v2,v3)
        glEnd()
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        glEnable(GL_LIGHTING)

    def _draw_selection_overlay(self): #vers 1
        """Highlight the active sub-object selection (vertex/edge/face/poly)
        on top of the already-rendered mesh. No-op in 'object' mode."""
        if not OPENGL_AVAILABLE: return
        mode = getattr(self, '_select_mode', 'object')
        if mode == 'object':
            return
        verts = self._vertices
        if not verts:
            return
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)

        if mode == 'vertex' and self._selected_verts:
            glColor3f(1.0, 0.8, 0.1)
            glPointSize(max(4.0, min(10.0, self._dist * 0.03)))
            glBegin(GL_POINTS)
            for vi in self._selected_verts:
                if 0 <= vi < len(verts):
                    v = verts[vi]; glVertex3f(v[0], v[1], v[2])
            glEnd()

        elif mode == 'edge' and self._selected_edges:
            glColor3f(1.0, 0.8, 0.1)
            glLineWidth(3.0)
            glBegin(GL_LINES)
            for (vi, vj) in self._selected_edges:
                if 0 <= vi < len(verts) and 0 <= vj < len(verts):
                    a, b = verts[vi], verts[vj]
                    glVertex3f(*a); glVertex3f(*b)
            glEnd()

        elif mode in ('face', 'poly') and self._selected_faces:
            glColor4f(1.0, 0.8, 0.1, 0.45)
            glEnable(GL_BLEND); glDepthMask(False)
            glBegin(GL_TRIANGLES)
            for fi in self._selected_faces:
                if 0 <= fi < len(self._triangles):
                    v1, v2, v3, _ = self._triangles[fi]
                    self._emit_verts(v1, v2, v3)
            glEnd()
            glDepthMask(True)
            # Outline on top so the selection reads clearly in solid mode
            glColor3f(1.0, 0.9, 0.2); glLineWidth(2.0)
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
            glBegin(GL_TRIANGLES)
            for fi in self._selected_faces:
                if 0 <= fi < len(self._triangles):
                    v1, v2, v3, _ = self._triangles[fi]
                    self._emit_verts(v1, v2, v3)
            glEnd()
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)

        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def _draw_solid(self): #vers 3
        if not OPENGL_AVAILABLE: return
        flags = self._geom_flags()
        use_lighting = bool(flags & self.rpGEOMETRYLIGHT) and bool(self._normals)
        use_prelit   = bool(flags & self.rpGEOMETRYPRELIT) and bool(self._prelit)
        use_modulate = bool(flags & self.rpGEOMETRYMODULATEMATERIALCOLOR)
        if use_lighting:
            glEnable(GL_LIGHTING)
        else:
            glDisable(GL_LIGHTING)
        use_p = (use_prelit or self._use_prelight) and bool(self._prelit)
        opaque = []; transparent = []
        for tri in self._triangles:
            fc = self._face_color(tri[3])
            (transparent if len(fc)>3 and fc[3]<0.99 else opaque).append((tri,fc))
        glBegin(GL_TRIANGLES)
        for (v1,v2,v3,mid),(r,g,b,*rest) in opaque:
            a = rest[0] if rest else 1.0
            if not use_p:
                if use_modulate: glColor4f(r,g,b,a)
                else: glColor4f(1.0,1.0,1.0,1.0)
            self._emit_verts(v1,v2,v3, use_prelit=use_p)
        glEnd()
        if transparent:
            glEnable(GL_BLEND); glDepthMask(False)
            glBegin(GL_TRIANGLES)
            for (v1,v2,v3,mid),(r,g,b,a) in transparent:
                if not use_p:
                    if use_modulate: glColor4f(r,g,b,a)
                    else: glColor4f(1.0,1.0,1.0,a)
                self._emit_verts(v1,v2,v3, use_prelit=use_p)
            glEnd()
            glDepthMask(True); glDisable(GL_BLEND)
        glEnable(GL_LIGHTING)
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        glColor4f(0,0,0,0.18); glLineWidth(0.5)
        glEnable(GL_POLYGON_OFFSET_LINE); glPolygonOffset(-1,-1)
        glBegin(GL_TRIANGLES)
        for v1,v2,v3,mid in self._triangles:
            self._emit_verts(v1,v2,v3)
        glEnd()
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        glDisable(GL_POLYGON_OFFSET_LINE)

    def _draw_textured(self): #vers 3
        if not OPENGL_AVAILABLE: return
        flags = self._geom_flags()
        use_lighting = bool(flags & self.rpGEOMETRYLIGHT) and bool(self._normals)
        use_prelit   = bool(flags & self.rpGEOMETRYPRELIT) and bool(self._prelit)
        use_modulate = bool(flags & self.rpGEOMETRYMODULATEMATERIALCOLOR)
        if use_lighting:
            glEnable(GL_LIGHTING)
        else:
            glDisable(GL_LIGHTING)
        glEnable(GL_TEXTURE_2D)
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,
                  GL_MODULATE if use_modulate else GL_REPLACE)
        use_p = (use_prelit or self._use_prelight) and bool(self._prelit)
        mats  = self._materials
        batches: Dict[tuple,list] = {}
        no_tex = []
        for tri in self._triangles:
            v1,v2,v3,mid = tri
            tname = ''
            if mats and 0 <= mid < len(mats):
                tname = getattr(mats[mid],'texture_name','') or ''
            gl_id = self._tex_ids.get(tname.lower(), 0)
            if gl_id:
                r,g,b,a = self._face_color(mid)
                key = (gl_id, round(r,2), round(g,2), round(b,2), round(a,2))
                batches.setdefault(key,[]).append(tri)
            else:
                no_tex.append(tri)
        opaque_b = {k:v for k,v in batches.items() if k[4]>=0.99}
        transp_b = {k:v for k,v in batches.items() if k[4]<0.99}
        for batch_dict, use_blend in [(opaque_b,False),(transp_b,True)]:
            if use_blend: glEnable(GL_BLEND); glDepthMask(False)
            for key, tris in batch_dict.items():
                gl_id=key[0]; r=key[1]; g=key[2]; b=key[3]; a=key[4]
                glBindTexture(GL_TEXTURE_2D, gl_id)
                if not use_p:
                    if use_modulate: glColor4f(r,g,b,a)
                    else: glColor4f(1.0,1.0,1.0,a)
                glBegin(GL_TRIANGLES)
                for v1,v2,v3,mid in tris:
                    self._emit_verts(v1,v2,v3, use_prelit=use_p, use_uv=True)
                glEnd()
            if use_blend: glDepthMask(True); glDisable(GL_BLEND)
        glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)
        no_opaque = [t for t in no_tex if self._face_color(t[3])[3]>=0.99]
        no_transp = [t for t in no_tex if self._face_color(t[3])[3]<0.99]
        for tri_list, use_blend in [(no_opaque,False),(no_transp,True)]:
            if use_blend: glEnable(GL_BLEND); glDepthMask(False)
            for v1,v2,v3,mid in tri_list:
                r,g,b,a = self._face_color(mid)
                if not use_p:
                    if use_modulate: glColor4f(r,g,b,a)
                    else: glColor4f(1.0,1.0,1.0,a)
                glBegin(GL_TRIANGLES)
                self._emit_verts(v1,v2,v3, use_prelit=use_p)
                glEnd()
            if use_blend: glDepthMask(True); glDisable(GL_BLEND)
        glEnable(GL_LIGHTING)

    # RW geometry flags
    rpGEOMETRYTRISTRIP          = 0x0001
    rpGEOMETRYPOSITIONS         = 0x0002
    rpGEOMETRYTEXTURED          = 0x0004
    rpGEOMETRYPRELIT            = 0x0008
    rpGEOMETRYNORMALS           = 0x0010
    rpGEOMETRYLIGHT             = 0x0020
    rpGEOMETRYMODULATEMATERIALCOLOR = 0x0040
    rpGEOMETRYTEXTURED2         = 0x0080

    def _geom_flags(self): #vers 1
        """Return geometry flags from current model, or sensible defaults."""
        return getattr(self, '_current_geom_flags',
               self.rpGEOMETRYLIGHT | self.rpGEOMETRYMODULATEMATERIALCOLOR | self.rpGEOMETRYNORMALS)

    def _strip_tex_suffix(self, name: str) -> str: #vers 2
        """Strip GTA texture suffix.
        Handles: buildrt4_fehihwm (alpha suffix) and vehiclegeneric256 (numeric size suffix)."""
        import re as _re
        n = _re.sub(r'_[a-z]{4,8}$', '', name)
        n = _re.sub(r'\d+$', '', n)
        return n

    def _rw_wrap_to_gl(self, rw: int) -> int: #vers 1
        """Convert RW addressing mode to GL wrap constant.
        0=NONE 1=WRAP 2=CLAMP 3=MIRROR"""
        if not OPENGL_AVAILABLE: return 0
        if rw == 2: return GL_CLAMP_TO_EDGE
        if rw == 3: return GL_MIRRORED_REPEAT
        return GL_REPEAT

    def _upload_textures(self, textures: list, additive: bool = False): #vers 4
        if not OPENGL_AVAILABLE: return
        # Guard: don't attempt upload if GL context not initialized
        try:
            if hasattr(self, 'isValid') and not self.isValid():
                self._pending_textures = getattr(self, '_pending_textures', []) + textures
                from PyQt6.QtCore import QTimer
                QTimer.singleShot(200, lambda: self._flush_pending_textures())
                return
        except Exception:
            pass
        self.makeCurrent()
        if not additive:
            self.clear_textures()
        for tex in textures:
            name = tex.get('name','').lower()
            rgba = tex.get('rgba_data', b'')
            w    = tex.get('width', 0); h = tex.get('height', 0)
            if not (name and rgba and w > 0 and h > 0): continue
            wrap_u = tex.get('wrap_u', 1)
            wrap_v = tex.get('wrap_v', 1)
            gl_wrap_s = self._rw_wrap_to_gl(wrap_u)
            gl_wrap_t = self._rw_wrap_to_gl(wrap_v)
            gl_id = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, gl_wrap_s)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, gl_wrap_t)
            try:
                glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,w,h,0,GL_RGBA,GL_UNSIGNED_BYTE,rgba)
                glGenerateMipmap(GL_TEXTURE_2D)
                self._tex_ids[name] = gl_id
                self._tex_wrap[name] = (wrap_u, wrap_v)
            except Exception as e:
                print(f"[DFFViewport] Tex upload fail '{name}': {e}")
                glDeleteTextures(1,[gl_id])
        glBindTexture(GL_TEXTURE_2D, 0); self.doneCurrent()

    def _flush_pending_textures(self): #vers 2
        """Upload any textures that were queued before GL context was ready."""
        pending = getattr(self, '_pending_textures', [])
        if not pending: return
        if hasattr(self, 'isValid') and not self.isValid():
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(200, self._flush_pending_textures)
            return
        self._pending_textures = []
        self._upload_textures(pending, additive=True)
        self.update()

    def clear_textures(self): #vers 2
        if OPENGL_AVAILABLE and self._tex_ids:
            try: glDeleteTextures(len(self._tex_ids), list(self._tex_ids.values()))
            except Exception: pass
        self._tex_ids.clear()
        self._tex_wrap.clear()

    def load_geometry(self, geometry, materials: list): #vers 3
        self._all_geoms  = []  # clear multi-geom data
        self._current_geom_flags = getattr(geometry, 'flags', 0)
        self._vertices  = [(v.x,v.y,v.z) for v in geometry.vertices]
        self._normals   = [(n.x,n.y,n.z) for n in geometry.normals] if geometry.normals else []
        self._uvs       = [(u.u,u.v) for u in geometry.uv_layers[0]] if geometry.uv_layers else []
        self._triangles = [(t.v1,t.v2,t.v3,t.material_id) for t in geometry.triangles]
        self._materials = materials
        self._prelit    = [(c.r,c.g,c.b,c.a) for c in getattr(geometry,'colors',[])] if geometry.colors else []
        self._auto_fit(); self.update()
        if self._on_geometry_loaded:
            try: self._on_geometry_loaded()
            except Exception: pass

    def _auto_fit(self): #vers 1
        if not self._vertices: return
        xs=[v[0] for v in self._vertices]; ys=[v[1] for v in self._vertices]; zs=[v[2] for v in self._vertices]
        diag = math.sqrt((max(xs)-min(xs))**2+(max(ys)-min(ys))**2+(max(zs)-min(zs))**2)
        self._dist  = max(diag*1.5, 2.0)
        self._pan_x = -(max(xs)+min(xs))/2
        self._pan_y = -(max(ys)+min(ys))/2
        self.update()

    def set_render_mode(self, mode: str): #vers 1
        self._mode = mode; self.update()

    def set_backface_cull(self, v: bool): #vers 1
        self._backface_cull = v; self.update()

    def set_show_grid(self, v: bool): #vers 1
        self._show_grid = v; self.update()

    def load_all_geometries(self, geometries, materials_list, frames, atomics, damaged=False): #vers 4
        self._all_geoms = []
        self._vertices  = []  # clear single-geom data
        # Use flags from first geometry as representative
        if geometries:
            self._current_geom_flags = getattr(geometries[0], 'flags', 0)
        fname = {i: (f.name.lower() if f.name else '') for i,f in enumerate(frames)}
        for i, geom in enumerate(geometries):
            atomic = next((a for a in atomics if a.geometry_index == i), None)
            if not atomic: continue
            fi   = atomic.frame_index
            name = fname.get(fi, '')
            is_dam = name.endswith('_dam')
            is_ok  = name.endswith('_ok')
            is_lod = name.endswith('_vlo') or name.endswith('_lo')
            if is_dam and not damaged: continue
            if is_ok  and damaged: continue
            if is_lod and not getattr(self, '_show_lod', False): continue
            # Skip frames hidden by the frame tree
            if name and name in getattr(self, '_hidden_frames', set()): continue
            rot, tx, ty, tz = self._calc_world_matrix(frames, fi)
            verts = [(rot[0]*v.x+rot[1]*v.y+rot[2]*v.z+tx,
                      rot[3]*v.x+rot[4]*v.y+rot[5]*v.z+ty,
                      rot[6]*v.x+rot[7]*v.y+rot[8]*v.z+tz) for v in geom.vertices]
            norms = [(rot[0]*n.x+rot[1]*n.y+rot[2]*n.z,
                      rot[3]*n.x+rot[4]*n.y+rot[5]*n.z,
                      rot[6]*n.x+rot[7]*n.y+rot[8]*n.z) for n in geom.normals] if geom.normals else []
            uvs   = [(u.u,u.v) for u in geom.uv_layers[0]] if geom.uv_layers else []
            tris  = [(t.v1,t.v2,t.v3,t.material_id) for t in geom.triangles]
            prelit= [(c.r,c.g,c.b,c.a) for c in geom.colors] if geom.colors else []
            geom_flags = getattr(geom, 'flags', 0)
            self._all_geoms.append((verts,norms,uvs,tris,geom.materials,prelit,geom_flags))

        # Place wheels at dummy frames using wheels.DFF — only when _show_wheels is set
        if getattr(self, '_show_wheels', False):
            wheel_data = self._get_wheel_geom_data()
            if wheel_data:
                wv,wn,wu,wt,wm,wp = wheel_data[:6]
                wflags = wheel_data[6] if len(wheel_data) > 6 else 0
                front_scale = getattr(self, '_wheel_front_scale', 1.0)
                rear_scale  = getattr(self, '_wheel_rear_scale',  1.0)
                mult        = getattr(self, '_wheel_scale_mult', 1.0)
                front_scale *= mult
                rear_scale  *= mult
                for fi2, fn2 in fname.items():
                    if 'dummy' not in fn2: continue
                    if not any(w in fn2 for w in ('wheel_lf','wheel_rf','wheel_lb','wheel_rb',
                                                   'wheel_lm','wheel_rm')): continue
                    r2,tx2,ty2,tz2 = self._calc_world_matrix(frames, fi2)
                    is_left  = 'wheel_l' in fn2
                    is_front = 'wheel_lf' in fn2 or 'wheel_rf' in fn2
                    scale    = front_scale if is_front else rear_scale
                    v2 = []
                    for vx,vy,vz in wv:
                        # Scale wheel geometry around its local origin
                        svx, svy, svz = vx*scale, vy*scale, vz*scale
                        wx = r2[0]*svx+r2[1]*svy+r2[2]*svz+tx2
                        wy = r2[3]*svx+r2[4]*svy+r2[5]*svz+ty2
                        wz = r2[6]*svx+r2[7]*svy+r2[8]*svz+tz2
                        if is_left:
                            wx = tx2 - (r2[0]*svx+r2[1]*svy+r2[2]*svz)
                        v2.append((wx,wy,wz))
                    n2 = [(r2[0]*nx+r2[1]*ny+r2[2]*nz,
                           r2[3]*nx+r2[4]*ny+r2[5]*nz,
                           r2[6]*nx+r2[7]*ny+r2[8]*nz) for nx,ny,nz in wn] if wn else []
                    self._all_geoms.append((v2,n2,wu,wt,wm,wp,wflags))
        all_pts=[p for g in self._all_geoms for p in g[0]]
        if all_pts:
            xs=[p[0] for p in all_pts]; ys=[p[1] for p in all_pts]
            diag=math.sqrt(max(1,(max(xs)-min(xs))**2+(max(ys)-min(ys))**2))
            self._dist=max(diag*2.0,2.0)
            self._pan_x=-(max(xs)+min(xs))/2; self._pan_y=-(max(ys)+min(ys))/2
        self.update()
        if self._on_geometry_loaded:
            try: self._on_geometry_loaded()
            except Exception: pass

    def _calc_world_matrix(self, frames, frame_idx): #vers 1
        r=[1,0,0,0,1,0,0,0,1]; tx=ty=tz=0.0
        visited=set(); idx=frame_idx; chain=[]
        while 0<=idx<len(frames) and idx not in visited:
            visited.add(idx); chain.append(frames[idx]); idx=frames[idx].parent_index
        for frame in reversed(chain):
            fr=frame.rotation; fp=frame.position
            nr=[r[0]*fr[0]+r[1]*fr[3]+r[2]*fr[6],r[0]*fr[1]+r[1]*fr[4]+r[2]*fr[7],r[0]*fr[2]+r[1]*fr[5]+r[2]*fr[8],
                r[3]*fr[0]+r[4]*fr[3]+r[5]*fr[6],r[3]*fr[1]+r[4]*fr[4]+r[5]*fr[7],r[3]*fr[2]+r[4]*fr[5]+r[5]*fr[8],
                r[6]*fr[0]+r[7]*fr[3]+r[8]*fr[6],r[6]*fr[1]+r[7]*fr[4]+r[8]*fr[7],r[6]*fr[2]+r[7]*fr[5]+r[8]*fr[8]]
            r=nr; tx+=fp.x; ty+=fp.y; tz+=fp.z
        return r, tx, ty, tz

    def load_wheels_dff(self, path: str, wheel_type: str = 'wheel_saloon_l0'): #vers 1
        try:
            from apps.methods.dff_parser import load_dff
            self._wheels_model = load_dff(path)
            self._wheel_type   = wheel_type
        except Exception as e:
            print(f"[DFFViewport] Wheel DFF load fail: {e}")

    def _get_wheel_geom_data(self): #vers 2
        """Return geometry data for the current wheel type from wheels.DFF.
        Matches _wheel_type exactly (e.g. wheel_saloon_l0), falls back to first wheel."""
        m = self._wheels_model
        if not m or not m.geometries: return None
        fname = {i: (f.name.lower() if f.name else '') for i,f in enumerate(m.frames)}
        wtype = getattr(self, '_wheel_type', 'wheel_saloon_l0').lower()

        def _geom_data(geom):
            return (
                [(v.x,v.y,v.z) for v in geom.vertices],
                [(n.x,n.y,n.z) for n in geom.normals] if geom.normals else [],
                [(u.u,u.v) for u in geom.uv_layers[0]] if geom.uv_layers else [],
                [(t.v1,t.v2,t.v3,t.material_id) for t in geom.triangles],
                geom.materials,
                [(c.r,c.g,c.b,c.a) for c in geom.colors] if geom.colors else [],
                getattr(geom,'flags',0)
            )

        # Pass 1: exact match on full wheel type name
        for i, geom in enumerate(m.geometries):
            atomic = next((a for a in m.atomics if a.geometry_index==i), None)
            if not atomic: continue
            name = fname.get(atomic.frame_index,'')
            if name == wtype:
                return _geom_data(geom)

        # Pass 2: wheel type contained in frame name (e.g. wheel_saloon_l0 in wheel_saloon_l0_dam)
        for i, geom in enumerate(m.geometries):
            atomic = next((a for a in m.atomics if a.geometry_index==i), None)
            if not atomic: continue
            name = fname.get(atomic.frame_index,'')
            if wtype in name and not name.endswith('_dam'):
                return _geom_data(geom)

        # Pass 3: base type without _l0 suffix
        base = wtype.replace('_l0','').replace('_lo','')
        for i, geom in enumerate(m.geometries):
            atomic = next((a for a in m.atomics if a.geometry_index==i), None)
            if not atomic: continue
            name = fname.get(atomic.frame_index,'')
            if base in name and not name.endswith('_dam'):
                return _geom_data(geom)

        # Pass 4: first non-damaged wheel geometry as fallback
        for i, geom in enumerate(m.geometries):
            atomic = next((a for a in m.atomics if a.geometry_index==i), None)
            if not atomic: continue
            name = fname.get(atomic.frame_index,'')
            if 'wheel' in name and not name.endswith('_dam'):
                return _geom_data(geom)
        return None

    def set_assembly_mode(self, enabled: bool): #vers 1
        self._assembly_mode = enabled; self.update()

    def set_show_lod(self, enabled: bool): #vers 1
        self._show_lod = enabled; self.update()

    def _draw_assembly(self): #vers 2
        if not OPENGL_AVAILABLE: return
        for entry in getattr(self,'_all_geoms',[]):
            verts,norms,uvs,tris,mats,prelit = entry[:6]
            geom_flags = entry[6] if len(entry) > 6 else self._current_geom_flags
            old_v,old_n,old_u,old_t,old_m,old_p,old_f = (
                self._vertices,self._normals,self._uvs,
                self._triangles,self._materials,self._prelit,
                getattr(self,'_current_geom_flags',0))
            self._vertices=verts; self._normals=norms; self._uvs=uvs
            self._triangles=tris; self._materials=mats; self._prelit=prelit
            self._current_geom_flags=geom_flags
            if   self._mode=='wireframe': self._draw_wireframe()
            elif self._mode=='solid':     self._draw_solid()
            elif self._mode=='textured':  self._draw_textured()
            (self._vertices,self._normals,self._uvs,
             self._triangles,self._materials,self._prelit,
             self._current_geom_flags) = (old_v,old_n,old_u,old_t,old_m,old_p,old_f)

    def set_prelight(self, v: bool): #vers 1
        self._use_prelight = v; self.update()

    def set_light_dir(self, x, y, z): #vers 1
        self._light_dir = (x, y, z, 0.0)
        if OPENGL_AVAILABLE and self.isVisible():
            self.makeCurrent(); self._setup_lighting(); self.doneCurrent()
        self.update()

    def set_ambient(self, v: float): #vers 1
        self._ambient = v
        if OPENGL_AVAILABLE and self.isVisible():
            self.makeCurrent(); self._setup_lighting(); self.doneCurrent()
        self.update()

    def set_diffuse(self, v: float): #vers 1
        self._diffuse = v
        if OPENGL_AVAILABLE and self.isVisible():
            self.makeCurrent(); self._setup_lighting(); self.doneCurrent()
        self.update()

    def reset_camera(self): #vers 1
        self._yaw=45.0; self._pitch=25.0; self._pan_x=0.0; self._pan_y=0.0
        self._auto_fit(); self.update()

    def set_view_lock(self, locked: bool, label: str = "", yaw: float = None,
                       pitch: float = None, projection: str = 'perspective'): #vers 1
        """Lock/unlock this pane to a fixed preset view (3ds Max style Top/
        Front/Side/Perspective panes). Locked panes cannot rotate-drag and
        use parallel (ortho) projection; unlocked/perspective panes behave
        as before (free rotate)."""
        self._view_locked = locked
        self._view_label  = label
        self._projection  = projection
        if yaw   is not None: self._yaw   = yaw
        if pitch is not None: self._pitch = pitch
        if label:
            self._label_widget.setText(label)
            self._label_widget.adjustSize()
            self._label_widget.show()
        else:
            self._label_widget.hide()
        self.resizeGL(self.width(), self.height())
        self.update()

    def mousePressEvent(self, event): #vers 2
        self._last_pos = event.pos()
        if event.button() == Qt.MouseButton.LeftButton:
            mode = getattr(self, '_select_mode', 'object')
            if mode == 'object':
                return
            mx, my = event.pos().x(), event.pos().y()
            if mode == 'vertex':
                key = self._pick_vertex(mx, my)
            elif mode == 'edge':
                key = self._pick_edge(mx, my)
            else:  # 'face' or 'poly'
                key = self._pick_face(mx, my)
            if key is not None:
                self._apply_selection_click(mode, key, event.modifiers())
                self.update()
            elif not (event.modifiers() & (Qt.KeyboardModifier.ControlModifier |
                                           Qt.KeyboardModifier.ShiftModifier)):
                # Clicked empty space with no modifier — clear selection
                self._selected_set_for_mode(mode).clear()
                self._notify_selection_changed()
                self.update()

    def mouseMoveEvent(self, event): #vers 2
        dx = event.pos().x() - self._last_pos.x()
        dy = event.pos().y() - self._last_pos.y()
        if event.buttons() & Qt.MouseButton.RightButton and not self._view_locked:
            self._yaw   += dx * 0.5
            self._pitch += dy * 0.5
        elif event.buttons() & Qt.MouseButton.MiddleButton:
            scale = self._dist * 0.002
            self._pan_x += dx * scale
            self._pan_y -= dy * scale
        self._last_pos = event.pos(); self.update()

    def mouseReleaseEvent(self, event): #vers 1
        self._last_pos = event.pos()

    def wheelEvent(self, event): #vers 2
        f = 0.85 if event.angleDelta().y() > 0 else 1.15
        self._dist = max(0.1, min(50000.0, self._dist*f))
        if self._projection == 'ortho':
            self.resizeGL(self.width(), self.height())
        self.update()

    # - Model Workshop compatibility methods
    # These map COL3DViewport API onto DFFViewport equivalents

    def zoom_in(self): #vers 2
        self._dist = max(0.1, self._dist * 0.8)
        if self._projection == 'ortho': self.resizeGL(self.width(), self.height())
        self.update()

    def zoom_out(self): #vers 2
        self._dist = min(50000.0, self._dist * 1.25)
        if self._projection == 'ortho': self.resizeGL(self.width(), self.height())
        self.update()

    def reset_view(self): #vers 1
        self._yaw=45.0; self._pitch=25.0; self._pan_x=0.0; self._pan_y=0.0
        self._auto_fit(); self.update()

    def fit_to_window(self): #vers 1
        self._auto_fit(); self.update()

    def pan(self, dx, dy): #vers 1
        scale = self._dist * 0.002
        self._pan_x += dx * scale; self._pan_y -= dy * scale; self.update()

    def set_show_mesh(self, v: bool): #vers 1
        # DFFViewport always shows mesh — no-op for compatibility
        self.update()

    def set_backface(self, v: bool): #vers 1
        self._backface_cull = not v; self.update()

    def flip_vertical(self): #vers 1
        self._vertices = [(x, -y, z) for x, y, z in self._vertices]; self.update()

    def flip_horizontal(self): #vers 1
        self._vertices = [(-x, y, z) for x, y, z in self._vertices]; self.update()

    def rotate_cw(self): #vers 1
        self._yaw = (self._yaw + 90) % 360; self.update()

    def rotate_ccw(self): #vers 1
        self._yaw = (self._yaw - 90) % 360; self.update()

    def set_current_model(self, model, index=0): #vers 1
        """Load a DFFModel directly — compatibility with COL3DViewport API."""
        if not model or not model.geometries: return
        g = model.geometries[min(index, len(model.geometries)-1)]
        self.load_geometry(g, g.materials)

    def set_checkerboard_background(self): #vers 2
        """No checkerboard rendering in GL mode — clear any colour override back to theme."""
        self._bg_color_override = None
        self.update()

    def set_background_color(self, color): #vers 2
        """Set an explicit background colour, overriding the theme colour.

        color: (r, g, b) tuple 0-255, or a QColor.
        """
        if hasattr(color, 'getRgb'):
            r, g, b, _ = color.getRgb()
            self._bg_color_override = (r, g, b)
        else:
            self._bg_color_override = tuple(color[:3])
        self.update()

    def _refresh(self): #vers 1
        self.update()


class VehicleViewport(DFFViewport):
    """DFFViewport + vehicle animation (doors, rotors, wheels)."""

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._anim_enabled      = False
        self._anim_timer        = None
        self._anim_speed        = 1.0
        self._anim_rates        = {'moving_rotor': 360.0, 'moving_rotor2': 360.0,
                                   'prop': 360.0, 'misc_a': 180.0, 'misc_b': 180.0}
        self._anim_frame_angles = {}
        self._anim_door_open    = {}
        self._wheel_heading     = 0.0
        self._dragging          = False
        from PyQt6.QtCore import Qt
        self._drag_btn          = Qt.MouseButton.NoButton

    def set_animation(self, enabled: bool): #vers 1
        self._anim_enabled = enabled
        if enabled:
            if self._anim_timer is None:
                from PyQt6.QtCore import QTimer
                self._anim_timer = QTimer(self)
                self._anim_timer.timeout.connect(self._anim_tick)
            self._anim_timer.start(33)
        else:
            if self._anim_timer: self._anim_timer.stop()
            self.update()

    def _anim_tick(self): #vers 1
        if not self._anim_enabled or not self._assembly_mode: return
        for fname, rate in self._anim_rates.items():
            cur = self._anim_frame_angles.get(fname, 0.0)
            self._anim_frame_angles[fname] = (cur + rate * self._anim_speed / 30.0) % 360.0
        self._rebuild_anim_geoms()

    def _rebuild_anim_geoms(self): #vers 1
        m = getattr(self, '_dff_model', None)
        if not m: return
        self.load_all_geometries(
            m.geometries, [g.materials for g in m.geometries],
            m.frames, m.atomics,
            damaged=getattr(self, '_damaged', False))

    def toggle_door(self, door_name: str): #vers 1
        self._anim_door_open[door_name] = not self._anim_door_open.get(door_name, False)
        self._rebuild_anim_geoms()

    def _get_anim_rotation(self, frame_name: str): #vers 1
        name = frame_name.lower()
        for key in ('moving_rotor', 'moving_rotor2', 'prop', 'misc_a', 'misc_b'):
            if key in name:
                angle = self._anim_frame_angles.get(key, 0.0)
                ca=math.cos(math.radians(angle)); sa=math.sin(math.radians(angle))
                return [ca,-sa,0, sa,ca,0, 0,0,1]
        for key in ('door_lf','door_rf','door_lr','door_rr','bonnet','boot'):
            if key in name:
                is_open = self._anim_door_open.get(name, False)
                angle = 70.0 if is_open else 0.0
                ca=math.cos(math.radians(angle)); sa=math.sin(math.radians(angle))
                return [1,0,0, 0,ca,-sa, 0,sa,ca]
        return None

    def set_animation_speed(self, speed: float): #vers 1
        self._anim_speed = max(0.1, speed)

    def set_wheel_heading(self, angle_deg: float): #vers 1
        self._wheel_heading = angle_deg
        if getattr(self,'_assembly_mode',False) and getattr(self,'_dff_model',None):
            m = self._dff_model
            self.load_all_geometries(m.geometries,[g.materials for g in m.geometries],
                                     m.frames, m.atomics, getattr(self,'_damaged',False))

    def load_wheels_dff(self, path: str, wheel_type: str = 'wheel_saloon_l0'): #vers 1
        try:
            try:
                from apps.methods.dff_parser import load_dff
            except ImportError:
                from apps.components.Vehicle_Workshop.depends.dff_parser import load_dff
            self._wheels_model = load_dff(path)
            self._wheel_type   = wheel_type
        except Exception as e:
            print(f'[VehicleViewport] wheels.DFF load fail: {e}')

    def _get_wheel_geom_data(self): #vers 1
        m = getattr(self, '_wheels_model', None)
        if not m: return None
        wtype = getattr(self, '_wheel_type', 'wheel_saloon_l0').lower()
        for a in m.atomics:
            fi = a.frame_index
            fname = (m.frames[fi].name or '').lower() if fi < len(m.frames) else ''
            if fname == wtype:
                g = m.geometries[a.geometry_index]
                return (
                    [(v.x,v.y,v.z) for v in g.vertices],
                    [(n.x,n.y,n.z) for n in g.normals] if g.normals else [],
                    [(u.u,u.v) for u in g.uv_layers[0]] if g.uv_layers else [],
                    [(t.v1,t.v2,t.v3,t.material_id) for t in g.triangles],
                    g.materials,
                    [(c.r,c.g,c.b,c.a) for c in g.colors] if g.colors else []
                )
        return None

    def mousePressEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        self._last_pos = event.pos(); self._dragging=True; self._drag_btn=event.button()

    def mouseMoveEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        if not self._dragging: return
        dx = event.pos().x()-self._last_pos.x()
        dy = event.pos().y()-self._last_pos.y()
        if self._drag_btn == Qt.MouseButton.LeftButton:
            self._yaw   += dx*0.5
            self._pitch  = max(-89, min(89, self._pitch+dy*0.5))
        elif self._drag_btn == Qt.MouseButton.MiddleButton:
            s = self._dist/500.0
            self._pan_x += dx*s; self._pan_y -= dy*s
        self._last_pos = event.pos(); self.update()

    def mouseReleaseEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        self._dragging=False; self._drag_btn=Qt.MouseButton.NoButton

    def wheelEvent(self, event): #vers 1
        f = 0.88 if event.angleDelta().y()>0 else 1.13
        self._dist = max(0.1, min(50000.0, self._dist*f)); self.update()
