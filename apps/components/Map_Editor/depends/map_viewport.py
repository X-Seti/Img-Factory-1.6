#!/usr/bin/env python3
# apps/components/Map_Editor/depends/map_viewport.py - Version: 1
# X-Seti - Jul 24 2026 - Map Workshop - world instance viewport
#
# MapViewport renders the placements loaded by GTAWorldLoader (position
# markers for now - one coloured cube per instance, plus a ground grid
# for spatial reference) using the same camera/projection architecture
# as apps/methods/dff_viewport.py's DFFViewport: yaw/pitch/pan/dist
# camera, ortho vs perspective projection, and set_view_lock() so this
# same class can be locked to Top/Side/Front views or left free-rotating
# for a Perspective/3D pane - exactly the pattern Model Workshop's
# existing 4-pane (Top/Front/Side/Perspective) quad viewport already
# uses for DFFViewport, just applied here to world data instead of a
# single model's geometry.
#
# Loading actual per-instance DFF/TXD geometry (rather than plain marker
# cubes) is a later step - this establishes the viewport/camera/pane
# foundation first, which is independent of that and can be built and
# tested on its own.

import math
from typing import List, Optional

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


class MapViewport(QOpenGLWidget if OPENGL_AVAILABLE else QWidget):
    """OpenGL viewport for a GTA world's placed object instances.
    Same camera/pane-lock architecture as DFFViewport - usable as a
    free-rotating Perspective/3D pane or locked to Top/Side/Front for a
    multi-pane layout."""

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.setMinimumSize(200, 200)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

        # World data - list of (x, y, z, model_name) tuples. Kept as
        # plain tuples rather than holding a reference to the loader's
        # own IPLInstance objects, so this viewport doesn't need to know
        # anything about gta_dat_parser's data classes directly.
        self._instances: List[tuple] = []
        self._vertex_array = None   # numpy array, built by set_instances
        self._gizmo_pos = None      # (x, y, z) world position, or None
        self._culls = []            # list of cull dicts (center_x/y/z, width, height)
        self._show_culls = False
        self._model_cache = None    # ModelCache, set via set_model_cache()
        self._render_mode = 'solid' # 'solid' | 'semi' | 'wireframe' - only affects
                                    # instances with real loaded geometry; instances
                                    # falling back to point rendering are unaffected
        self._marker_size = 1.0

        # Camera - identical scheme to DFFViewport
        self._dist  = 200.0
        self._yaw   = 45.0
        self._pitch = 25.0
        self._pan_x = 0.0
        self._pan_y = 0.0
        self._last_pos = QPoint()

        # Pane lock (Top/Side/Front/Perspective) - see set_view_lock
        self._view_locked = False
        self._view_label  = ""
        self._projection  = 'perspective'

        self._show_grid = True
        self._bg_color_override = None
        self.app_settings = None

        self._label_widget = QLabel(self)
        # Was setStyleSheet("color: palette(text); background:
        # transparent; font-weight: bold;") - any stylesheet
        # application, even one specifying a transparent background,
        # sets Qt's WA_StyledBackground attribute, forcing the full
        # style-based paint path rather than the lightweight "alien
        # widget" compositing normally used for child widgets on a
        # QOpenGLWidget - a known trigger for repeated "paintEngine
        # should no longer be called" warnings (Keith's report).
        # QPalette + a bold font set directly avoids the stylesheet
        # machinery entirely while keeping the exact same appearance
        # and all existing functionality (right-click menu still
        # works, since this is still a real, interactive QLabel).
        pal = self._label_widget.palette()
        pal.setColor(self._label_widget.foregroundRole(), pal.color(pal.ColorRole.Text))
        self._label_widget.setPalette(pal)
        label_font = self._label_widget.font()
        label_font.setBold(True)
        self._label_widget.setFont(label_font)
        self._label_widget.setAutoFillBackground(False)
        self._label_widget.hide()

    def set_instances(self, instances): #vers 2
        """Feed in world placements to render - accepts a list of
        IPLInstance-like objects (anything with pos_x/pos_y/pos_z and
        model_name attributes) or plain (x,y,z,name) tuples.

        Precomputes a flat numpy vertex array ONCE here, rather than
        rebuilding per-instance Python-side data on every paintGL call -
        the array feeds a single glDrawArrays call (see _draw_instances),
        which is what actually fixes the reported slowness: the old
        approach looped in Python calling glVertex3f per vertex of a
        full 6-quad cube for every instance (over 1.2 million individual
        GL calls per frame at 51,711 instances) - this reduces that to
        one draw call for the whole set, at the cost of a simpler point-
        based visual instead of cubes (real per-instance DFF/TXD
        geometry is the actual long-term fix - this keeps the same
        'plot x,y,z' concept Keith described, just fast)."""
        out = []
        for inst in instances:
            if hasattr(inst, 'pos_x'):
                out.append((inst.pos_x, inst.pos_y, inst.pos_z,
                            getattr(inst, 'model_name', '')))
            else:
                out.append(tuple(inst))
        self._instances = out
        # Full original instances (or None per-entry for plain tuples
        # with no real object behind them), index-aligned with
        # self._instances/self._vertex_array - lets picking map a hit
        # back to a complete IPLInstance for opening the object handler.
        self._full_instances = [inst if hasattr(inst, 'pos_x') else None
                                for inst in instances]

        # GTA is Z-up; this viewport's OpenGL space is Y-up (matches the
        # old per-cube glTranslatef(x, z, y) convention)
        try:
            import numpy as np
            self._vertex_array = (np.array([(x, z, y) for x, y, z, _n in out],
                                           dtype=np.float32) if out else None)
        except Exception:
            self._vertex_array = None

        self._auto_fit()
        self.update()

    def _auto_fit(self): #vers 2
        """Frame the camera distance/pan to cover all loaded instances.

        Was diag * 0.6 (the full 3D bounding-box diagonal, scaled
        down) - found via testing that this can undershoot the actual
        half-extent needed whenever one axis span is much larger than
        another (a real test dataset spanning 45 units in X and 55 in
        Y produced an ortho half_h smaller than half the Y span,
        pushing instances outside the frustum entirely - this was also
        why real-data viewport picking initially appeared broken, when
        the actual issue was objects being auto-framed out of view).
        Using the largest single-axis span with a safety margin
        guarantees the full bounding box fits regardless of aspect
        ratio skew."""
        if not self._instances:
            return
        xs = [i[0] for i in self._instances]
        ys = [i[1] for i in self._instances]
        zs = [i[2] for i in self._instances]
        max_span = max(max(xs) - min(xs), max(ys) - min(ys), max(zs) - min(zs))
        self._dist  = max(max_span * 1.1, 5.0)
        self._pan_x = -(max(xs)+min(xs))/2
        self._pan_y = -(max(ys)+min(ys))/2
        try:
            if hasattr(self, 'resizeGL') and (not hasattr(self, 'isValid') or self.isValid()):
                self.resizeGL(self.width(), self.height())
        except Exception:
            pass
        self.update()

    def set_view_lock(self, locked: bool, label: str = "", yaw: float = None,
                       pitch: float = None, projection: str = 'perspective'): #vers 1
        """Lock/unlock this pane to a fixed preset view (Top/Side/Front/
        Perspective) - identical contract to DFFViewport.set_view_lock,
        so the same pane-menu/persistence code can drive either."""
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
        try:
            if hasattr(self, 'resizeGL') and (not hasattr(self, 'isValid') or self.isValid()):
                self.resizeGL(self.width(), self.height())
        except Exception:
            pass
        self.update()

    def _get_bg_color(self): #vers 1
        if self._bg_color_override:
            r, g, b = self._bg_color_override
            return QColor(r, g, b)
        pal = self.palette()
        return pal.color(pal.ColorRole.Base)

    def set_bg_color_override(self, rgb_or_none): #vers 1
        """Set (or clear, if None) a background colour override -
        rgb_or_none is an (r,g,b) 0-255 tuple, or None to use the
        palette default. Existed as bare state with no setter before."""
        self._bg_color_override = rgb_or_none
        self.update()

    def initializeGL(self): #vers 1
        if not OPENGL_AVAILABLE: return
        bg = self._get_bg_color()
        glClearColor(bg.redF(), bg.greenF(), bg.blueF(), 1.0)
        glEnable(GL_DEPTH_TEST)

    def resizeGL(self, w, h): #vers 1
        if not OPENGL_AVAILABLE: return
        glViewport(0, 0, max(1, w), max(1, h))
        glMatrixMode(GL_PROJECTION); glLoadIdentity()
        aspect = max(1, w) / max(1, h)
        if self._projection == 'ortho':
            half_h = max(0.01, self._dist * 0.5)
            glOrtho(-half_h*aspect, half_h*aspect, -half_h, half_h,
                    -100000.0, 100000.0)
            self._ortho_half_h = half_h
        else:
            gluPerspective(45.0, aspect, 0.1, 100000.0)
        glMatrixMode(GL_MODELVIEW)
        self._label_widget.move(4, 2)

    def paintGL(self): #vers 1
        if not OPENGL_AVAILABLE: return
        bg = self._get_bg_color()
        glClearColor(bg.redF(), bg.greenF(), bg.blueF(), 1.0)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity()

        if self._projection == 'ortho':
            # Top: look straight down; Side: look along X; Front: along Y
            # pan_x/pan_y are consistently "negative of the desired
            # centre" everywhere else (perspective's glTranslatef,
            # _auto_fit, MapWorkshop._center_on_instance) - negate back
            # here since gluLookAt's target parameter needs the actual
            # (positive) world position, not the negated pan value.
            # Found via viewport-picking testing: without this negation,
            # a real dataset spanning non-trivial distance from the
            # origin projected completely outside the visible frustum,
            # since the camera was actually centred on the mirror-image
            # location of where the instances really were.
            if self._view_label == 'Top':
                gluLookAt(-self._pan_x, self._dist, -self._pan_y,
                          -self._pan_x, 0, -self._pan_y, 0, 0, -1)
            elif self._view_label == 'Side':
                gluLookAt(self._dist, -self._pan_y, -self._pan_x,
                          0, -self._pan_y, -self._pan_x, 0, 1, 0)
            else:  # Front
                gluLookAt(-self._pan_x, -self._pan_y, self._dist,
                          -self._pan_x, -self._pan_y, 0, 0, 1, 0)
        else:
            glTranslatef(0, 0, -self._dist)
            glRotatef(self._pitch, 1, 0, 0)
            glRotatef(self._yaw,   0, 1, 0)
            glTranslatef(self._pan_x, 0, self._pan_y)

        if self._show_grid:
            self._draw_grid()
        self._draw_instances()
        if self._gizmo_pos is not None:
            self._draw_gizmo()
        if self._show_culls and self._culls:
            self._draw_cull_boxes()

    def set_gizmo_position(self, pos): #vers 1
        """Show (or hide, if pos is None) an XYZ axis gizmo at a world
        position - used to mark the currently-selected instance from
        Instance List."""
        self._gizmo_pos = pos
        self.update()

    def set_cull_boxes(self, culls, visible): #vers 1
        """Set the loaded cull zone data and whether to draw it -
        culls is a list of dicts with center_x/y/z, width, height (see
        GTAWorldLoader.culls / gta_dat_parser._parse_cull)."""
        self._culls = culls or []
        self._show_culls = visible
        self.update()

    def _draw_cull_boxes(self): #vers 1
        """Draw a wireframe box for every loaded cull zone.

        Interpretation of the parsed fields (center_x/y/z, width,
        height) is an assumption, not verified against real behaviour
        (unlike the binary IPL work elsewhere in this project, which
        was empirically checked against real sample data) - treats
        'width' as the full horizontal (X and Y) extent and 'height'
        as the full vertical (Z) extent, centred at (center_x,
        center_y, center_z). The two 'unknown' fields _parse_cull
        also captures aren't used here at all."""
        glColor3f(1.0, 0.85, 0.2)
        glLineWidth(1.5)
        for c in self._culls:
            cx, cy, cz = c.get('center_x', 0.0), c.get('center_y', 0.0), c.get('center_z', 0.0)
            hw = c.get('width', 0.0) / 2.0
            hh = c.get('height', 0.0) / 2.0
            gx, gy, gz = cx, cz, cy   # same Z-up (GTA) -> Y-up swap as instances
            glPushMatrix()
            glTranslatef(gx, gy, gz)
            glBegin(GL_LINE_LOOP)
            glVertex3f(-hw, -hh, -hw); glVertex3f(hw, -hh, -hw)
            glVertex3f(hw, -hh, hw);  glVertex3f(-hw, -hh, hw)
            glEnd()
            glBegin(GL_LINE_LOOP)
            glVertex3f(-hw, hh, -hw); glVertex3f(hw, hh, -hw)
            glVertex3f(hw, hh, hw);  glVertex3f(-hw, hh, hw)
            glEnd()
            glBegin(GL_LINES)
            for dx, dz in ((-hw, -hw), (hw, -hw), (hw, hw), (-hw, hw)):
                glVertex3f(dx, -hh, dz); glVertex3f(dx, hh, dz)
            glEnd()
            glPopMatrix()
        glLineWidth(1.0)

    def _draw_gizmo(self, size: float = 3.0): #vers 2
        """Draw a simple 3-axis (red=world X, green=world Y, blue=
        world Z) gizmo at self._gizmo_pos, marking the currently-
        selected instance - not an interactive manipulation handle
        yet, just a visual marker, drawn on top of the instance
        points.

        Axis lines are drawn in this viewport's own OpenGL-local space
        (Y-up), but coloured/labelled by GTA's world axes (Z-up) - so
        green (world Y) draws along local Z, and blue (world Z) draws
        along local Y (straight up), matching the same swap already
        applied to every instance position elsewhere in this class."""
        x, y, z = self._gizmo_pos
        gx, gy, gz = x, z, y   # same Z-up (GTA) -> Y-up (this viewport) swap as instances
        glPushMatrix()
        glTranslatef(gx, gy, gz)
        glLineWidth(2.0)
        glBegin(GL_LINES)
        glColor3f(1.0, 0.2, 0.2); glVertex3f(0, 0, 0); glVertex3f(size, 0, 0)   # world X
        glColor3f(0.2, 1.0, 0.2); glVertex3f(0, 0, 0); glVertex3f(0, 0, size)   # world Y -> local Z
        glColor3f(0.3, 0.5, 1.0); glVertex3f(0, 0, 0); glVertex3f(0, size, 0)   # world Z -> local Y
        glEnd()
        glLineWidth(1.0)
        glPopMatrix()

    def _draw_grid(self, size: int = 500, step: int = 50): #vers 1
        glColor3f(0.4, 0.4, 0.4)
        glBegin(GL_LINES)
        for i in range(-size, size + 1, step):
            glVertex3f(i, 0, -size); glVertex3f(i, 0, size)
            glVertex3f(-size, 0, i); glVertex3f(size, 0, i)
        glEnd()

    def set_model_cache(self, cache): #vers 1
        """Set the ModelCache used to resolve real DFF/TXD geometry for
        instances - without this, every instance falls back to point
        rendering (the previous, only behaviour)."""
        self._model_cache = cache
        self.update()

    def set_render_mode(self, mode): #vers 1
        """'solid' (default) | 'semi' (alpha-blended) | 'wireframe' -
        only affects instances with real loaded geometry; instances
        falling back to point rendering are unaffected by this."""
        self._render_mode = mode
        self.update()

    def _draw_instances(self): #vers 4
        """Draw every loaded instance - real DFF geometry (via
        self._model_cache) where a model is available and loadable,
        falling back to the existing batched point rendering (see the
        long-standing note on _draw_instances vers 3 about why plain
        glBegin/glEnd points, not glVertexPointer/glDrawArrays) for
        everything else - unindexed models, parse failures, or no
        ModelCache set at all.

        Known limitation, not solved here: real geometry is drawn with
        one glPushMatrix/transform/draw/glPopMatrix sequence PER
        INSTANCE, which is far more expensive per-object than the
        single batched glBegin/glEnd block point rendering uses. For a
        world where most instances have loadable geometry (the common
        case once a real IMG set is indexed), this could get slow at
        GTASOL's ~50k instance scale - proper visibility culling
        (distance/frustum-based) would be the next step if that
        happens in practice, not attempted yet."""
        cache = getattr(self, '_model_cache', None)
        full_instances = getattr(self, '_full_instances', None)

        if cache is None or not full_instances:
            # No cache set, or this pane's instances aren't real
            # IPLInstance objects (plain tuples with nothing to look
            # up geometry for) - same behaviour as before.
            self._draw_instances_as_points(getattr(self, '_vertex_array', None))
            return

        fallback_points = []   # (gx, gy, gz) for instances with no usable geometry
        any_mesh_drawn = False
        for inst in full_instances:
            if inst is None:
                continue
            geo = cache.get_geometry(inst.model_name)
            if geo is None or not geo.geometries:
                fallback_points.append((inst.pos_x, inst.pos_z, inst.pos_y))
                continue
            self._draw_instance_mesh(inst, geo)
            any_mesh_drawn = True

        if fallback_points:
            glColor3f(0.9, 0.5, 0.2)
            glPointSize(max(1.0, min(8.0, self._marker_size)))
            glBegin(GL_POINTS)
            for gx, gy, gz in fallback_points:
                glVertex3f(gx, gy, gz)
            glEnd()

    def _draw_instances_as_points(self, va): #vers 1
        """The pre-mesh-rendering fallback path, unchanged - draws
        every instance as a single batched point (see _draw_instances'
        docstring for why glBegin/glEnd, not client-side arrays)."""
        glColor3f(0.9, 0.5, 0.2)
        glPointSize(max(1.0, min(8.0, self._marker_size)))
        if va is not None and len(va):
            glBegin(GL_POINTS)
            for x, y, z in va.tolist():
                glVertex3f(x, y, z)
            glEnd()
            return
        if self._instances:
            glBegin(GL_POINTS)
            for x, y, z, _name in self._instances:
                glVertex3f(x, z, y)
            glEnd()

    def _draw_instance_mesh(self, inst, geo): #vers 1
        """Draw one instance's real DFF geometry, transformed by its
        position/rotation(quaternion)/scale, respecting the current
        render mode (solid/semi/wireframe). Untextured (flat grey) for
        now - texture binding via self._model_cache.get_textures() is
        the next piece, not done yet."""
        import numpy as np
        gx, gy, gz = inst.pos_x, inst.pos_z, inst.pos_y   # same Z-up->Y-up swap as points

        x, y, z, w = inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w
        xx, yy, zz = x*x, y*y, z*z
        xy, xz, yz = x*y, x*z, y*z
        wx, wy, wz = w*x, w*y, w*z
        rot3 = np.array([
            [1-2*(yy+zz), 2*(xy-wz),   2*(xz+wy)],
            [2*(xy+wz),   1-2*(xx+zz), 2*(yz-wx)],
            [2*(xz-wy),   2*(yz+wx),   1-2*(xx+yy)],
        ])
        # Embed the 3x3 rotation into a 4x4 matrix, column-major (OpenGL's
        # own convention for glMultMatrixf) - column i of rot3 becomes
        # the first 3 entries of column i here.
        mat = np.identity(4, dtype=np.float32)
        mat[0:3, 0:3] = rot3
        gl_matrix = mat.flatten(order='F')   # column-major flattening

        glPushMatrix()
        glTranslatef(gx, gy, gz)
        glMultMatrixf(gl_matrix)
        glScalef(inst.scale_x, inst.scale_z, inst.scale_y)   # same axis swap as position

        if self._render_mode == 'wireframe':
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
            glDisable(GL_BLEND)
        elif self._render_mode == 'semi':
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
            glEnable(GL_BLEND)
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        else:  # solid
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
            glDisable(GL_BLEND)

        alpha = 0.45 if self._render_mode == 'semi' else 1.0
        glColor4f(0.65, 0.65, 0.7, alpha)

        for geometry in geo.geometries:
            if not geometry.triangles or not geometry.vertices:
                continue
            glBegin(GL_TRIANGLES)
            for tri in geometry.triangles:
                for vi in (tri.v1, tri.v2, tri.v3):
                    if 0 <= vi < len(geometry.vertices):
                        v = geometry.vertices[vi]
                        glVertex3f(v.x, v.y, v.z)
            glEnd()

        if self._render_mode == 'wireframe':
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)   # restore default
        glPopMatrix()

    def _draw_cube(self, s): #vers 1
        h = s / 2.0
        glBegin(GL_QUADS)
        # Top/bottom
        glVertex3f(-h, h, -h); glVertex3f(h, h, -h); glVertex3f(h, h, h); glVertex3f(-h, h, h)
        glVertex3f(-h, -h, -h); glVertex3f(-h, -h, h); glVertex3f(h, -h, h); glVertex3f(h, -h, -h)
        # Front/back
        glVertex3f(-h, -h, h); glVertex3f(h, -h, h); glVertex3f(h, h, h); glVertex3f(-h, h, h)
        glVertex3f(-h, -h, -h); glVertex3f(-h, h, -h); glVertex3f(h, h, -h); glVertex3f(h, -h, -h)
        # Left/right
        glVertex3f(-h, -h, -h); glVertex3f(-h, -h, h); glVertex3f(-h, h, h); glVertex3f(-h, h, -h)
        glVertex3f(h, -h, -h); glVertex3f(h, h, -h); glVertex3f(h, h, h); glVertex3f(h, -h, h)
        glEnd()

    def _view_matrix(self): #vers 1
        """Replicate the exact camera transform paintGL applies, as a
        4x4 numpy matrix - standard gluLookAt-equivalent math for ortho
        views, standard translate/rotate composition for perspective.
        Verified via standalone numeric tests against known cases
        (centred point projects to NDC origin, offset points land on
        the expected side) earlier this session."""
        import numpy as np

        def look_at(eye, center, up):
            eye, center, up = (np.array(eye, dtype=float), np.array(center, dtype=float),
                              np.array(up, dtype=float))
            f = center - eye; f = f / np.linalg.norm(f)
            s = np.cross(f, up); s = s / np.linalg.norm(s)
            u = np.cross(s, f)
            m = np.identity(4)
            m[0, 0:3] = s; m[1, 0:3] = u; m[2, 0:3] = -f
            m[0, 3] = -np.dot(s, eye); m[1, 3] = -np.dot(u, eye); m[2, 3] = np.dot(f, eye)
            return m

        if self._projection == 'ortho':
            # Same negation as paintGL's corrected ortho gluLookAt calls -
            # pan_x/pan_y are "negative of desired centre" everywhere
            # else in this class, so negate back here too for the
            # actual world-space look-at target.
            if self._view_label == 'Top':
                eye = (-self._pan_x, self._dist, -self._pan_y)
                center = (-self._pan_x, 0, -self._pan_y)
                up = (0, 0, -1)
            elif self._view_label == 'Side':
                eye = (self._dist, -self._pan_y, -self._pan_x)
                center = (0, -self._pan_y, -self._pan_x)
                up = (0, 1, 0)
            else:  # Front
                eye = (-self._pan_x, -self._pan_y, self._dist)
                center = (-self._pan_x, -self._pan_y, 0)
                up = (0, 1, 0)
            return look_at(eye, center, up)

        def rot_x(deg):
            r = math.radians(deg); c, s = math.cos(r), math.sin(r)
            m = np.identity(4); m[1, 1] = c; m[1, 2] = -s; m[2, 1] = s; m[2, 2] = c
            return m

        def rot_y(deg):
            r = math.radians(deg); c, s = math.cos(r), math.sin(r)
            m = np.identity(4); m[0, 0] = c; m[0, 2] = s; m[2, 0] = -s; m[2, 2] = c
            return m

        def translate(x, y, z):
            m = np.identity(4); m[0, 3] = x; m[1, 3] = y; m[2, 3] = z
            return m

        return (translate(0, 0, -self._dist) @ rot_x(self._pitch) @ rot_y(self._yaw) @
               translate(self._pan_x, 0, self._pan_y))

    def _projection_matrix(self): #vers 1
        """Replicate the exact projection resizeGL sets up - same
        parameters (fovy/near/far for perspective, or the last-used
        ortho half_h) so picking math matches what's actually
        rendered."""
        import numpy as np
        w, h = max(1, self.width()), max(1, self.height())
        aspect = w / h
        if self._projection == 'ortho':
            half_h = getattr(self, '_ortho_half_h', max(0.01, self._dist * 0.5))
            l, r = -half_h * aspect, half_h * aspect
            b, t = -half_h, half_h
            n, f = -100000.0, 100000.0
            m = np.identity(4)
            m[0, 0] = 2 / (r - l); m[1, 1] = 2 / (t - b); m[2, 2] = -2 / (f - n)
            m[0, 3] = -(r + l) / (r - l); m[1, 3] = -(t + b) / (t - b); m[2, 3] = -(f + n) / (f - n)
            return m
        fovy, near, far = 45.0, 0.1, 100000.0
        f_ = 1.0 / math.tan(math.radians(fovy) / 2)
        m = np.zeros((4, 4))
        m[0, 0] = f_ / aspect; m[1, 1] = f_
        m[2, 2] = (far + near) / (near - far); m[2, 3] = (2 * far * near) / (near - far)
        m[3, 2] = -1
        return m

    def _project_point(self, gx, gy, gz): #vers 1
        """Project a world point (already in this viewport's own Y-up
        local space, matching the vertex array) to screen pixel
        coordinates - None if behind the camera or wildly outside the
        viewport (early-out for picking, not a hard clip)."""
        import numpy as np
        view = self._view_matrix()
        proj = self._projection_matrix()
        p = np.array([gx, gy, gz, 1.0])
        clip = proj @ view @ p
        if clip[3] <= 1e-6:
            return None
        ndc = clip[:3] / clip[3]
        if not (-1.2 <= ndc[0] <= 1.2 and -1.2 <= ndc[1] <= 1.2):
            return None
        screen_x = (ndc[0] + 1) / 2 * self.width()
        screen_y = (1 - ndc[1]) / 2 * self.height()   # NDC y-up -> Qt y-down
        return screen_x, screen_y

    def pick_instance_at(self, click_x, click_y, threshold_px=12): #vers 1
        """Find the loaded instance whose projected screen position is
        closest to a click, within threshold_px - returns the full
        IPLInstance (via self._full_instances, index-aligned with the
        vertex array) or None if nothing is close enough / this pane
        has no real instances (plain tuples with no object behind
        them, e.g. from a caller that only passed x/y/z/name)."""
        if self._vertex_array is None or len(self._vertex_array) == 0:
            return None
        best_idx = None
        best_dist = threshold_px
        for idx, (gx, gy, gz) in enumerate(self._vertex_array):
            proj = self._project_point(float(gx), float(gy), float(gz))
            if proj is None:
                continue
            sx, sy = proj
            d = ((sx - click_x) ** 2 + (sy - click_y) ** 2) ** 0.5
            if d < best_dist:
                best_dist = d
                best_idx = idx
        if best_idx is None:
            return None
        full = getattr(self, '_full_instances', [])
        return full[best_idx] if best_idx < len(full) else None

    def mousePressEvent(self, event): #vers 2
        self._last_pos = event.pos()
        self._press_pos = event.pos()

    def set_pick_callback(self, callback): #vers 2
        """Set a function to call as callback(instance, pane) whenever
        the user clicks (not drags) directly on a rendered marker in
        this pane - lets MapWorkshop wire this to opening the object
        edit panel without MapViewport needing any reference back to
        the workshop itself. Passing the pane itself (not just the
        instance) lets the callback scope its response to just this
        one pane, rather than needing to guess or affect all panes -
        per Keith's report that clicking an object was repositioning
        all 3 views instead of just responding to the one clicked."""
        self._pick_callback = callback

    def configure_movement(self, pan_button='middle', rotate_button='right',
                           invert_x=False, invert_y=False): #vers 1
        """Set which mouse button pans vs rotates this pane, and
        whether pan direction is inverted per axis - lets the Map
        Editor settings expose per-viewport-mode adjustment, since a
        single hardcoded mapping doesn't feel consistent across Top/
        Side/Front/3D's different camera orientations."""
        self._pan_button = pan_button
        self._rotate_button = rotate_button
        self._pan_invert_x = invert_x
        self._pan_invert_y = invert_y

    def _button_matches(self, buttons, name): #vers 1
        mapping = {'left': Qt.MouseButton.LeftButton,
                  'middle': Qt.MouseButton.MiddleButton,
                  'right': Qt.MouseButton.RightButton}
        return bool(buttons & mapping.get(name, Qt.MouseButton.MiddleButton))

    def mouseMoveEvent(self, event): #vers 2
        dx = event.pos().x() - self._last_pos.x()
        dy = event.pos().y() - self._last_pos.y()
        pan_button = getattr(self, '_pan_button', 'middle')
        rotate_button = getattr(self, '_rotate_button', 'right')
        invert_x = getattr(self, '_pan_invert_x', False)
        invert_y = getattr(self, '_pan_invert_y', False)
        if self._button_matches(event.buttons(), rotate_button) and not self._view_locked:
            self._yaw   += dx * 0.5
            self._pitch += dy * 0.5
        elif self._button_matches(event.buttons(), pan_button):
            scale = self._dist * 0.002
            self._pan_x += (-dx if invert_x else dx) * scale
            self._pan_y -= (-dy if invert_y else dy) * scale
        self._last_pos = event.pos(); self.update()

    def mouseReleaseEvent(self, event): #vers 3
        press_pos = getattr(self, '_press_pos', None)
        if press_pos is not None:
            dx = event.pos().x() - press_pos.x()
            dy = event.pos().y() - press_pos.y()
            if (dx * dx + dy * dy) <= 16:   # ~4px - a click, not a drag
                callback = getattr(self, '_pick_callback', None)
                if callback is not None:
                    picked = self.pick_instance_at(event.pos().x(), event.pos().y())
                    if picked is not None:
                        callback(picked, self)
        self._last_pos = event.pos()

    def wheelEvent(self, event): #vers 1
        f = 0.85 if event.angleDelta().y() > 0 else 1.15
        self._dist = max(0.1, min(50000.0, self._dist * f))
        if self._projection == 'ortho':
            try:
                self.resizeGL(self.width(), self.height())
            except Exception:
                pass
        self.update()

    def reset_view(self): #vers 1
        self._yaw = 45.0; self._pitch = 25.0
        self._pan_x = 0.0; self._pan_y = 0.0
        self._auto_fit()
