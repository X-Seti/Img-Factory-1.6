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
        self._label_widget.setStyleSheet(
            "color: palette(text); background: transparent; font-weight: bold;")
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

    def _auto_fit(self): #vers 1
        """Frame the camera distance/pan to cover all loaded instances."""
        if not self._instances:
            return
        xs = [i[0] for i in self._instances]
        ys = [i[1] for i in self._instances]
        zs = [i[2] for i in self._instances]
        diag = math.sqrt((max(xs)-min(xs))**2 + (max(ys)-min(ys))**2 +
                         (max(zs)-min(zs))**2)
        self._dist  = max(diag * 0.6, 5.0)
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
            if self._view_label == 'Top':
                gluLookAt(self._pan_x, self._dist, self._pan_y,
                          self._pan_x, 0, self._pan_y, 0, 0, -1)
            elif self._view_label == 'Side':
                gluLookAt(self._dist, self._pan_y, self._pan_x,
                          0, self._pan_y, self._pan_x, 0, 1, 0)
            else:  # Front
                gluLookAt(self._pan_x, self._pan_y, self._dist,
                          self._pan_x, self._pan_y, 0, 0, 1, 0)
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

    def set_gizmo_position(self, pos): #vers 1
        """Show (or hide, if pos is None) an XYZ axis gizmo at a world
        position - used to mark the currently-selected instance from
        Instance List."""
        self._gizmo_pos = pos
        self.update()

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

    def _draw_instances(self): #vers 3
        """Draw all loaded instance positions as points, via a single
        glBegin(GL_POINTS)/glEnd block - not full 6-quad cubes, which
        at 51,711 instances meant over 1.2 million individual
        glVertex3f calls per frame and was the original cause of the
        reported slowness. Real per-instance DFF/TXD geometry is the
        proper long-term replacement for this whole method, once
        that's built.

        Deliberately NOT using glVertexPointer/glDrawArrays/
        glEnableClientState (client-side vertex arrays) here, even
        though they'd be faster still - a real crash report showed
        glVertexPointer returning None from PyOpenGL's late-binding on
        actual hardware (that specific legacy client-array API not
        exposed by the driver/context), which cascaded into a second
        exception inside PyOpenGL's own error handling and a full
        process abort/core dump - not something a try/except around
        the call can safely catch, since the failure escalates past
        normal Python exception handling. Plain glBegin/glEnd immediate
        mode is already used elsewhere in this codebase (dff_viewport.py)
        without issue, so it's the safer, more portable choice here even
        at some cost to raw draw-call count."""
        glColor3f(0.9, 0.5, 0.2)
        glPointSize(max(1.0, min(8.0, self._marker_size)))

        va = getattr(self, '_vertex_array', None)
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

    def mousePressEvent(self, event): #vers 1
        self._last_pos = event.pos()

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

    def mouseReleaseEvent(self, event): #vers 1
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
