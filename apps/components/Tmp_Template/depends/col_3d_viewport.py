#this belongs in components/Col_Editor/depends/col_3d_viewport.py - Version: 1
# X-Seti - October20 2025 - IMG Factory 1.5 - COL 3D Viewport

"""
COL 3D Viewport - OpenGL-based 3D rendering widget for COL collision models
Renders vertices, faces, spheres, boxes, and shadow meshes with mouse navigation
Based on Steve M's COL Editor II approach using OpenGL
"""

import math
from typing import Optional, List, Tuple
from PyQt6.QtWidgets import QWidget
from PyQt6.QtCore import Qt, QPoint, pyqtSignal
from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QTransform
from PyQt6.QtOpenGLWidgets import QOpenGLWidget

try:
    from OpenGL.GL import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
except ImportError:
    OPENGL_AVAILABLE = False

from PyQt6.QtGui import QSurfaceFormat

# Request OpenGL compatibility profile
fmt = QSurfaceFormat()
fmt.setVersion(2, 1)  # OpenGL 2.1 with compatibility
fmt.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
QSurfaceFormat.setDefaultFormat(fmt)

##Methods list -
# _draw_bounding_box
# _draw_checkerboard
# _draw_grid
# _draw_shadow_mesh
# draw_box
# draw_face_mesh
# draw_sphere
# fit_to_window
# initializeGL
# mouseMoveEvent
# mousePressEvent
# pan
# paintGL
# render_collision
# reset_view
# resizeGL
# rotate_x
# rotate_y
# rotate_z
# set_background_color
# set_checkerboard_background
# set_current_model
# set_model
# set_view_options
# setPixmap
# update_display
# wheelEvent
# zoom_in
# zoom_out

class COL3DViewport(QOpenGLWidget if OPENGL_AVAILABLE else QWidget):
    """OpenGL 3D viewport for COL collision models"""
    
    model_selected = pyqtSignal(int)
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        
        # View state
        self.rotation_x = 20.0
        self.rotation_y = 45.0
        self.zoom = 10.0
        self.pan_x = 0.0
        self.pan_y = 0.0

        self._overlay_opacity = 50
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)

        # Mouse interaction
        self.dragging = False
        self.drag_start = QPoint(0, 0)
        self.drag_mode = None  # 'pan' or 'rotate'

        # Mouse tracking
        self.last_mouse_pos = QPoint()
        self.mouse_button = Qt.MouseButton.NoButton

        # Display state
        self.background_color = QColor(42, 42, 42)
        self.background_mode = 'solid'
        self.placeholder_text = "Select a collision model to preview"
        self._checkerboard_size = 16

        self.scaled_pixmap = None
        self.current_model = None
        self.original_pixmap = None

        # Display options
        self.show_spheres = True
        self.show_boxes = True
        self.show_mesh = True
        self.show_wireframe = True
        self.show_bounds = True
        self.show_shadow_mesh = False
        
        # Current data
        self.current_model = None
        self.current_file = None
        self.selected_model_index = -1
        
        # Colors
        self.bg_color = QColor(30, 30, 30)
        self.mesh_color = QColor(0, 255, 0)
        self.wireframe_color = QColor(100, 255, 100)
        self.sphere_color = QColor(0, 200, 255)
        self.box_color = QColor(255, 200, 0)
        self.bounds_color = QColor(255, 0, 0)
        
        self.setMinimumSize(400, 300)


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
            #self.setText(self.placeholder_text)
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
            #self.setText(f"Collision Model: {name}\n\nRendering...")
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


    def initializeGL(self): #vers 3
        """Initialize OpenGL settings - Modern OpenGL compatible"""
        if not OPENGL_AVAILABLE:
            return

        glClearColor(self.bg_color.redF(), self.bg_color.greenF(),
                    self.bg_color.blueF(), 1.0)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        # Legacy OpenGL features - wrap in try/except for compatibility
        try:
            glEnable(GL_LINE_SMOOTH)
            glHint(GL_LINE_SMOOTH_HINT, GL_NICEST)

            # Setup lighting (legacy)
            glEnable(GL_LIGHTING)
            glEnable(GL_LIGHT0)
            glEnable(GL_COLOR_MATERIAL)
            glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)

            # Light position
            glLightfv(GL_LIGHT0, GL_POSITION, [1.0, 1.0, 1.0, 0.0])
            glLightfv(GL_LIGHT0, GL_AMBIENT, [0.3, 0.3, 0.3, 1.0])
            glLightfv(GL_LIGHT0, GL_DIFFUSE, [0.8, 0.8, 0.8, 1.0])
        except Exception as e:
            # Modern OpenGL core profile - skip legacy features
            print(f"Note: Using modern OpenGL (legacy lighting disabled): {e}")
    
    def resizeGL(self, w, h): #vers 1
        """Handle viewport resize"""
        if not OPENGL_AVAILABLE:
            return
        
        glViewport(0, 0, w, h)
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        aspect = w / h if h > 0 else 1.0
        gluPerspective(45.0, aspect, 0.1, 1000.0)
        glMatrixMode(GL_MODELVIEW)
    
    def paintGL(self): #vers 1
        """Render the 3D scene"""
        if not OPENGL_AVAILABLE:
            self._paint_fallback()
            return
        
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity()
        
        # Apply camera transformations
        glTranslatef(self.pan_x, -self.pan_y, -self.zoom)
        glRotatef(self.rotation_x, 1.0, 0.0, 0.0)
        glRotatef(self.rotation_y, 0.0, 1.0, 0.0)
        
        # Draw grid
        self._draw_grid()
        
        if not self.current_model:
            return
        
        # Draw collision elements
        if self.show_mesh and hasattr(self.current_model, 'faces'):
            self.draw_face_mesh()
        
        if self.show_spheres and hasattr(self.current_model, 'spheres'):
            for sphere in self.current_model.spheres:
                self.draw_sphere(sphere)
        
        if self.show_boxes and hasattr(self.current_model, 'boxes'):
            for box in self.current_model.boxes:
                self.draw_box(box)
        
        if self.show_bounds and hasattr(self.current_model, 'bounding_box'):
            self._draw_bounding_box(self.current_model.bounding_box)
        
        if self.show_shadow_mesh and hasattr(self.current_model, 'shadow_faces'):
            self._draw_shadow_mesh()
    
    def draw_face_mesh(self): #vers 1
        """Draw collision mesh faces"""
        if not hasattr(self.current_model, 'faces') or not hasattr(self.current_model, 'vertices'):
            return
        
        vertices = self.current_model.vertices
        faces = self.current_model.faces


        print(f"✅ Drawing {len(faces)} faces, {len(vertices)} vertices")
        if len(vertices) > 0:
            v = vertices[0]
            print(f"   First vertex: ({v.position.x:.3f}, {v.position.y:.3f}, {v.position.z:.3f})")

        
        if self.show_wireframe:
            glDisable(GL_LIGHTING)
            glColor3f(self.wireframe_color.redF(), self.wireframe_color.greenF(), 
                     self.wireframe_color.blueF())
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        else:
            glEnable(GL_LIGHTING)
            glColor3f(self.mesh_color.redF(), self.mesh_color.greenF(), 
                     self.mesh_color.blueF())
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        
        glBegin(GL_TRIANGLES)
        for face in faces:
            if hasattr(face, 'indices') and len(face.indices) >= 3:
                for idx in face.indices[:3]:
                    if idx < len(vertices):
                        v = vertices[idx]
                        if hasattr(v, 'x'):
                            glVertex3f(v.x, v.y, v.z)
        glEnd()
        
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    
    def draw_sphere(self, sphere): #vers 1
        """Draw collision sphere"""
        if not hasattr(sphere, 'center') or not hasattr(sphere, 'radius'):
            return
        
        glDisable(GL_LIGHTING)
        glColor4f(self.sphere_color.redF(), self.sphere_color.greenF(), 
                 self.sphere_color.blueF(), 0.4)
        
        glPushMatrix()
        glTranslatef(sphere.center.x, sphere.center.y, sphere.center.z)
        
        # Draw wireframe sphere
        quadric = gluNewQuadric()
        gluQuadricDrawStyle(quadric, GLU_LINE)
        gluSphere(quadric, sphere.radius, 16, 16)
        gluDeleteQuadric(quadric)
        
        glPopMatrix()
    
    def draw_box(self, box): #vers 1
        """Draw collision box"""
        if not hasattr(box, 'min') or not hasattr(box, 'max'):
            return
        
        glDisable(GL_LIGHTING)
        glColor4f(self.box_color.redF(), self.box_color.greenF(), 
                 self.box_color.blueF(), 0.4)
        
        min_v = box.min
        max_v = box.max
        
        glBegin(GL_LINE_LOOP)
        glVertex3f(min_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, min_v.z)
        glVertex3f(min_v.x, max_v.y, min_v.z)
        glEnd()
        
        glBegin(GL_LINE_LOOP)
        glVertex3f(min_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, max_v.y, max_v.z)
        glVertex3f(min_v.x, max_v.y, max_v.z)
        glEnd()
        
        glBegin(GL_LINES)
        glVertex3f(min_v.x, min_v.y, min_v.z)
        glVertex3f(min_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, max_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, max_v.z)
        glVertex3f(min_v.x, max_v.y, min_v.z)
        glVertex3f(min_v.x, max_v.y, max_v.z)
        glEnd()
    
    def _draw_grid(self): #vers 1
        """Draw reference grid"""
        glDisable(GL_LIGHTING)
        glColor3f(0.3, 0.3, 0.3)
        glBegin(GL_LINES)
        
        grid_size = 50.0
        grid_step = 5.0
        
        for i in range(-int(grid_size), int(grid_size) + 1):
            pos = i * grid_step
            glVertex3f(pos, 0, -grid_size * grid_step)
            glVertex3f(pos, 0, grid_size * grid_step)
            glVertex3f(-grid_size * grid_step, 0, pos)
            glVertex3f(grid_size * grid_step, 0, pos)
        
        glEnd()
        
        # Draw axes
        glBegin(GL_LINES)
        glColor3f(1.0, 0.0, 0.0)
        glVertex3f(0, 0, 0)
        glVertex3f(10, 0, 0)
        glColor3f(0.0, 1.0, 0.0)
        glVertex3f(0, 0, 0)
        glVertex3f(0, 10, 0)
        glColor3f(0.0, 0.0, 1.0)
        glVertex3f(0, 0, 0)
        glVertex3f(0, 0, 10)
        glEnd()
    
    def _draw_bounding_box(self, bbox): #vers 1
        """Draw bounding box"""
        if not bbox or not hasattr(bbox, 'min') or not hasattr(bbox, 'max'):
            return
        
        glDisable(GL_LIGHTING)
        glColor3f(self.bounds_color.redF(), self.bounds_color.greenF(), 
                 self.bounds_color.blueF())
        
        min_v = bbox.min
        max_v = bbox.max
        
        glBegin(GL_LINE_LOOP)
        glVertex3f(min_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, min_v.z)
        glVertex3f(min_v.x, max_v.y, min_v.z)
        glEnd()
        
        glBegin(GL_LINE_LOOP)
        glVertex3f(min_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, max_v.y, max_v.z)
        glVertex3f(min_v.x, max_v.y, max_v.z)
        glEnd()
        
        glBegin(GL_LINES)
        glVertex3f(min_v.x, min_v.y, min_v.z)
        glVertex3f(min_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, max_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, max_v.z)
        glVertex3f(min_v.x, max_v.y, min_v.z)
        glVertex3f(min_v.x, max_v.y, max_v.z)
        glEnd()
    
    def _draw_shadow_mesh(self): #vers 1
        """Draw shadow mesh if present"""
        if not hasattr(self.current_model, 'shadow_faces'):
            return
        
        glDisable(GL_LIGHTING)
        glColor4f(0.5, 0.5, 0.5, 0.3)
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        
        # Draw shadow faces similar to regular mesh
        glBegin(GL_TRIANGLES)
        for face in self.current_model.shadow_faces:
            if hasattr(face, 'indices'):
                for idx in face.indices[:3]:
                    if idx < len(self.current_model.vertices):
                        v = self.current_model.vertices[idx]
                        glVertex3f(v.x, v.y, v.z)
        glEnd()
        
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    
    def mousePressEvent(self, event): #vers 1
        """Handle mouse press for rotation/zoom"""
        self.last_mouse_pos = event.pos()
        self.mouse_button = event.button()
    
    def mouseMoveEvent(self, event): #vers 1
        """Handle mouse movement for navigation"""
        dx = event.pos().x() - self.last_mouse_pos.x()
        dy = event.pos().y() - self.last_mouse_pos.y()
        
        if self.mouse_button == Qt.MouseButton.LeftButton:
            # Rotate view
            self.rotation_y += dx * 0.5
            self.rotation_x += dy * 0.5
            self.update()
        elif self.mouse_button == Qt.MouseButton.RightButton:
            # Pan view
            self.pan_x += dx * 0.05
            self.pan_y += dy * 0.05
            self.update()
        
        self.last_mouse_pos = event.pos()
    
    def wheelEvent(self, event): #vers 1
        """Handle mouse wheel for zoom"""
        delta = event.angleDelta().y()
        self.zoom -= delta * 0.01
        self.zoom = max(1.0, min(100.0, self.zoom))
        self.update()
    
    def set_current_model(self, model, model_index=-1): #vers 1
        """Set current COL model to display"""
        self.current_model = model
        self.selected_model_index = model_index
        self.update()
    
    def set_view_options(self, show_spheres=None, show_boxes=None, show_mesh=None, 
                        show_wireframe=None, show_bounds=None, show_shadow=None): #vers 1
        """Update view display options"""
        if show_spheres is not None:
            self.show_spheres = show_spheres
        if show_boxes is not None:
            self.show_boxes = show_boxes
        if show_mesh is not None:
            self.show_mesh = show_mesh
        if show_wireframe is not None:
            self.show_wireframe = show_wireframe
        if show_bounds is not None:
            self.show_bounds = show_bounds
        if show_shadow is not None:
            self.show_shadow_mesh = show_shadow
        
        self.update()
    
    def reset_view(self): #vers 2
        """Reset camera to default position"""
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        self.rotation_x = 20.0
        self.rotation_y = 45.0
        self.rotation_z = 0
        #self.zoom = 10.0
        self.pan_x = 0.0
        self.pan_y = 0.0
        self.render_collision()
        self.update()
    

    def pan(self, dx, dy): #vers 3
        """Pan the view by delta x and y"""
        self.pan_x -= dx * 0.05 # Inverted to fix left/right swap
        self.pan_y -= dy * 0.05  # Inverted to fix up/down swap
        self.update()


    """
    def paintEvent(self, event): #vers 2
        "Paint the preview with background and image"
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
    """

    def set_checkerboard_background(self): #vers 1
        """Enable checkerboard background"""
        self.background_mode = 'checkerboard'
        self.update()

    def set_background_color(self, color): #vers 2
        """Set background color for viewport"""
        if color.isValid():
            self.bg_color = color
            if OPENGL_AVAILABLE:
                glClearColor(color.redF(), color.greenF(), color.blueF(), 1.0)
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
        """Zoom in by decreasing zoom distance"""
        self.zoom += 1.0 #swapped
        self.zoom = max(1.0, self.zoom)
        self.update()

    def zoom_out(self): #vers 1
        """Zoom out by increasing zoom distance"""
        self.zoom -= 1.0 #swapped
        self.zoom = min(100.0, self.zoom)
        self.update()

    def fit_to_window(self): #vers 2
        """Fit image to window size"""
        if not self.original_pixmap:
            return

        img_size = self.original_pixmap.size()
        widget_size = self.size()

        zoom_h = widget_size.width() / img_size.width()
        zoom_w = widget_size.height() / img_size.height()

        self.zoom = min(zoom_w, zoom_h) * 0.95
        self.pan = QPoint(0, 0)
        self._update_scaled_pixmap()
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

    def update_display(self): #vers 1
        """Force display update"""
        self.update()
    
    def _paint_fallback(self): #vers 1
        """Fallback rendering when OpenGL unavailable"""
        painter = QPainter(self)
        painter.fillRect(self.rect(), self.bg_color)
        
        painter.setPen(QPen(QColor(255, 255, 255)))
        text = "OpenGL not available\nInstall PyOpenGL:\npip install PyOpenGL PyOpenGL_accelerate"
        painter.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, text)


# Export classes
__all__ = ['COL3DViewport']
