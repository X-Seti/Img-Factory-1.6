#this belongs in apps/components/Col_Editor/depends/col_3d_viewport.py - Version: 9
# X-Seti - December13 2025 - IMG Factory 1.5 - COL 3D Viewport Pure OpenGL

"""
COL 3D Viewport - Pure OpenGL 3D rendering (no pixmap fallback)
Professional rendering matching Steve M's COL Editor II:
- Shaded surface rendering
- Proper 3D collision spheres (cyan)
- Grid floor for reference
- Professional OpenGL lighting
- Translucent bounding boxes
- Backface culling toggle (OFF by default for collision models)
"""

import math
from typing import Optional
from PyQt6.QtWidgets import QWidget
from PyQt6.QtCore import Qt, QPoint, pyqtSignal
from PyQt6.QtGui import QColor, QPainter, QPen
from PyQt6.QtOpenGLWidgets import QOpenGLWidget

try:
    from OpenGL.GL import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
except ImportError:
    OPENGL_AVAILABLE = False
    print("Warning: PyOpenGL not available - install with: pip install PyOpenGL PyOpenGL_accelerate")

from PyQt6.QtGui import QSurfaceFormat

# Request OpenGL compatibility profile
if OPENGL_AVAILABLE:
    fmt = QSurfaceFormat()
    fmt.setVersion(2, 1)
    fmt.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
    fmt.setDepthBufferSize(24)
    fmt.setSamples(4)
    QSurfaceFormat.setDefaultFormat(fmt)

##Methods list -
# draw_bounding_box
# draw_collision_box #vers 2
# draw_collision_sphere
# draw_face_mesh_shaded #vers 3
# draw_grid_floor
# fit_to_model
# fit_to_window
# initializeGL
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintGL
# pan
# render_collision
# render_collision_model
# reset_camera
# reset_view
# resizeGL
# rotate_x
# rotate_y
# rotate_z
# set_current_model #vers 2
# set_model #vers 2
# set_theme_colors
# set_view_options #vers 2
# setup_lighting
# toggle_backface_culling
# update_display
# wheelEvent
# zoom_in
# zoom_out

class COL3DViewport(QOpenGLWidget if OPENGL_AVAILABLE else QWidget):
    """Pure OpenGL 3D viewport for COL collision models"""
    
    model_selected = pyqtSignal(int)
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        
        # COL model data
        self.current_model = None
        self.selected_model_index = -1
        self.current_file = None
        
        # Camera state
        self.camera_distance = 50.0
        self.camera_rotation_x = 20.0
        self.camera_rotation_y = 45.0
        self.camera_pan_x = 0.0
        self.camera_pan_y = 0.0
        
        # Legacy compatibility variable names
        self.rotation_x = 20.0
        self.rotation_y = 45.0
        self.rotation_z = 0.0
        self.zoom = 10.0
        self.pan_x = 0.0
        self.pan_y = 0.0
        
        # Mouse interaction
        self.last_mouse_pos = QPoint()
        self.dragging = False
        self.drag_button = Qt.MouseButton.NoButton
        
        # Professional theme colors (matching reference editor)
        self.bg_color = QColor(30, 40, 50)
        self.grid_color = QColor(60, 70, 80)
        self.mesh_color = QColor(200, 200, 200)
        self.wireframe_color = QColor(100, 150, 200)
        self.sphere_color = QColor(0, 200, 255, 128)
        self.box_color = QColor(255, 100, 100, 64)
        self.bounds_color = QColor(255, 255, 0, 96)
        
        # Display options
        self.show_mesh = True
        self.show_spheres = True
        self.show_boxes = True
        self.show_bounds = True
        self.show_grid = True
        self.show_wireframe = False
        self.show_shadow_mesh = False
        
        # Rendering options
        self.lighting_enabled = True
        self.backface_culling = False  # OFF by default - collision models have inconsistent winding
        
        self.setMinimumSize(400, 300)
        self.setMouseTracking(True)
    
    
    def initializeGL(self): #vers 1
        """Initialize OpenGL with professional rendering settings"""
        if not OPENGL_AVAILABLE:
            return
        
        # Background color
        glClearColor(
            self.bg_color.redF(),
            self.bg_color.greenF(),
            self.bg_color.blueF(),
            1.0
        )
        
        # Enable depth testing
        glEnable(GL_DEPTH_TEST)
        glDepthFunc(GL_LEQUAL)
        
        # Enable blending for transparency
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        
        # Smooth shading
        glShadeModel(GL_SMOOTH)
        
        # Polygon offset for z-fighting
        glEnable(GL_POLYGON_OFFSET_FILL)
        glPolygonOffset(1.0, 1.0)
        
        # Backface culling (optional - OFF by default for collision models)
        if self.backface_culling:
            glEnable(GL_CULL_FACE)
            glCullFace(GL_BACK)
        else:
            glDisable(GL_CULL_FACE)
        
        # Setup lighting
        self.setup_lighting()
        
        # Quality hints
        glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST)
        glHint(GL_LINE_SMOOTH_HINT, GL_NICEST)
    
    
    def setup_lighting(self): #vers 1
        """Setup professional 3-point lighting system"""
        if not OPENGL_AVAILABLE or not self.lighting_enabled:
            return
        
        try:
            glEnable(GL_LIGHTING)
            glEnable(GL_COLOR_MATERIAL)
            glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
            
            # Key light (main light from upper right)
            glEnable(GL_LIGHT0)
            glLightfv(GL_LIGHT0, GL_POSITION, [1.0, 1.0, 1.0, 0.0])
            glLightfv(GL_LIGHT0, GL_AMBIENT, [0.2, 0.2, 0.2, 1.0])
            glLightfv(GL_LIGHT0, GL_DIFFUSE, [0.8, 0.8, 0.8, 1.0])
            glLightfv(GL_LIGHT0, GL_SPECULAR, [0.5, 0.5, 0.5, 1.0])
            
            # Fill light (softer from left)
            glEnable(GL_LIGHT1)
            glLightfv(GL_LIGHT1, GL_POSITION, [-1.0, 0.5, 0.5, 0.0])
            glLightfv(GL_LIGHT1, GL_AMBIENT, [0.1, 0.1, 0.1, 1.0])
            glLightfv(GL_LIGHT1, GL_DIFFUSE, [0.3, 0.3, 0.3, 1.0])
            
            # Back light (rim light)
            glEnable(GL_LIGHT2)
            glLightfv(GL_LIGHT2, GL_POSITION, [0.0, 0.5, -1.0, 0.0])
            glLightfv(GL_LIGHT2, GL_AMBIENT, [0.05, 0.05, 0.05, 1.0])
            glLightfv(GL_LIGHT2, GL_DIFFUSE, [0.2, 0.2, 0.2, 1.0])
            
        except Exception as e:
            print(f"Lighting setup failed: {e}")
            glDisable(GL_LIGHTING)
    
    
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
        """Render the professional 3D scene"""
        if not OPENGL_AVAILABLE:
            return
        
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity()
        
        # Setup camera transformation
        glTranslatef(self.camera_pan_x, -self.camera_pan_y, -self.camera_distance)
        glRotatef(self.camera_rotation_x, 1, 0, 0)
        glRotatef(self.camera_rotation_y, 0, 1, 0)
        
        # Draw grid floor
        if self.show_grid:
            self.draw_grid_floor()
        
        # Draw COL model if loaded
        if self.current_model:
            self.render_collision_model()
    
    
    def draw_grid_floor(self): #vers 1
        """Draw professional grid floor with axis indicators"""
        glDisable(GL_LIGHTING)
        
        # Grid lines
        glColor4f(
            self.grid_color.redF(),
            self.grid_color.greenF(),
            self.grid_color.blueF(),
            0.3
        )
        
        glBegin(GL_LINES)
        grid_size = 100.0
        grid_step = 5.0
        
        for i in range(-int(grid_size / grid_step), int(grid_size / grid_step) + 1):
            pos = i * grid_step
            
            # Lines parallel to X axis
            glVertex3f(pos, 0, -grid_size)
            glVertex3f(pos, 0, grid_size)
            
            # Lines parallel to Z axis
            glVertex3f(-grid_size, 0, pos)
            glVertex3f(grid_size, 0, pos)
        
        glEnd()
        
        # Coordinate axes (thicker, colored)
        glLineWidth(2.0)
        glBegin(GL_LINES)
        
        # X axis - Red
        glColor3f(0.8, 0.2, 0.2)
        glVertex3f(0, 0, 0)
        glVertex3f(30, 0, 0)
        
        # Y axis - Green
        glColor3f(0.2, 0.8, 0.2)
        glVertex3f(0, 0, 0)
        glVertex3f(0, 30, 0)
        
        # Z axis - Blue
        glColor3f(0.2, 0.2, 0.8)
        glVertex3f(0, 0, 0)
        glVertex3f(0, 0, 30)
        
        glEnd()
        glLineWidth(1.0)
    
    
    def render_collision_model(self): #vers 1
        """Render complete COL model"""
        if not self.current_model:
            return
        
        # Draw bounding box (transparent)
        if self.show_bounds and hasattr(self.current_model, 'bounding_box'):
            self.draw_bounding_box()
        
        # Draw solid mesh faces with lighting
        if self.show_mesh and hasattr(self.current_model, 'faces') and self.current_model.faces:
            self.draw_face_mesh_shaded()
        
        # Draw collision spheres (transparent cyan)
        if self.show_spheres and hasattr(self.current_model, 'spheres') and self.current_model.spheres:
            for sphere in self.current_model.spheres:
                self.draw_collision_sphere(sphere)
        
        # Draw collision boxes (transparent red)
        if self.show_boxes and hasattr(self.current_model, 'boxes') and self.current_model.boxes:
            for box in self.current_model.boxes:
                self.draw_collision_box(box)
    
    
    def draw_face_mesh_shaded(self): #vers 3
        """Draw mesh faces with proper shading and lighting - with debug validation"""
        if not hasattr(self.current_model, 'vertices') or not self.current_model.vertices:
            return
        
        if self.lighting_enabled:
            glEnable(GL_LIGHTING)
        
        # Set material properties
        glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, [0.3, 0.3, 0.3, 1.0])
        glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, [0.8, 0.8, 0.8, 1.0])
        glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, [0.5, 0.5, 0.5, 1.0])
        glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 32.0)
        
        # Draw solid faces
        glColor4f(
            self.mesh_color.redF(),
            self.mesh_color.greenF(),
            self.mesh_color.blueF(),
            1.0
        )
        
        vertices = self.current_model.vertices
        total_faces = len(self.current_model.faces)
        valid_faces = 0
        invalid_faces = 0
        
        glBegin(GL_TRIANGLES)
        
        for face_idx, face in enumerate(self.current_model.faces):
            # COLFace has vertex_indices (tuple of 3 ints)
            if not hasattr(face, 'vertex_indices'):
                invalid_faces += 1
                continue
                
            if len(face.vertex_indices) < 3:
                invalid_faces += 1
                continue
            
            idx0, idx1, idx2 = face.vertex_indices[:3]
            
            # Validate indices (check bounds)
            if idx0 >= len(vertices) or idx1 >= len(vertices) or idx2 >= len(vertices):
                invalid_faces += 1
                continue
            
            # Check for negative indices
            if idx0 < 0 or idx1 < 0 or idx2 < 0:
                invalid_faces += 1
                continue
            
            # Get vertex positions (COLVertex has .position with .x .y .z)
            try:
                v0 = vertices[idx0].position
                v1 = vertices[idx1].position
                v2 = vertices[idx2].position
            except (AttributeError, IndexError) as e:
                invalid_faces += 1
                continue
            
            # Calculate face normal
            edge1_x = v1.x - v0.x
            edge1_y = v1.y - v0.y
            edge1_z = v1.z - v0.z
            
            edge2_x = v2.x - v0.x
            edge2_y = v2.y - v0.y
            edge2_z = v2.z - v0.z
            
            nx = edge1_y * edge2_z - edge1_z * edge2_y
            ny = edge1_z * edge2_x - edge1_x * edge2_z
            nz = edge1_x * edge2_y - edge1_y * edge2_x
            
            # Normalize
            length = math.sqrt(nx*nx + ny*ny + nz*nz)
            if length > 0:
                nx /= length
                ny /= length
                nz /= length
                glNormal3f(nx, ny, nz)
            
            # Draw triangle
            glVertex3f(v0.x, v0.y, v0.z)
            glVertex3f(v1.x, v1.y, v1.z)
            glVertex3f(v2.x, v2.y, v2.z)
            
            valid_faces += 1
        
        glEnd()
        
        # Debug output (only first time or when changed)
        if not hasattr(self, '_last_face_count') or self._last_face_count != total_faces:
            print(f"[COL Viewport] Rendering: {valid_faces}/{total_faces} faces, {invalid_faces} skipped")
            print(f"[COL Viewport] Vertices available: {len(vertices)}")
            print(f"[COL Viewport] Backface culling: {'ON' if self.backface_culling else 'OFF'}")
            self._last_face_count = total_faces
        
        # Optional wireframe overlay
        if self.show_wireframe:
            glDisable(GL_LIGHTING)
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
            glColor4f(
                self.wireframe_color.redF(),
                self.wireframe_color.greenF(),
                self.wireframe_color.blueF(),
                0.5
            )
            
            glBegin(GL_TRIANGLES)
            for face in self.current_model.faces:
                if hasattr(face, 'vertex_indices') and len(face.vertex_indices) >= 3:
                    idx0, idx1, idx2 = face.vertex_indices[:3]
                    if idx0 < len(vertices) and idx1 < len(vertices) and idx2 < len(vertices):
                        if idx0 >= 0 and idx1 >= 0 and idx2 >= 0:
                            try:
                                v0 = vertices[idx0].position
                                v1 = vertices[idx1].position
                                v2 = vertices[idx2].position
                                glVertex3f(v0.x, v0.y, v0.z)
                                glVertex3f(v1.x, v1.y, v1.z)
                                glVertex3f(v2.x, v2.y, v2.z)
                            except:
                                pass
            glEnd()
            
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    
    
    def draw_collision_sphere(self, sphere): #vers 1
        """Draw proper 3D collision sphere"""
        if not hasattr(sphere, 'center') or not hasattr(sphere, 'radius'):
            return
        
        glDisable(GL_LIGHTING)
        glEnable(GL_BLEND)
        
        # Beautiful cyan color
        glColor4f(
            self.sphere_color.redF(),
            self.sphere_color.greenF(),
            self.sphere_color.blueF(),
            0.5
        )
        
        glPushMatrix()
        glTranslatef(sphere.center.x, sphere.center.y, sphere.center.z)
        
        # Create smooth sphere using GLU quadric
        quadric = gluNewQuadric()
        gluQuadricDrawStyle(quadric, GLU_FILL)
        gluQuadricNormals(quadric, GLU_SMOOTH)
        gluSphere(quadric, sphere.radius, 32, 16)
        gluDeleteQuadric(quadric)
        
        glPopMatrix()
    
    
    def draw_collision_box(self, box): #vers 2
        """Draw collision box with transparency"""
        if not hasattr(box, 'min_point') or not hasattr(box, 'max_point'):
            return
        
        glDisable(GL_LIGHTING)
        glEnable(GL_BLEND)
        
        # Red transparent box
        glColor4f(
            self.box_color.redF(),
            self.box_color.greenF(),
            self.box_color.blueF(),
            0.25
        )
        
        min_v = box.min_point
        max_v = box.max_point
        
        # Draw box as solid with transparency
        glBegin(GL_QUADS)
        
        # Front face
        glVertex3f(min_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, max_v.y, max_v.z)
        glVertex3f(min_v.x, max_v.y, max_v.z)
        
        # Back face
        glVertex3f(min_v.x, min_v.y, min_v.z)
        glVertex3f(min_v.x, max_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, min_v.z)
        glVertex3f(max_v.x, min_v.y, min_v.z)
        
        # Top face
        glVertex3f(min_v.x, max_v.y, min_v.z)
        glVertex3f(min_v.x, max_v.y, max_v.z)
        glVertex3f(max_v.x, max_v.y, max_v.z)
        glVertex3f(max_v.x, max_v.y, min_v.z)
        
        # Bottom face
        glVertex3f(min_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, min_v.y, max_v.z)
        glVertex3f(min_v.x, min_v.y, max_v.z)
        
        # Right face
        glVertex3f(max_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, max_v.z)
        glVertex3f(max_v.x, min_v.y, max_v.z)
        
        # Left face
        glVertex3f(min_v.x, min_v.y, min_v.z)
        glVertex3f(min_v.x, min_v.y, max_v.z)
        glVertex3f(min_v.x, max_v.y, max_v.z)
        glVertex3f(min_v.x, max_v.y, min_v.z)
        
        glEnd()
    
    
    def draw_bounding_box(self): #vers 1
        """Draw model bounding box with transparency"""
        bounds = self.current_model.bounding_box
        
        glDisable(GL_LIGHTING)
        glEnable(GL_BLEND)
        
        # Yellow transparent bounds
        glColor4f(
            self.bounds_color.redF(),
            self.bounds_color.greenF(),
            self.bounds_color.blueF(),
            0.15
        )
        
        min_v = bounds.min
        max_v = bounds.max
        
        # Draw wireframe box
        glLineWidth(1.5)
        
        # Bottom rectangle
        glBegin(GL_LINE_LOOP)
        glVertex3f(min_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, min_v.y, max_v.z)
        glVertex3f(min_v.x, min_v.y, max_v.z)
        glEnd()
        
        # Top rectangle
        glBegin(GL_LINE_LOOP)
        glVertex3f(min_v.x, max_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, max_v.z)
        glVertex3f(min_v.x, max_v.y, max_v.z)
        glEnd()
        
        # Vertical lines
        glBegin(GL_LINES)
        glVertex3f(min_v.x, min_v.y, min_v.z)
        glVertex3f(min_v.x, max_v.y, min_v.z)
        
        glVertex3f(max_v.x, min_v.y, min_v.z)
        glVertex3f(max_v.x, max_v.y, min_v.z)
        
        glVertex3f(max_v.x, min_v.y, max_v.z)
        glVertex3f(max_v.x, max_v.y, max_v.z)
        
        glVertex3f(min_v.x, min_v.y, max_v.z)
        glVertex3f(min_v.x, max_v.y, max_v.z)
        glEnd()
        
        glLineWidth(1.0)
    
    
    # ===== MOUSE INTERACTION =====
    
    def mousePressEvent(self, event): #vers 1
        """Handle mouse press"""
        self.last_mouse_pos = event.pos()
        self.dragging = True
        self.drag_button = event.button()
    
    
    def mouseMoveEvent(self, event): #vers 1
        """Handle mouse drag"""
        if not self.dragging:
            return
        
        dx = event.pos().x() - self.last_mouse_pos.x()
        dy = event.pos().y() - self.last_mouse_pos.y()
        
        if self.drag_button == Qt.MouseButton.LeftButton:
            # Rotate camera
            self.camera_rotation_y += dx * 0.5
            self.camera_rotation_x += dy * 0.5
            self.camera_rotation_x = max(-89, min(89, self.camera_rotation_x))
            
            # Sync legacy variables
            self.rotation_x = self.camera_rotation_x
            self.rotation_y = self.camera_rotation_y
            
            self.update()
            
        elif self.drag_button == Qt.MouseButton.RightButton:
            # Pan camera
            self.camera_pan_x += dx * 0.05
            self.camera_pan_y += dy * 0.05
            
            # Sync legacy variables
            self.pan_x = self.camera_pan_x
            self.pan_y = self.camera_pan_y
            
            self.update()
        
        self.last_mouse_pos = event.pos()
    
    
    def mouseReleaseEvent(self, event): #vers 1
        """Handle mouse release"""
        self.dragging = False
        self.drag_button = Qt.MouseButton.NoButton
    
    
    def wheelEvent(self, event): #vers 1
        """Handle mouse wheel for zoom"""
        delta = event.angleDelta().y()
        zoom_speed = 0.1
        
        if delta > 0:
            self.camera_distance *= (1.0 - zoom_speed)
        else:
            self.camera_distance *= (1.0 + zoom_speed)
        
        # Clamp zoom
        self.camera_distance = max(5.0, min(500.0, self.camera_distance))
        
        # Sync legacy variable
        self.zoom = self.camera_distance
        
        self.update()
    
    
    # ===== PUBLIC API / COMPATIBILITY METHODS =====
    
    def set_model(self, model): #vers 2
        """Set COL model to display with diagnostics"""
        self.current_model = model
        
        # Print diagnostic info
        if model:
            print(f"\n[COL Viewport] Model loaded:")
            print(f"  Name: {getattr(model, 'name', 'Unknown')}")
            print(f"  Vertices: {len(getattr(model, 'vertices', []))}")
            print(f"  Faces: {len(getattr(model, 'faces', []))}")
            print(f"  Spheres: {len(getattr(model, 'spheres', []))}")
            print(f"  Boxes: {len(getattr(model, 'boxes', []))}")
            
            # Sample first few face indices to check validity
            if hasattr(model, 'faces') and model.faces:
                print(f"  Sample face indices (first 5):")
                for i, face in enumerate(model.faces[:5]):
                    if hasattr(face, 'vertex_indices'):
                        print(f"    Face {i}: {face.vertex_indices}")
            
            # Check vertex range
            if hasattr(model, 'vertices') and model.vertices:
                if model.vertices:
                    v0 = model.vertices[0].position
                    print(f"  First vertex: ({v0.x:.2f}, {v0.y:.2f}, {v0.z:.2f})")
            
            self.fit_to_model()
        
        self.update()
    
    
    def set_current_model(self, model, model_index=-1): #vers 2
        """Set current model (compatibility alias) with diagnostics"""
        self.current_model = model
        self.selected_model_index = model_index
        
        # Print diagnostic info
        if model:
            print(f"\n[COL Viewport] Model #{model_index} loaded:")
            print(f"  Name: {getattr(model, 'name', 'Unknown')}")
            print(f"  Vertices: {len(getattr(model, 'vertices', []))}")
            print(f"  Faces: {len(getattr(model, 'faces', []))}")
            print(f"  Spheres: {len(getattr(model, 'spheres', []))}")
            print(f"  Boxes: {len(getattr(model, 'boxes', []))}")
            
            self.fit_to_model()
        
        self.update()
    
    
    def fit_to_model(self): #vers 1
        """Auto-fit camera to model bounds"""
        if not self.current_model or not hasattr(self.current_model, 'bounding_box'):
            return
        
        bounds = self.current_model.bounding_box
        
        # Calculate model size
        size_x = bounds.max.x - bounds.min.x
        size_y = bounds.max.y - bounds.min.y
        size_z = bounds.max.z - bounds.min.z
        
        max_size = max(size_x, size_y, size_z)
        
        # Set camera distance based on model size
        if max_size > 0:
            self.camera_distance = max_size * 2.0
            self.zoom = self.camera_distance
        else:
            self.camera_distance = 50.0
            self.zoom = 50.0
    
    
    def reset_camera(self): #vers 1
        """Reset camera to default view"""
        self.camera_distance = 50.0
        self.camera_rotation_x = 20.0
        self.camera_rotation_y = 45.0
        self.camera_pan_x = 0.0
        self.camera_pan_y = 0.0
        
        # Sync legacy variables
        self.rotation_x = 20.0
        self.rotation_y = 45.0
        self.rotation_z = 0.0
        self.zoom = 50.0
        self.pan_x = 0.0
        self.pan_y = 0.0
        
        self.update()
    
    
    def reset_view(self): #vers 1
        """Reset view (alias for reset_camera)"""
        self.reset_camera()
    
    
    def zoom_in(self): #vers 1
        """Zoom in (decrease camera distance)"""
        self.camera_distance *= 0.9
        self.camera_distance = max(5.0, self.camera_distance)
        self.zoom = self.camera_distance
        self.update()
    
    
    def zoom_out(self): #vers 1
        """Zoom out (increase camera distance)"""
        self.camera_distance *= 1.1
        self.camera_distance = min(500.0, self.camera_distance)
        self.zoom = self.camera_distance
        self.update()
    
    
    def fit_to_window(self): #vers 1
        """Fit model to window (alias for fit_to_model)"""
        self.fit_to_model()
    
    
    def rotate_x(self, degrees): #vers 1
        """Rotate around X axis"""
        self.camera_rotation_x += degrees
        self.camera_rotation_x = max(-89, min(89, self.camera_rotation_x))
        self.rotation_x = self.camera_rotation_x
        self.update()
    
    
    def rotate_y(self, degrees): #vers 1
        """Rotate around Y axis"""
        self.camera_rotation_y += degrees
        self.camera_rotation_y = self.camera_rotation_y % 360
        self.rotation_y = self.camera_rotation_y
        self.update()
    
    
    def rotate_z(self, degrees): #vers 1
        """Rotate around Z axis (not used in this camera system)"""
        self.rotation_z = (self.rotation_z + degrees) % 360
        # Z rotation not implemented in this camera
    
    
    def pan(self, dx, dy): #vers 1
        """Pan camera by pixel offset"""
        self.camera_pan_x += dx * 0.05
        self.camera_pan_y += dy * 0.05
        self.pan_x = self.camera_pan_x
        self.pan_y = self.camera_pan_y
        self.update()
    
    
    def render_collision(self): #vers 1
        """Render collision (triggers repaint)"""
        self.update()
    
    
    def set_view_options(self, show_mesh=None, show_spheres=None, 
                        show_boxes=None, show_bounds=None, 
                        show_grid=None, show_wireframe=None,
                        show_shadow=None, backface_culling=None): #vers 2
        """Update display options"""
        if show_mesh is not None:
            self.show_mesh = show_mesh
        if show_spheres is not None:
            self.show_spheres = show_spheres
        if show_boxes is not None:
            self.show_boxes = show_boxes
        if show_bounds is not None:
            self.show_bounds = show_bounds
        if show_grid is not None:
            self.show_grid = show_grid
        if show_wireframe is not None:
            self.show_wireframe = show_wireframe
        if show_shadow is not None:
            self.show_shadow_mesh = show_shadow
        if backface_culling is not None:
            self.backface_culling = backface_culling
            # Apply immediately
            if OPENGL_AVAILABLE:
                if backface_culling:
                    glEnable(GL_CULL_FACE)
                    glCullFace(GL_BACK)
                else:
                    glDisable(GL_CULL_FACE)
        
        self.update()
    
    
    def set_theme_colors(self, bg_color=None, mesh_color=None, 
                        sphere_color=None, box_color=None): #vers 1
        """Update theme colors"""
        if bg_color:
            self.bg_color = bg_color
            glClearColor(bg_color.redF(), bg_color.greenF(), 
                        bg_color.blueF(), 1.0)
        
        if mesh_color:
            self.mesh_color = mesh_color
        if sphere_color:
            self.sphere_color = sphere_color
        if box_color:
            self.box_color = box_color
        
        self.update()
    
    
    def toggle_backface_culling(self): #vers 1
        """Toggle backface culling on/off"""
        self.backface_culling = not self.backface_culling
        if OPENGL_AVAILABLE:
            if self.backface_culling:
                glEnable(GL_CULL_FACE)
                glCullFace(GL_BACK)
            else:
                glDisable(GL_CULL_FACE)
        self.update()
        return self.backface_culling
    
    
    def update_display(self): #vers 1
        """Force display update"""
        self.update()


# Export
__all__ = ['COL3DViewport', 'OPENGL_AVAILABLE']
