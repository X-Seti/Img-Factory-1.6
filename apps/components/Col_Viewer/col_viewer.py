#this belongs in components/col_viewer/col_viewer.py - Version: 1
# X-Seti - October22 2025 - IMG Factory 1.5 - COL Viewer

"""
COL Viewer - Clean 3D collision viewer from scratch
OpenGL-based rendering with theme integration
View-only, no editing features
"""

import os
import sys
from typing import Optional
from PyQt6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QLabel
from PyQt6.QtCore import Qt, QPoint, pyqtSignal
from PyQt6.QtGui import QColor
from PyQt6.QtOpenGLWidgets import QOpenGLWidget

try:
    from OpenGL.GL import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
except ImportError:
    OPENGL_AVAILABLE = False

# Import COL parser and materials
from apps.components.col_viewer.depends.COL_Parser import COLParser, COLModel
from apps.components.col_viewer.depends.COL_Materials import get_material_name, get_material_info

##Methods list -
# draw_bounding_box
# draw_collision_boxes
# draw_collision_spheres
# draw_grid
# draw_mesh_faces
# get_theme_colors
# initializeGL
# load_col_file
# mouseMoveEvent
# mousePressEvent
# paintGL
# render_col_model
# reset_camera
# resizeGL
# set_theme_colors
# update_info_label
# wheelEvent
# zoom_extents

##Classes -
# COL3DViewport
# COLViewerWidget

class COL3DViewport(QOpenGLWidget if OPENGL_AVAILABLE else QWidget): #vers 1
    """OpenGL 3D viewport for COL rendering"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        
        # COL model data
        self.col_model: Optional[COLModel] = None
        
        # Camera state
        self.camera_distance = 50.0
        self.camera_rotation_x = 20.0
        self.camera_rotation_y = 45.0
        self.camera_pan_x = 0.0
        self.camera_pan_y = 0.0
        
        # Mouse interaction
        self.last_mouse_pos = QPoint()
        self.dragging = False
        self.drag_button = Qt.MouseButton.NoButton
        
        # Theme colors
        self.bg_color = QColor(42, 42, 42)
        self.grid_color = QColor(80, 80, 80)
        self.mesh_color = QColor(100, 150, 255)
        self.bounds_color = QColor(255, 255, 0)
        self.sphere_color = QColor(255, 100, 100)
        self.box_color = QColor(100, 255, 100)
        
        # Display options
        self.show_mesh = True
        self.show_spheres = True
        self.show_boxes = True
        self.show_bounds = True
        self.show_grid = True
        
        self.setMinimumSize(400, 400)
    
    def initializeGL(self): #vers 1
        """Initialize OpenGL settings"""
        if not OPENGL_AVAILABLE:
            return
        
        glClearColor(
            self.bg_color.redF(),
            self.bg_color.greenF(),
            self.bg_color.blueF(),
            1.0
        )
        
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        
        # Enable lighting for better 3D appearance
        try:
            glEnable(GL_LIGHTING)
            glEnable(GL_LIGHT0)
            glEnable(GL_COLOR_MATERIAL)
            glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
            
            # Light position
            glLightfv(GL_LIGHT0, GL_POSITION, [1.0, 1.0, 1.0, 0.0])
            glLightfv(GL_LIGHT0, GL_AMBIENT, [0.3, 0.3, 0.3, 1.0])
            glLightfv(GL_LIGHT0, GL_DIFFUSE, [0.8, 0.8, 0.8, 1.0])
        except:
            pass  # Modern OpenGL may not support legacy lighting
    
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
            return
        
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity()
        
        # Setup camera
        glTranslatef(self.camera_pan_x, self.camera_pan_y, -self.camera_distance)
        glRotatef(self.camera_rotation_x, 1, 0, 0)
        glRotatef(self.camera_rotation_y, 0, 1, 0)
        
        # Draw grid
        if self.show_grid:
            self.draw_grid()
        
        # Draw COL model if loaded
        if self.col_model:
            self.render_col_model()
    
    def draw_grid(self): #vers 1
        """Draw ground grid"""
        glDisable(GL_LIGHTING)
        glColor3f(
            self.grid_color.redF(),
            self.grid_color.greenF(),
            self.grid_color.blueF()
        )
        
        glBegin(GL_LINES)
        grid_size = 50.0
        grid_step = 5.0
        
        for i in range(-int(grid_size), int(grid_size) + 1):
            pos = i * grid_step
            # Lines parallel to X axis
            glVertex3f(pos, 0, -grid_size * grid_step)
            glVertex3f(pos, 0, grid_size * grid_step)
            # Lines parallel to Z axis
            glVertex3f(-grid_size * grid_step, 0, pos)
            glVertex3f(grid_size * grid_step, 0, pos)
        
        glEnd()
        
        # Draw axes
        glBegin(GL_LINES)
        # X axis - Red
        glColor3f(1.0, 0.0, 0.0)
        glVertex3f(0, 0, 0)
        glVertex3f(20, 0, 0)
        # Y axis - Green
        glColor3f(0.0, 1.0, 0.0)
        glVertex3f(0, 0, 0)
        glVertex3f(0, 20, 0)
        # Z axis - Blue
        glColor3f(0.0, 0.0, 1.0)
        glVertex3f(0, 0, 0)
        glVertex3f(0, 0, 20)
        glEnd()
    
    def render_col_model(self): #vers 1
        """Render complete COL model"""
        if not self.col_model:
            return
        
        # Draw bounding box
        if self.show_bounds and self.col_model.bounds:
            self.draw_bounding_box()
        
        # Draw collision spheres
        if self.show_spheres and self.col_model.spheres:
            self.draw_collision_spheres()
        
        # Draw collision boxes
        if self.show_boxes and self.col_model.boxes:
            self.draw_collision_boxes()
        
        # Draw mesh faces
        if self.show_mesh and self.col_model.faces:
            self.draw_mesh_faces()
    
    def draw_bounding_box(self): #vers 1
        """Draw model bounding box"""
        bounds = self.col_model.bounds
        
        glDisable(GL_LIGHTING)
        glColor3f(
            self.bounds_color.redF(),
            self.bounds_color.greenF(),
            self.bounds_color.blueF()
        )
        
        min_v = bounds.min
        max_v = bounds.max
        
        # Draw box edges
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
    
    def draw_collision_spheres(self): #vers 1
        """Draw collision spheres"""
        glDisable(GL_LIGHTING)
        glColor4f(
            self.sphere_color.redF(),
            self.sphere_color.greenF(),
            self.sphere_color.blueF(),
            0.3
        )
        
        for sphere in self.col_model.spheres:
            glPushMatrix()
            glTranslatef(sphere.center.x, sphere.center.y, sphere.center.z)
            
            # Draw wireframe sphere
            quadric = gluNewQuadric()
            gluQuadricDrawStyle(quadric, GLU_LINE)
            gluSphere(quadric, sphere.radius, 12, 12)
            gluDeleteQuadric(quadric)
            
            glPopMatrix()
    
    def draw_collision_boxes(self): #vers 1
        """Draw collision boxes"""
        glDisable(GL_LIGHTING)
        glColor4f(
            self.box_color.redF(),
            self.box_color.greenF(),
            self.box_color.blueF(),
            0.3
        )
        
        for box in self.col_model.boxes:
            min_p = box.min_point
            max_p = box.max_point
            
            # Draw box edges
            glBegin(GL_LINE_LOOP)
            glVertex3f(min_p.x, min_p.y, min_p.z)
            glVertex3f(max_p.x, min_p.y, min_p.z)
            glVertex3f(max_p.x, max_p.y, min_p.z)
            glVertex3f(min_p.x, max_p.y, min_p.z)
            glEnd()
            
            glBegin(GL_LINE_LOOP)
            glVertex3f(min_p.x, min_p.y, max_p.z)
            glVertex3f(max_p.x, min_p.y, max_p.z)
            glVertex3f(max_p.x, max_p.y, max_p.z)
            glVertex3f(min_p.x, max_p.y, max_p.z)
            glEnd()
            
            glBegin(GL_LINES)
            glVertex3f(min_p.x, min_p.y, min_p.z)
            glVertex3f(min_p.x, min_p.y, max_p.z)
            glVertex3f(max_p.x, min_p.y, min_p.z)
            glVertex3f(max_p.x, min_p.y, max_p.z)
            glVertex3f(max_p.x, max_p.y, min_p.z)
            glVertex3f(max_p.x, max_p.y, max_p.z)
            glVertex3f(min_p.x, max_p.y, min_p.z)
            glVertex3f(min_p.x, max_p.y, max_p.z)
            glEnd()
    
    def draw_mesh_faces(self): #vers 1
        """Draw collision mesh faces"""
        try:
            glEnable(GL_LIGHTING)
        except:
            glDisable(GL_LIGHTING)
        
        glColor4f(
            self.mesh_color.redF(),
            self.mesh_color.greenF(),
            self.mesh_color.blueF(),
            0.6
        )
        
        vertices = self.col_model.vertices
        
        glBegin(GL_TRIANGLES)
        for face in self.col_model.faces:
            v1_idx, v2_idx, v3_idx = face.vertex_indices
            
            # Bounds check
            if (v1_idx < len(vertices) and 
                v2_idx < len(vertices) and 
                v3_idx < len(vertices)):
                
                v1 = vertices[v1_idx].position
                v2 = vertices[v2_idx].position
                v3 = vertices[v3_idx].position
                
                glVertex3f(v1.x, v1.y, v1.z)
                glVertex3f(v2.x, v2.y, v2.z)
                glVertex3f(v3.x, v3.y, v3.z)
        
        glEnd()
    
    def mousePressEvent(self, event): #vers 1
        """Handle mouse press for camera control"""
        self.last_mouse_pos = event.pos()
        self.dragging = True
        self.drag_button = event.button()
    
    def mouseMoveEvent(self, event): #vers 1
        """Handle mouse move for camera control"""
        if not self.dragging:
            return
        
        dx = event.pos().x() - self.last_mouse_pos.x()
        dy = event.pos().y() - self.last_mouse_pos.y()
        
        if self.drag_button == Qt.MouseButton.LeftButton:
            # Rotate camera
            self.camera_rotation_y += dx * 0.5
            self.camera_rotation_x += dy * 0.5
            
            # Clamp X rotation
            self.camera_rotation_x = max(-89, min(89, self.camera_rotation_x))
            
        elif self.drag_button == Qt.MouseButton.MiddleButton:
            # Pan camera
            self.camera_pan_x += dx * 0.1
            self.camera_pan_y -= dy * 0.1
        
        self.last_mouse_pos = event.pos()
        self.update()
    
    def mouseReleaseEvent(self, event): #vers 1
        """Handle mouse release"""
        self.dragging = False
        self.drag_button = Qt.MouseButton.NoButton
    
    def wheelEvent(self, event): #vers 1
        """Handle mouse wheel for zoom"""
        delta = event.angleDelta().y()
        zoom_factor = 1.1 if delta > 0 else 0.9
        
        self.camera_distance *= zoom_factor
        self.camera_distance = max(5.0, min(500.0, self.camera_distance))
        
        self.update()
    
    def load_col_file(self, file_path: str) -> bool: #vers 1
        """Load COL file for display"""
        try:
            parser = COLParser(debug=True)
            self.col_model = parser.parse_col_file(file_path)
            
            if self.col_model:
                self.zoom_extents()
                self.update()
                return True
            
            return False
            
        except Exception as e:
            print(f"Error loading COL: {str(e)}")
            return False
    
    def zoom_extents(self): #vers 1
        """Zoom camera to fit model bounds"""
        if not self.col_model or not self.col_model.bounds:
            return
        
        bounds = self.col_model.bounds
        # Calculate appropriate distance based on bounding sphere radius
        self.camera_distance = bounds.radius * 3.0
        self.camera_distance = max(10.0, self.camera_distance)
    
    def reset_camera(self): #vers 1
        """Reset camera to default position"""
        self.camera_distance = 50.0
        self.camera_rotation_x = 20.0
        self.camera_rotation_y = 45.0
        self.camera_pan_x = 0.0
        self.camera_pan_y = 0.0
        self.update()
    
    def set_theme_colors(self, theme_colors: dict): #vers 1
        """Apply theme colors to viewport"""
        try:
            # Background
            bg = theme_colors.get('bg_primary', '#2a2a2a')
            self.bg_color = QColor(bg)
            
            # Grid
            grid = theme_colors.get('border', '#505050')
            self.grid_color = QColor(grid)
            
            # Mesh
            mesh = theme_colors.get('accent_primary', '#6496ff')
            self.mesh_color = QColor(mesh)
            
            # Bounds
            bounds = theme_colors.get('warning', '#ffff00')
            self.bounds_color = QColor(bounds)
            
            # Spheres
            sphere = theme_colors.get('error', '#ff6464')
            self.sphere_color = QColor(sphere)
            
            # Boxes
            box = theme_colors.get('success', '#64ff64')
            self.box_color = QColor(box)
            
            # Update OpenGL clear color
            if OPENGL_AVAILABLE:
                self.makeCurrent()
                glClearColor(
                    self.bg_color.redF(),
                    self.bg_color.greenF(),
                    self.bg_color.blueF(),
                    1.0
                )
            
            self.update()
            
        except Exception as e:
            print(f"Error applying theme: {str(e)}")


class COLViewerWidget(QWidget): #vers 1
    """Complete COL viewer widget with controls"""
    
    fileLoaded = pyqtSignal(str)
    
    def __init__(self, parent=None):
        super().__init__(parent)
        
        self.main_window = None
        self.setup_ui()
    
    def setup_ui(self): #vers 1
        """Setup viewer UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        
        # Info bar
        self.info_label = QLabel("No COL file loaded")
        self.info_label.setStyleSheet("padding: 4px; font-weight: bold;")
        layout.addWidget(self.info_label)
        
        # 3D viewport
        self.viewport = COL3DViewport()
        layout.addWidget(self.viewport, 1)
        
        # Control buttons
        controls_layout = QHBoxLayout()
        
        self.reset_btn = QPushButton("Reset Camera")
        self.reset_btn.clicked.connect(self.viewport.reset_camera)
        controls_layout.addWidget(self.reset_btn)
        
        self.fit_btn = QPushButton("Fit View")
        self.fit_btn.clicked.connect(self.viewport.zoom_extents)
        controls_layout.addWidget(self.fit_btn)
        
        controls_layout.addStretch()
        layout.addLayout(controls_layout)
    
    def load_col_file(self, file_path: str) -> bool: #vers 1
        """Load COL file"""
        if self.viewport.load_col_file(file_path):
            self.update_info_label(file_path)
            self.fileLoaded.emit(file_path)
            return True
        return False
    
    def update_info_label(self, file_path: str): #vers 2
        """Update info label with COL details including material info"""
        if self.viewport.col_model:
            model = self.viewport.col_model
            header = model.header
            
            # Basic info
            info_text = f"{os.path.basename(file_path)} | "
            info_text += f"COL{header.version} | "
            info_text += f"{len(model.spheres)} spheres | "
            info_text += f"{len(model.boxes)} boxes | "
            info_text += f"{len(model.vertices)} vertices | "
            info_text += f"{len(model.faces)} faces"
            
            # Detect game version for material names
            game = "SA" if header.version >= 2 else "VC"
            
            # Get unique materials used
            material_ids = set()
            for sphere in model.spheres:
                material_ids.add(sphere.material_id)
            for box in model.boxes:
                material_ids.add(box.material_id)
            for face in model.faces:
                material_ids.add(face.material_id)
            
            # Add material count
            if material_ids:
                info_text += f" | {len(material_ids)} materials"
            
            self.info_label.setText(info_text)
            
            # Log material details
            if hasattr(self, 'main_window') and self.main_window:
                mat_names = [get_material_name(mid, game) for mid in sorted(material_ids)]
                if mat_names and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Materials: {', '.join(mat_names[:5])}{'...' if len(mat_names) > 5 else ''}")
    
    def get_theme_colors(self) -> dict: #vers 1
        """Get theme colors from main window"""
        try:
            if self.main_window and hasattr(self.main_window, 'app_settings'):
                if hasattr(self.main_window.app_settings, 'get_theme_colors'):
                    return self.main_window.app_settings.get_theme_colors()
        except:
            pass
        
        # Fallback colors
        return {
            'bg_primary': '#2a2a2a',
            'border': '#505050',
            'accent_primary': '#6496ff',
            'warning': '#ffff00',
            'error': '#ff6464',
            'success': '#64ff64'
        }
    
    def apply_theme(self): #vers 1
        """Apply current theme to viewport"""
        theme_colors = self.get_theme_colors()
        self.viewport.set_theme_colors(theme_colors)
        
        # Apply theme to controls
        bg = theme_colors.get('bg_secondary', '#f0f0f0')
        text = theme_colors.get('text_primary', '#000000')
        border = theme_colors.get('border', '#cccccc')
        
        self.info_label.setStyleSheet(f"""
            QLabel {{
                background-color: {bg};
                color: {text};
                border: 1px solid {border};
                padding: 4px;
                font-weight: bold;
            }}
        """)


# Export main classes
__all__ = ['COLViewerWidget', 'COL3DViewport']
