#this belongs in components/Col_Editor/depends/col_preview_generator.py - Version: 1
# X-Seti - October20 2025 - IMG Factory 1.5 - COL Preview Generator

"""
COL Preview Generator - Creates 2D thumbnail previews of COL collision models
Generates icon-sized previews showing collision geometry from top-down view
Used for model list icons and file browser thumbnails
"""

import math
from typing import Optional, Tuple, List
from PyQt6.QtCore import QRect, QPoint, QSize, Qt
from PyQt6.QtGui import QPixmap, QPainter, QColor, QPen, QBrush, QPolygon

##Methods list -
# calculate_bounds
# create_col_file_preview
# create_col_model_preview
# draw_box_2d
# draw_bounds_2d
# draw_face_mesh_2d
# draw_sphere_2d
# generate_preview
# project_to_2d

##Classes -
# COLPreviewGenerator

class COLPreviewGenerator:
    """Generates 2D preview thumbnails for COL collision models"""
    
    DEFAULT_SIZE = 64
    PREVIEW_SIZE = 128
    LARGE_SIZE = 256
    
    def __init__(self): #vers 1
        """Initialize preview generator with default settings"""
        self.bg_color = QColor(30, 30, 30)
        self.mesh_color = QColor(0, 255, 0)
        self.wireframe_color = QColor(100, 255, 100)
        self.sphere_color = QColor(0, 200, 255)
        self.box_color = QColor(255, 200, 0)
        self.bounds_color = QColor(255, 0, 0)
        self.padding = 8
    
    def generate_preview(self, col_model, size=DEFAULT_SIZE, view_angle='top'): #vers 1
        """
        Generate preview pixmap for COL model
        
        Args:
            col_model: COL model object with vertices, faces, spheres, boxes
            size: Output size in pixels (square)
            view_angle: 'top', 'front', 'side' or 'iso' for isometric
            
        Returns:
            QPixmap with rendered preview
        """
        if not col_model:
            return self._create_empty_preview(size)
        
        pixmap = QPixmap(size, size)
        pixmap.fill(self.bg_color)
        
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        
        try:
            # Calculate bounding box for camera positioning
            bounds = self.calculate_bounds(col_model)
            if not bounds:
                return pixmap
            
            min_v, max_v = bounds
            center = ((min_v[0] + max_v[0]) / 2, 
                     (min_v[1] + max_v[1]) / 2, 
                     (min_v[2] + max_v[2]) / 2)
            scale = self._calculate_scale(min_v, max_v, size)
            
            # Draw collision elements
            if hasattr(col_model, 'faces') and hasattr(col_model, 'vertices'):
                self.draw_face_mesh_2d(painter, col_model, center, scale, size, view_angle)
            
            if hasattr(col_model, 'spheres'):
                for sphere in col_model.spheres:
                    self.draw_sphere_2d(painter, sphere, center, scale, size, view_angle)
            
            if hasattr(col_model, 'boxes'):
                for box in col_model.boxes:
                    self.draw_box_2d(painter, box, center, scale, size, view_angle)
            
            # Draw bounding box
            self.draw_bounds_2d(painter, min_v, max_v, center, scale, size, view_angle)
            
        finally:
            painter.end()
        
        return pixmap
    
    def calculate_bounds(self, col_model): #vers 1
        """Calculate bounding box for entire collision model"""
        min_x = min_y = min_z = float('inf')
        max_x = max_y = max_z = float('-inf')
        
        found_any = False
        
        # Check vertices
        if hasattr(col_model, 'vertices'):
            for v in col_model.vertices:
                if hasattr(v, 'x'):
                    min_x = min(min_x, v.x)
                    max_x = max(max_x, v.x)
                    min_y = min(min_y, v.y)
                    max_y = max(max_y, v.y)
                    min_z = min(min_z, v.z)
                    max_z = max(max_z, v.z)
                    found_any = True
        
        # Check spheres
        if hasattr(col_model, 'spheres'):
            for s in col_model.spheres:
                if hasattr(s, 'center') and hasattr(s, 'radius'):
                    min_x = min(min_x, s.center.x - s.radius)
                    max_x = max(max_x, s.center.x + s.radius)
                    min_y = min(min_y, s.center.y - s.radius)
                    max_y = max(max_y, s.center.y + s.radius)
                    min_z = min(min_z, s.center.z - s.radius)
                    max_z = max(max_z, s.center.z + s.radius)
                    found_any = True
        
        # Check boxes
        if hasattr(col_model, 'boxes'):
            for b in col_model.boxes:
                if hasattr(b, 'min') and hasattr(b, 'max'):
                    min_x = min(min_x, b.min.x)
                    max_x = max(max_x, b.max.x)
                    min_y = min(min_y, b.min.y)
                    max_y = max(max_y, b.max.y)
                    min_z = min(min_z, b.min.z)
                    max_z = max(max_z, b.max.z)
                    found_any = True
        
        if not found_any:
            return None
        
        return ((min_x, min_y, min_z), (max_x, max_y, max_z))
    
    def project_to_2d(self, x, y, z, view_angle='top'): #vers 1
        """Project 3D coordinates to 2D based on view angle"""
        if view_angle == 'top':
            return (x, z)
        elif view_angle == 'front':
            return (x, y)
        elif view_angle == 'side':
            return (z, y)
        elif view_angle == 'iso':
            # Isometric projection
            iso_x = x - z
            iso_y = y + (x + z) * 0.5
            return (iso_x, iso_y)
        else:
            return (x, z)
    
    def draw_face_mesh_2d(self, painter, col_model, center, scale, size, view_angle): #vers 1
        """Draw collision mesh faces in 2D"""
        if not hasattr(col_model, 'faces') or not hasattr(col_model, 'vertices'):
            return
        
        vertices = col_model.vertices
        faces = col_model.faces
        
        painter.setPen(QPen(self.wireframe_color, 1))
        painter.setBrush(QBrush(QColor(0, 255, 0, 30)))
        
        for face in faces:
            if not hasattr(face, 'indices') or len(face.indices) < 3:
                continue
            
            points = []
            for idx in face.indices[:3]:
                if idx < len(vertices):
                    v = vertices[idx]
                    if hasattr(v, 'x'):
                        x2d, y2d = self.project_to_2d(v.x - center[0], 
                                                      v.y - center[1], 
                                                      v.z - center[2], 
                                                      view_angle)
                        screen_x = int(size / 2 + x2d * scale)
                        screen_y = int(size / 2 - y2d * scale)
                        points.append(QPoint(screen_x, screen_y))
            
            if len(points) >= 3:
                polygon = QPolygon(points)
                painter.drawPolygon(polygon)
    
    def draw_sphere_2d(self, painter, sphere, center, scale, size, view_angle): #vers 1
        """Draw collision sphere in 2D"""
        if not hasattr(sphere, 'center') or not hasattr(sphere, 'radius'):
            return
        
        x2d, y2d = self.project_to_2d(sphere.center.x - center[0],
                                      sphere.center.y - center[1],
                                      sphere.center.z - center[2],
                                      view_angle)
        
        screen_x = int(size / 2 + x2d * scale)
        screen_y = int(size / 2 - y2d * scale)
        screen_radius = int(sphere.radius * scale)
        
        painter.setPen(QPen(self.sphere_color, 2))
        painter.setBrush(QBrush(QColor(0, 200, 255, 50)))
        painter.drawEllipse(screen_x - screen_radius, screen_y - screen_radius,
                           screen_radius * 2, screen_radius * 2)
    
    def draw_box_2d(self, painter, box, center, scale, size, view_angle): #vers 1
        """Draw collision box in 2D"""
        if not hasattr(box, 'min') or not hasattr(box, 'max'):
            return
        
        # Get all 8 corners of box
        min_v = box.min
        max_v = box.max
        
        corners_3d = [
            (min_v.x, min_v.y, min_v.z),
            (max_v.x, min_v.y, min_v.z),
            (max_v.x, max_v.y, min_v.z),
            (min_v.x, max_v.y, min_v.z),
            (min_v.x, min_v.y, max_v.z),
            (max_v.x, min_v.y, max_v.z),
            (max_v.x, max_v.y, max_v.z),
            (min_v.x, max_v.y, max_v.z)
        ]
        
        # Project to 2D
        corners_2d = []
        for x, y, z in corners_3d:
            x2d, y2d = self.project_to_2d(x - center[0], y - center[1], 
                                         z - center[2], view_angle)
            screen_x = int(size / 2 + x2d * scale)
            screen_y = int(size / 2 - y2d * scale)
            corners_2d.append(QPoint(screen_x, screen_y))
        
        painter.setPen(QPen(self.box_color, 2))
        painter.setBrush(QBrush(QColor(255, 200, 0, 50)))
        
        # Draw box outline based on view
        if view_angle == 'top':
            # Draw top face
            points = [corners_2d[0], corners_2d[1], corners_2d[2], corners_2d[3]]
            polygon = QPolygon(points)
            painter.drawPolygon(polygon)
        elif view_angle == 'front':
            # Draw front face
            points = [corners_2d[0], corners_2d[1], corners_2d[5], corners_2d[4]]
            polygon = QPolygon(points)
            painter.drawPolygon(polygon)
        else:
            # Draw as rectangle for simplified view
            min_2d_x = min(p.x() for p in corners_2d)
            max_2d_x = max(p.x() for p in corners_2d)
            min_2d_y = min(p.y() for p in corners_2d)
            max_2d_y = max(p.y() for p in corners_2d)
            painter.drawRect(min_2d_x, min_2d_y, max_2d_x - min_2d_x, max_2d_y - min_2d_y)
    
    def draw_bounds_2d(self, painter, min_v, max_v, center, scale, size, view_angle): #vers 1
        """Draw bounding box outline in 2D"""
        corners_3d = [
            (min_v[0], min_v[1], min_v[2]),
            (max_v[0], min_v[1], min_v[2]),
            (max_v[0], max_v[1], min_v[2]),
            (min_v[0], max_v[1], min_v[2]),
            (min_v[0], min_v[1], max_v[2]),
            (max_v[0], min_v[1], max_v[2]),
            (max_v[0], max_v[1], max_v[2]),
            (min_v[0], max_v[1], max_v[2])
        ]
        
        corners_2d = []
        for x, y, z in corners_3d:
            x2d, y2d = self.project_to_2d(x - center[0], y - center[1], 
                                         z - center[2], view_angle)
            screen_x = int(size / 2 + x2d * scale)
            screen_y = int(size / 2 - y2d * scale)
            corners_2d.append(QPoint(screen_x, screen_y))
        
        painter.setPen(QPen(self.bounds_color, 1, Qt.PenStyle.DashLine))
        
        # Draw bounding box edges
        min_2d_x = min(p.x() for p in corners_2d)
        max_2d_x = max(p.x() for p in corners_2d)
        min_2d_y = min(p.y() for p in corners_2d)
        max_2d_y = max(p.y() for p in corners_2d)
        painter.drawRect(min_2d_x, min_2d_y, max_2d_x - min_2d_x, max_2d_y - min_2d_y)
    
    def create_col_model_preview(self, col_model, size=DEFAULT_SIZE): #vers 1
        """
        Create preview for single COL model
        Convenience method with default settings
        """
        return self.generate_preview(col_model, size, 'iso')
    
    def create_col_file_preview(self, col_file, size=DEFAULT_SIZE): #vers 1
        """
        Create preview for entire COL file (first model or composite)
        
        Args:
            col_file: COL file object with models list
            size: Output size in pixels
            
        Returns:
            QPixmap with preview
        """
        if not col_file or not hasattr(col_file, 'models') or not col_file.models:
            return self._create_empty_preview(size)
        
        # Use first model for preview
        return self.generate_preview(col_file.models[0], size, 'iso')
    
    def _calculate_scale(self, min_v, max_v, size): #vers 1
        """Calculate scale factor to fit model in preview"""
        width = max_v[0] - min_v[0]
        height = max_v[1] - min_v[1]
        depth = max_v[2] - min_v[2]
        
        max_dimension = max(width, height, depth)
        
        if max_dimension == 0:
            return 1.0
        
        available_size = size - (self.padding * 2)
        return available_size / max_dimension
    
    def _create_empty_preview(self, size): #vers 1
        """Create empty preview pixmap"""
        pixmap = QPixmap(size, size)
        pixmap.fill(self.bg_color)
        
        painter = QPainter(pixmap)
        painter.setPen(QPen(QColor(100, 100, 100)))
        painter.drawText(pixmap.rect(), Qt.AlignmentFlag.AlignCenter, "No Data")
        painter.end()
        
        return pixmap
    
    def set_colors(self, bg=None, mesh=None, wireframe=None, sphere=None, box=None, bounds=None): #vers 1
        """Update preview colors"""
        if bg:
            self.bg_color = bg
        if mesh:
            self.mesh_color = mesh
        if wireframe:
            self.wireframe_color = wireframe
        if sphere:
            self.sphere_color = sphere
        if box:
            self.box_color = box
        if bounds:
            self.bounds_color = bounds


def create_col_preview(col_model, size=64, view='iso'): #vers 1
    """
    Standalone function to create COL preview
    
    Args:
        col_model: COL model object
        size: Preview size in pixels
        view: View angle ('top', 'front', 'side', 'iso')
        
    Returns:
        QPixmap with preview
    """
    generator = COLPreviewGenerator()
    return generator.generate_preview(col_model, size, view)


def create_col_thumbnail(col_model, size=32): #vers 1
    """Create small thumbnail for list views"""
    generator = COLPreviewGenerator()
    return generator.generate_preview(col_model, size, 'iso')


def create_col_icon(col_model, size=16): #vers 1
    """Create tiny icon for tree views"""
    generator = COLPreviewGenerator()
    return generator.generate_preview(col_model, size, 'top')


# Export functions and classes
__all__ = [
    'COLPreviewGenerator',
    'create_col_preview',
    'create_col_thumbnail', 
    'create_col_icon'
]