#this belongs in components/col_structure_manager.py - Version: 1
# X-Seti - July23 2025 - IMG Factory 1.5 - COL Structure Manager - Complete Port
# Ported from col_structure_manager.py-old with 100% functionality preservation
# ONLY debug system changed from old COL debug to img_debugger

"""
COL Structure Manager - COMPLETE PORT
Handles COL file structure parsing, validation and data management
Uses IMG debug system throughout - preserves 100% original functionality
"""

import struct
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass

# Import IMG debug system - NO fallback code
try:
    from apps.debug.debug_functions import img_debugger
except ImportError:
    # Fallback debug function if import fails
    class DebugFallback:
        def debug(self, msg): print(f"DEBUG: {msg}")
        def error(self, msg): print(f"ERROR: {msg}")
        def warning(self, msg): print(f"WARNING: {msg}")
        def success(self, msg): print(f"SUCCESS: {msg}")
    img_debugger = DebugFallback()

##Functions list -
# _estimate_model_size
# get_model_statistics
# parse_col_bounds
# parse_col_boxes
# parse_col_faces
# parse_col_header
# parse_col_spheres
# parse_col_vertices
# parse_complete_model
# validate_model_structure

##Classes list -
# COLBounds
# COLBox
# COLFace
# COLHeader
# COLModelStructure
# COLSphere
# COLStructureManager
# COLVertex

@dataclass
class COLHeader:
    """COL file header structure"""
    signature: str
    file_size: int
    model_name: str
    model_id: int
    version: int

@dataclass
class COLBounds:
    """COL bounding data structure"""
    radius: float
    center: Tuple[float, float, float]
    min_point: Tuple[float, float, float]
    max_point: Tuple[float, float, float]

@dataclass
class COLSphere:
    """COL collision sphere structure"""
    center: Tuple[float, float, float]
    radius: float
    material: int
    flags: int = 0

@dataclass
class COLBox:
    """COL collision box structure"""
    min_point: Tuple[float, float, float]
    max_point: Tuple[float, float, float]
    material: int
    flags: int = 0

@dataclass
class COLVertex:
    """COL mesh vertex structure"""
    position: Tuple[float, float, float]

@dataclass
class COLFace:
    """COL mesh face structure"""
    vertex_indices: Tuple[int, int, int]
    material: int
    light: int = 0
    flags: int = 0

@dataclass
class COLModelStructure:
    """Complete COL model structure"""
    header: COLHeader
    bounds: COLBounds
    spheres: List[COLSphere]
    boxes: List[COLBox]
    vertices: List[COLVertex]
    faces: List[COLFace]
    face_groups: List = None
    shadow_vertices: List[COLVertex] = None
    shadow_faces: List[COLFace] = None

class COLStructureManager:
    """Manages COL file structure parsing and validation"""
    
    def __init__(self):
        self.debug = True
        
    def parse_col_header(self, data: bytes, offset: int = 0) -> Tuple[COLHeader, int]: #vers 1
        """Parse COL file header and return header + new offset"""
        try:
            if len(data) < offset + 32:
                raise ValueError("Data too short for COL header")
            
            # Read signature (4 bytes)
            signature = data[offset:offset+4].decode('ascii', errors='ignore')
            offset += 4
            
            # Read file size (4 bytes)
            file_size = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            
            # Read model name (22 bytes, null-terminated)
            name_bytes = data[offset:offset+22]
            model_name = name_bytes.split(b'\x00')[0].decode('ascii', errors='ignore')
            offset += 22
            
            # Read model ID (2 bytes)
            model_id = struct.unpack('<H', data[offset:offset+2])[0]
            offset += 2
            
            # Determine version from signature
            version = 1
            if signature.startswith('COL'):
                version_char = signature[3] if len(signature) > 3 else '1'
                if version_char.isdigit():
                    version = int(version_char)
            
            header = COLHeader(
                signature=signature,
                file_size=file_size,
                model_name=model_name,
                model_id=model_id,
                version=version
            )
            
            if self.debug:
                img_debugger.debug(f"🔍 COL Header: {signature} v{version}, Model: '{model_name}', Size: {file_size}")
            
            return header, offset
            
        except Exception as e:
            raise ValueError(f"Error parsing COL header: {str(e)}")
    
    def parse_col_bounds(self, data: bytes, offset: int, version: int) -> Tuple[COLBounds, int]: #vers 1
        """Parse COL bounding data based on version"""
        try:
            if version == 1:
                # COL1: radius + center + min + max (40 bytes)
                if len(data) < offset + 40:
                    raise ValueError("Data too short for COL1 bounds")
                
                radius = struct.unpack('<f', data[offset:offset+4])[0]
                offset += 4
                center = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                min_point = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                max_point = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
            else:
                # COL2/3: min + max + center + radius (28 bytes)
                if len(data) < offset + 28:
                    raise ValueError(f"Data too short for COL{version} bounds")
                
                min_point = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                max_point = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                center = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                radius = struct.unpack('<f', data[offset:offset+4])[0]
                offset += 4
            
            bounds = COLBounds(
                radius=radius,
                center=center,
                min_point=min_point,
                max_point=max_point
            )
            
            if self.debug:
                img_debugger.debug(f"📏 Bounds: radius={radius:.2f}, center={center}")
            
            return bounds, offset
            
        except Exception as e:
            raise ValueError(f"Error parsing COL bounds: {str(e)}")
    
    def parse_col_spheres(self, data: bytes, offset: int, count: int, version: int) -> Tuple[List[COLSphere], int]: #vers 1
        """Parse COL collision spheres"""
        try:
            spheres = []
            
            if version == 1:
                # COL1: center + radius + material + flags (24 bytes each)
                sphere_size = 24
            else:
                # COL2/3: center + radius + material (16 bytes each)
                sphere_size = 16
            
            if len(data) < offset + (count * sphere_size):
                raise ValueError(f"Data too short for {count} spheres")
            
            for i in range(count):
                # Parse center (12 bytes)
                center = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
                # Parse radius (4 bytes)
                radius = struct.unpack('<f', data[offset:offset+4])[0]
                offset += 4
                
                if version == 1:
                    # Parse material and flags (4 bytes each)
                    material = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                    flags = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                else:
                    # Parse material only (4 bytes)
                    material = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                    flags = 0
                
                sphere = COLSphere(
                    center=center,
                    radius=radius,
                    material=material,
                    flags=flags
                )
                spheres.append(sphere)
            
            if self.debug:
                img_debugger.debug(f"🔵 Parsed {len(spheres)} collision spheres")
            
            return spheres, offset
            
        except Exception as e:
            raise ValueError(f"Error parsing COL spheres: {str(e)}")
    
    def parse_col_boxes(self, data: bytes, offset: int, count: int, version: int) -> Tuple[List[COLBox], int]: #vers 1
        """Parse COL collision boxes"""
        try:
            boxes = []
            
            if version == 1:
                # COL1: min + max + material + flags (32 bytes each)
                box_size = 32
            else:
                # COL2/3: min + max + material (28 bytes each)
                box_size = 28
            
            if len(data) < offset + (count * box_size):
                raise ValueError(f"Data too short for {count} boxes")
            
            for i in range(count):
                # Parse min point (12 bytes)
                min_point = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
                # Parse max point (12 bytes)
                max_point = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
                if version == 1:
                    # Parse material and flags (4 bytes each)
                    material = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                    flags = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                else:
                    # Parse material only (4 bytes)
                    material = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                    flags = 0
                
                box = COLBox(
                    min_point=min_point,
                    max_point=max_point,
                    material=material,
                    flags=flags
                )
                boxes.append(box)
            
            if self.debug:
                img_debugger.debug(f"Parsed {len(boxes)} collision boxes")
            
            return boxes, offset
            
        except Exception as e:
            raise ValueError(f"Error parsing COL boxes: {str(e)}")
    
    def parse_col_vertices(self, data: bytes, offset: int, count: int) -> Tuple[List[COLVertex], int]: #vers 1
        """Parse COL mesh vertices"""
        try:
            vertices = []
            vertex_size = 12  # 3 floats (x, y, z)
            
            if len(data) < offset + (count * vertex_size):
                raise ValueError(f"Data too short for {count} vertices")
            
            for i in range(count):
                # Parse position (12 bytes)
                position = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
                vertex = COLVertex(position=position)
                vertices.append(vertex)
            
            if self.debug:
                img_debugger.debug(f"📍 Parsed {len(vertices)} mesh vertices")
            
            return vertices, offset
            
        except Exception as e:
            raise ValueError(f"Error parsing COL vertices: {str(e)}")
    
    def parse_col_faces(self, data: bytes, offset: int, count: int, version: int) -> Tuple[List[COLFace], int]: #vers 1
        """Parse COL mesh faces"""
        try:
            faces = []
            
            if version == 1:
                # COL1: vertex_indices + material + light + flags (16 bytes each)
                face_size = 16
            else:
                # COL2/3: vertex_indices + material + light (14 bytes each)
                face_size = 14
            
            if len(data) < offset + (count * face_size):
                raise ValueError(f"Data too short for {count} faces")
            
            for i in range(count):
                # Parse vertex indices (6 bytes - 3 shorts)
                vertex_indices = struct.unpack('<HHH', data[offset:offset+6])
                offset += 6
                
                # Parse material (2 bytes)
                material = struct.unpack('<H', data[offset:offset+2])[0]
                offset += 2
                
                # Parse light (2 bytes)
                light = struct.unpack('<H', data[offset:offset+2])[0]
                offset += 2
                
                if version == 1:
                    # Parse flags (4 bytes)
                    flags = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                else:
                    # Skip padding (2 bytes) and set flags to 0
                    offset += 2
                    flags = 0
                
                face = COLFace(
                    vertex_indices=vertex_indices,
                    material=material,
                    light=light,
                    flags=flags
                )
                faces.append(face)
            
            if self.debug:
                img_debugger.debug(f"🔺 Parsed {len(faces)} mesh faces")
            
            return faces, offset
            
        except Exception as e:
            raise ValueError(f"Error parsing COL faces: {str(e)}")
    
    def parse_complete_model(self, data: bytes, offset: int = 0) -> COLModelStructure: #vers 1
        """Parse complete COL model structure"""
        try:
            start_offset = offset
            
            # Parse header
            header, offset = self.parse_col_header(data, offset)
            
            # Parse bounds
            bounds, offset = self.parse_col_bounds(data, offset, header.version)
            
            # Parse collision data counts
            if header.version == 1:
                # COL1: num_spheres, num_unknown, num_boxes, num_vertices, num_faces
                if len(data) < offset + 20:
                    raise ValueError("Data too short for COL1 counts")
                
                num_spheres = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_unknown = struct.unpack('<I', data[offset:offset+4])[0]  # Unknown data
                offset += 4
                num_boxes = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_vertices = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_faces = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                
            else:
                # COL2/3: num_spheres, num_boxes, num_faces, num_vertices
                if len(data) < offset + 16:
                    raise ValueError(f"Data too short for COL{header.version} counts")
                
                num_spheres = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_boxes = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_faces = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_vertices = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_unknown = 0
            
            if self.debug:
                img_debugger.debug(f"📊 Counts: {num_spheres} spheres, {num_boxes} boxes, {num_vertices} vertices, {num_faces} faces")
            
            # Parse collision spheres
            spheres, offset = self.parse_col_spheres(data, offset, num_spheres, header.version)
            
            # Skip unknown data for COL1
            if header.version == 1 and num_unknown > 0:
                offset += num_unknown * 4  # Assume 4 bytes per unknown item
            
            # Parse collision boxes
            boxes, offset = self.parse_col_boxes(data, offset, num_boxes, header.version)
            
            # Parse mesh vertices
            vertices, offset = self.parse_col_vertices(data, offset, num_vertices)
            
            # Parse mesh faces
            faces, offset = self.parse_col_faces(data, offset, num_faces, header.version)
            
            # Create complete model structure
            model = COLModelStructure(
                header=header,
                bounds=bounds,
                spheres=spheres,
                boxes=boxes,
                vertices=vertices,
                faces=faces
            )
            
            total_size = offset - start_offset
            if self.debug:
                img_debugger.success(f"✅ Complete model parsed: {total_size} bytes processed")
            
            return model
            
        except Exception as e:
            img_debugger.error(f"❌ Error parsing complete model: {str(e)}")
            raise
    
    def get_model_statistics(self, model: COLModelStructure) -> Dict[str, int]: #vers 1
        """Get comprehensive model statistics"""
        try:
            stats = {
                'version': model.header.version,
                'file_size': model.header.file_size,
                'model_id': model.header.model_id,
                'spheres': len(model.spheres),
                'boxes': len(model.boxes),
                'vertices': len(model.vertices),
                'faces': len(model.faces),
                'total_collision_objects': len(model.spheres) + len(model.boxes),
                'estimated_memory': self._estimate_model_size(model)
            }
            
            if self.debug:
                img_debugger.debug(f"📈 Model stats: {stats['total_collision_objects']} collision objects, {stats['vertices']} vertices")
            
            return stats
            
        except Exception as e:
            img_debugger.error(f"Get model statistics error: {str(e)}")
            return {}
    
    def _estimate_model_size(self, model: COLModelStructure) -> int: #vers 1
        """Estimate memory usage of model in bytes"""
        try:
            size = 0
            
            # Header
            size += 32
            
            # Bounds
            if model.header.version == 1:
                size += 40
            else:
                size += 28
            
            # Counts
            if model.header.version == 1:
                size += 20
            else:
                size += 16
            
            # Spheres
            if model.header.version == 1:
                size += len(model.spheres) * 20
            else:
                size += len(model.spheres) * 16
            
            # Boxes
            if model.header.version == 1:
                size += len(model.boxes) * 32
            else:
                size += len(model.boxes) * 28
            
            # Vertices
            size += len(model.vertices) * 12
            
            # Faces
            if model.header.version == 1:
                size += len(model.faces) * 16
            else:
                size += len(model.faces) * 14
            
            return size
            
        except Exception as e:
            img_debugger.error(f"Estimate model size error: {str(e)}")
            return 0
    
    def validate_model_structure(self, model: COLModelStructure) -> Tuple[bool, List[str]]: #vers 1
        """Validate model structure and return issues"""
        try:
            issues = []
            
            # Validate header
            if not model.header.signature.startswith(('COL', 'COLL')):
                issues.append(f"Invalid signature: {model.header.signature}")
            
            if model.header.file_size <= 0:
                issues.append(f"Invalid file size: {model.header.file_size}")
            
            if model.header.version not in [1, 2, 3, 4]:
                issues.append(f"Unsupported version: {model.header.version}")
            
            # Validate bounds
            if model.bounds.radius < 0:
                issues.append(f"Invalid radius: {model.bounds.radius}")
            
            # Validate spheres
            for i, sphere in enumerate(model.spheres):
                if sphere.radius <= 0:
                    issues.append(f"Sphere {i}: invalid radius {sphere.radius}")
            
            # Validate boxes
            for i, box in enumerate(model.boxes):
                if any(min_val >= max_val for min_val, max_val in zip(box.min_point, box.max_point)):
                    issues.append(f"Box {i}: invalid bounds {box.min_point} to {box.max_point}")
            
            # Validate faces
            max_vertex_index = len(model.vertices) - 1
            for i, face in enumerate(model.faces):
                for j, vertex_idx in enumerate(face.vertex_indices):
                    if vertex_idx > max_vertex_index:
                        issues.append(f"Face {i}: vertex index {vertex_idx} out of range (max: {max_vertex_index})")
            
            is_valid = len(issues) == 0
            
            if self.debug:
                if is_valid:
                    img_debugger.success("✅ Model structure validation passed")
                else:
                    img_debugger.warning(f"⚠️ Model validation found {len(issues)} issues")
            
            return is_valid, issues
            
        except Exception as e:
            img_debugger.error(f"Validate model structure error: {str(e)}")
            return False, [f"Validation error: {str(e)}"]

# Export classes and functions
__all__ = [
    'COLHeader', 'COLBounds', 'COLSphere', 'COLBox', 'COLVertex', 'COLFace',
    'COLModelStructure', 'COLStructureManager'
]
