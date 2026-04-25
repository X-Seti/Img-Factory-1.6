#this belongs in components/col_viewer/depends/COL_Parser.py - Version: 1
# X-Seti - October22 2025 - IMG Factory 1.5 - COL Parser

"""
COL Parser - Clean collision file parser from scratch
Handles GTA III, Vice City, and San Andreas COL formats
No fallback code - works or fails clearly
"""

import struct
from dataclasses import dataclass
from typing import List, Tuple, Optional

##Methods list -
# parse_bounds_col1
# parse_bounds_col2_3
# parse_boxes
# parse_col_file
# parse_faces
# parse_header
# parse_model
# parse_spheres
# parse_vertices

##Classes -
# COLBounds
# COLBox
# COLFace
# COLHeader
# COLModel
# COLParser
# COLSphere
# COLVertex
# Vector3

@dataclass
class Vector3: #vers 1
    """3D vector"""
    x: float
    y: float
    z: float

@dataclass
class COLHeader: #vers 1
    """COL file header"""
    signature: str
    file_size: int
    model_name: str
    model_id: int
    version: int

@dataclass
class COLBounds: #vers 1
    """COL bounding box"""
    min: Vector3
    max: Vector3
    center: Vector3
    radius: float

@dataclass
class COLSphere: #vers 1
    """COL collision sphere"""
    center: Vector3
    radius: float
    material_id: int
    flags: int = 0

@dataclass
class COLBox: #vers 1
    """COL collision box"""
    min_point: Vector3
    max_point: Vector3
    material_id: int
    flags: int = 0

@dataclass
class COLVertex: #vers 1
    """COL mesh vertex"""
    position: Vector3

@dataclass
class COLFace: #vers 1
    """COL mesh face (triangle)"""
    vertex_indices: Tuple[int, int, int]
    material_id: int
    light: int
    flags: int = 0

@dataclass
class COLModel: #vers 1
    """Complete COL model"""
    header: COLHeader
    bounds: COLBounds
    spheres: List[COLSphere]
    boxes: List[COLBox]
    vertices: List[COLVertex]
    faces: List[COLFace]


class COLParser: #vers 1
    """Clean COL file parser"""
    
    def __init__(self, debug: bool = False):
        self.debug = debug
        self.error_message = ""
    
    def parse_col_file(self, file_path: str) -> Optional[COLModel]: #vers 1
        """Parse COL file from path"""
        try:
            with open(file_path, 'rb') as f:
                data = f.read()
            
            if len(data) < 32:
                self.error_message = "File too small for COL header"
                return None
            
            return self.parse_model(data, 0)
            
        except FileNotFoundError:
            self.error_message = f"File not found: {file_path}"
            return None
        except Exception as e:
            self.error_message = f"Parse failed: {str(e)}"
            return None
    
    def parse_model(self, data: bytes, offset: int = 0) -> Optional[COLModel]: #vers 1
        """Parse single COL model from data"""
        try:
            # Parse header
            header, offset = self.parse_header(data, offset)
            if not header:
                return None
            
            # Parse bounds based on version
            if header.version == 1:
                bounds, offset = self.parse_bounds_col1(data, offset)
            else:
                bounds, offset = self.parse_bounds_col2_3(data, offset)
            
            if not bounds:
                return None
            
            # Parse counts based on version
            if header.version == 1:
                # COL1: spheres, unknown, boxes, vertices, faces
                num_spheres, offset = self._read_uint(data, offset)
                num_unknown, offset = self._read_uint(data, offset)
                num_boxes, offset = self._read_uint(data, offset)
                num_vertices, offset = self._read_uint(data, offset)
                num_faces, offset = self._read_uint(data, offset)
            else:
                # COL2/3: spheres, boxes, faces, vertices
                num_spheres, offset = self._read_uint(data, offset)
                num_boxes, offset = self._read_uint(data, offset)
                num_faces, offset = self._read_uint(data, offset)
                num_vertices, offset = self._read_uint(data, offset)
            
            # Parse collision objects
            spheres, offset = self.parse_spheres(data, offset, num_spheres, header.version)
            boxes, offset = self.parse_boxes(data, offset, num_boxes, header.version)
            vertices, offset = self.parse_vertices(data, offset, num_vertices)
            faces, offset = self.parse_faces(data, offset, num_faces, header.version)
            
            # Build model
            model = COLModel(
                header=header,
                bounds=bounds,
                spheres=spheres,
                boxes=boxes,
                vertices=vertices,
                faces=faces
            )
            
            if self.debug:
                print(f"✅ Parsed: {header.model_name} (COL{header.version})")
                print(f"   {len(spheres)} spheres, {len(boxes)} boxes")
                print(f"   {len(vertices)} vertices, {len(faces)} faces")
            
            return model
            
        except Exception as e:
            self.error_message = f"Model parse error: {str(e)}"
            return None
    
    def parse_header(self, data: bytes, offset: int) -> Tuple[Optional[COLHeader], int]: #vers 1
        """Parse COL header (32 bytes)"""
        try:
            if len(data) < offset + 32:
                raise ValueError("Not enough data for header")
            
            # Signature (4 bytes)
            signature = data[offset:offset+4].decode('ascii', errors='ignore')
            offset += 4
            
            # File size (4 bytes)
            file_size = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            
            # Model name (22 bytes, null-terminated)
            name_bytes = data[offset:offset+22]
            model_name = name_bytes.split(b'\x00')[0].decode('ascii', errors='ignore')
            offset += 22
            
            # Model ID (2 bytes)
            model_id = struct.unpack('<H', data[offset:offset+2])[0]
            offset += 2
            
            # Determine version
            version = 1
            if signature == 'COLL':
                version = 1
            elif signature.startswith('COL'):
                version_char = signature[3] if len(signature) > 3 else '1'
                if version_char in '\x02\x03\x04':
                    version = ord(version_char)
                elif version_char.isdigit():
                    version = int(version_char)
            
            header = COLHeader(
                signature=signature,
                file_size=file_size,
                model_name=model_name,
                model_id=model_id,
                version=version
            )
            
            return header, offset
            
        except Exception as e:
            self.error_message = f"Header parse error: {str(e)}"
            return None, offset
    
    def parse_bounds_col1(self, data: bytes, offset: int) -> Tuple[Optional[COLBounds], int]: #vers 1
        """Parse COL1 bounding data (40 bytes)"""
        try:
            if len(data) < offset + 40:
                raise ValueError("Not enough data for COL1 bounds")
            
            # Radius (4 bytes)
            radius = struct.unpack('<f', data[offset:offset+4])[0]
            offset += 4
            
            # Center (12 bytes)
            center_x, center_y, center_z = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            # Min point (12 bytes)
            min_x, min_y, min_z = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            # Max point (12 bytes)
            max_x, max_y, max_z = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            bounds = COLBounds(
                min=Vector3(min_x, min_y, min_z),
                max=Vector3(max_x, max_y, max_z),
                center=Vector3(center_x, center_y, center_z),
                radius=radius
            )
            
            return bounds, offset
            
        except Exception as e:
            self.error_message = f"COL1 bounds parse error: {str(e)}"
            return None, offset
    
    def parse_bounds_col2_3(self, data: bytes, offset: int) -> Tuple[Optional[COLBounds], int]: #vers 1
        """Parse COL2/3 bounding data (28 bytes)"""
        try:
            if len(data) < offset + 28:
                raise ValueError("Not enough data for COL2/3 bounds")
            
            # Min point (12 bytes)
            min_x, min_y, min_z = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            # Max point (12 bytes)
            max_x, max_y, max_z = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            # Center (12 bytes) 
            center_x, center_y, center_z = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            # Radius (4 bytes)
            radius = struct.unpack('<f', data[offset:offset+4])[0]
            offset += 4
            
            bounds = COLBounds(
                min=Vector3(min_x, min_y, min_z),
                max=Vector3(max_x, max_y, max_z),
                center=Vector3(center_x, center_y, center_z),
                radius=radius
            )
            
            return bounds, offset
            
        except Exception as e:
            self.error_message = f"COL2/3 bounds parse error: {str(e)}"
            return None, offset
    
    def parse_spheres(self, data: bytes, offset: int, count: int, version: int) -> Tuple[List[COLSphere], int]: #vers 1
        """Parse collision spheres"""
        spheres = []
        
        try:
            bytes_per_sphere = 20 if version == 1 else 16
            
            for i in range(count):
                if len(data) < offset + bytes_per_sphere:
                    raise ValueError(f"Not enough data for sphere {i}")
                
                # Center (12 bytes)
                x, y, z = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
                # Radius (4 bytes)
                radius = struct.unpack('<f', data[offset:offset+4])[0]
                offset += 4
                
                # Material ID (4 bytes)
                material_id = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                
                # Flags (4 bytes, COL1 only)
                flags = 0
                if version == 1:
                    flags = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                
                sphere = COLSphere(
                    center=Vector3(x, y, z),
                    radius=radius,
                    material_id=material_id,
                    flags=flags
                )
                spheres.append(sphere)
            
            return spheres, offset
            
        except Exception as e:
            self.error_message = f"Sphere parse error: {str(e)}"
            return spheres, offset
    
    def parse_boxes(self, data: bytes, offset: int, count: int, version: int) -> Tuple[List[COLBox], int]: #vers 1
        """Parse collision boxes"""
        boxes = []
        
        try:
            bytes_per_box = 32 if version == 1 else 28
            
            for i in range(count):
                if len(data) < offset + bytes_per_box:
                    raise ValueError(f"Not enough data for box {i}")
                
                # Min point (12 bytes)
                min_x, min_y, min_z = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
                # Max point (12 bytes)
                max_x, max_y, max_z = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
                # Material ID (4 bytes)
                material_id = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                
                # Flags (4 bytes, COL1 only)
                flags = 0
                if version == 1:
                    flags = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                
                box = COLBox(
                    min_point=Vector3(min_x, min_y, min_z),
                    max_point=Vector3(max_x, max_y, max_z),
                    material_id=material_id,
                    flags=flags
                )
                boxes.append(box)
            
            return boxes, offset
            
        except Exception as e:
            self.error_message = f"Box parse error: {str(e)}"
            return boxes, offset
    
    def parse_vertices(self, data: bytes, offset: int, count: int) -> Tuple[List[COLVertex], int]: #vers 1
        """Parse mesh vertices"""
        vertices = []
        
        try:
            for i in range(count):
                if len(data) < offset + 12:
                    raise ValueError(f"Not enough data for vertex {i}")
                
                # Position (12 bytes)
                x, y, z = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
                vertex = COLVertex(position=Vector3(x, y, z))
                vertices.append(vertex)
            
            return vertices, offset
            
        except Exception as e:
            self.error_message = f"Vertex parse error: {str(e)}"
            return vertices, offset
    
    def parse_faces(self, data: bytes, offset: int, count: int, version: int) -> Tuple[List[COLFace], int]: #vers 1
        """Parse mesh faces"""
        faces = []
        
        try:
            bytes_per_face = 16 if version == 1 else 14
            
            for i in range(count):
                if len(data) < offset + bytes_per_face:
                    raise ValueError(f"Not enough data for face {i}")
                
                # Vertex indices (6 bytes - 3 shorts)
                v1, v2, v3 = struct.unpack('<HHH', data[offset:offset+6])
                offset += 6
                
                # Material ID (2 bytes)
                material_id = struct.unpack('<H', data[offset:offset+2])[0]
                offset += 2
                
                # Light (2 bytes)
                light = struct.unpack('<H', data[offset:offset+2])[0]
                offset += 2
                
                # Flags (4 bytes COL1, 2 bytes padding COL2/3)
                flags = 0
                if version == 1:
                    flags = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                else:
                    offset += 2  # Skip padding
                
                face = COLFace(
                    vertex_indices=(v1, v2, v3),
                    material_id=material_id,
                    light=light,
                    flags=flags
                )
                faces.append(face)
            
            return faces, offset
            
        except Exception as e:
            self.error_message = f"Face parse error: {str(e)}"
            return faces, offset
    
    def _read_uint(self, data: bytes, offset: int) -> Tuple[int, int]: #vers 1
        """Read unsigned int and return value + new offset"""
        if len(data) < offset + 4:
            raise ValueError("Not enough data for uint")
        value = struct.unpack('<I', data[offset:offset+4])[0]
        return value, offset + 4


# Export main classes
__all__ = [
    'COLParser',
    'COLModel',
    'COLHeader',
    'COLBounds',
    'COLSphere',
    'COLBox',
    'COLVertex',
    'COLFace',
    'Vector3'
]
