#this belongs in apps/methods/col_core_classes.py OR apps/components/Col_Editor/depends/col_core_classes.py - Version: 4
# X-Seti - December13 2025 - IMG Factory 1.5 - COL Core Classes

"""
COL Core Classes - Complete collision file handling
Handles GTA III, Vice City, and San Andreas COL formats (COL1, COL2, COL3, COL4)
Includes garbage face count detection and correction
Clean implementation - no fallback duplicates
"""

import struct
import os
from enum import Enum
from typing import List, Tuple, Optional
from apps.debug.debug_functions import img_debugger

# Global debug control
_global_debug_enabled = False

def set_col_debug_enabled(enabled: bool): #vers 1
    """Enable or disable COL debug output globally"""
    global _global_debug_enabled
    _global_debug_enabled = enabled
    if enabled:
        img_debugger.info("COL debug output enabled")
    else:
        img_debugger.info("COL debug output disabled")

def is_col_debug_enabled() -> bool: #vers 1
    """Check if COL debug output is enabled"""
    return _global_debug_enabled

def diagnose_col_file(file_path: str) -> dict: #vers 1
    """
    Diagnose COL file and return detailed information
    
    Returns:
        dict with keys: success, error, models, version, size, etc.
    """
    result = {
        'success': False,
        'error': None,
        'file_path': file_path,
        'file_size': 0,
        'models': [],
        'model_count': 0
    }
    
    try:
        if not os.path.exists(file_path):
            result['error'] = "File not found"
            return result
        
        file_size = os.path.getsize(file_path)
        result['file_size'] = file_size
        
        if file_size < 8:
            result['error'] = "File too small to be valid COL"
            return result
        
        # Try to load the file
        col_file = COLFile()
        success = col_file.load_from_file(file_path)
        
        if success:
            result['success'] = True
            result['model_count'] = len(col_file.models)
            
            for i, model in enumerate(col_file.models):
                model_info = {
                    'index': i,
                    'name': model.name,
                    'version': model.version.name,
                    'model_id': model.model_id,
                    'spheres': len(model.spheres),
                    'boxes': len(model.boxes),
                    'vertices': len(model.vertices),
                    'faces': len(model.faces),
                    'calculated_face_count': model.calculated_face_count
                }
                result['models'].append(model_info)
        else:
            result['error'] = col_file.load_error or "Unknown load error"
        
        return result
        
    except Exception as e:
        result['error'] = f"Diagnostic error: {str(e)}"
        return result

##Module Functions -
# diagnose_col_file
# is_col_debug_enabled
# set_col_debug_enabled

##Methods list -
# calculate_bounding_box
# get_info
# get_stats
# load_from_file
# save_to_file
# update_flags
# _build_col1_model
# _build_col23_model
# _calculate_face_count
# _parse_col1_model
# _parse_col23_model
# _parse_col_data
# _parse_col_model

##Classes -
# BoundingBox
# COLBox
# COLFace
# COLFaceGroup
# COLFile
# COLMaterial
# COLModel
# COLSphere
# COLVersion
# COLVertex
# Vector3


# ==================== DATA STRUCTURES ====================

class Vector3:
    """3D vector/point"""
    
    def __init__(self, x: float = 0.0, y: float = 0.0, z: float = 0.0): #vers 1
        """Initialize 3D vector"""
        self.x = x
        self.y = y
        self.z = z
    
    def __str__(self):
        return f"({self.x:.2f}, {self.y:.2f}, {self.z:.2f})"
    
    def __repr__(self):
        return f"Vector3({self.x}, {self.y}, {self.z})"


class BoundingBox:
    """Axis-aligned bounding box"""
    
    def __init__(self): #vers 1
        """Initialize bounding box"""
        self.min = Vector3(-1.0, -1.0, -1.0)
        self.max = Vector3(1.0, 1.0, 1.0)
        self.center = Vector3(0.0, 0.0, 0.0)
        self.radius = 1.0
    
    def __str__(self):
        return f"BoundingBox(min={self.min}, max={self.max}, radius={self.radius:.2f})"


class COLVersion(Enum):
    """COL file format versions"""
    COL_1 = 1  # GTA III
    COL_2 = 2  # GTA Vice City
    COL_3 = 3  # GTA San Andreas
    COL_4 = 4  # Extended format


class COLMaterial:
    """COL material/surface properties"""
    
    def __init__(self, material_id: int = 0, flags: int = 0): #vers 1
        """Initialize material"""
        self.material_id = material_id
        self.flags = flags
    
    def __str__(self):
        return f"COLMaterial(id={self.material_id}, flags={self.flags})"


class COLSphere:
    """Collision sphere"""
    
    def __init__(self, center: Vector3, radius: float, material: COLMaterial): #vers 1
        """Initialize collision sphere"""
        self.center = center
        self.radius = radius
        self.material = material
    
    def __str__(self):
        return f"COLSphere(center={self.center}, r={self.radius:.2f}, mat={self.material.material_id})"


class COLBox:
    """Collision box (axis-aligned)"""
    
    def __init__(self, min_point: Vector3, max_point: Vector3, material: COLMaterial): #vers 1
        """Initialize collision box"""
        self.min_point = min_point
        self.max_point = max_point
        self.material = material
    
    def __str__(self):
        return f"COLBox(min={self.min_point}, max={self.max_point}, mat={self.material.material_id})"


class COLVertex:
    """Mesh vertex"""
    
    def __init__(self, position: Vector3): #vers 1
        """Initialize vertex"""
        self.position = position
    
    def __str__(self):
        return f"COLVertex({self.position})"


class COLFace:
    """Mesh face (triangle)"""
    
    def __init__(self, vertex_indices: Tuple[int, int, int], material: COLMaterial, light: int): #vers 1
        """Initialize face"""
        self.vertex_indices = vertex_indices
        self.material = material
        self.light = light
    
    def __str__(self):
        return f"COLFace(indices={self.vertex_indices}, mat={self.material.material_id}, light={self.light})"


class COLFaceGroup:
    """Face group for COL2/COL3"""
    
    def __init__(self): #vers 1
        """Initialize face group"""
        self.faces: List[COLFace] = []
        self.material = COLMaterial()
    
    def add_face(self, face: COLFace): #vers 1
        """Add face to group"""
        self.faces.append(face)


# ==================== COL MODEL ====================

class COLModel:
    """Complete COL collision model"""
    
    def __init__(self): #vers 1
        """Initialize empty model"""
        self.name = ""
        self.model_id = 0
        self.version = COLVersion.COL_1
        self.bounding_box = BoundingBox()
        
        # Collision elements
        self.spheres: List[COLSphere] = []
        self.boxes: List[COLBox] = []
        self.vertices: List[COLVertex] = []
        self.faces: List[COLFace] = []
        self.face_groups: List[COLFaceGroup] = []
        
        # Status flags
        self.has_sphere_data = False
        self.has_box_data = False
        self.has_mesh_data = False
        
        # Debug flags
        self.calculated_face_count = False
    
    def get_stats(self) -> str: #vers 1
        """Get model statistics string"""
        return f"{self.name}: S:{len(self.spheres)} B:{len(self.boxes)} V:{len(self.vertices)} F:{len(self.faces)}"
    
    def update_flags(self): #vers 1
        """Update status flags based on data"""
        self.has_sphere_data = len(self.spheres) > 0
        self.has_box_data = len(self.boxes) > 0
        self.has_mesh_data = len(self.vertices) > 0 and len(self.faces) > 0
    
    def calculate_bounding_box(self): #vers 1
        """Calculate bounding box from all collision elements"""
        all_vertices = []
        
        # Add sphere extents
        for sphere in self.spheres:
            all_vertices.extend([
                Vector3(
                    sphere.center.x - sphere.radius,
                    sphere.center.y - sphere.radius,
                    sphere.center.z - sphere.radius
                ),
                Vector3(
                    sphere.center.x + sphere.radius,
                    sphere.center.y + sphere.radius,
                    sphere.center.z + sphere.radius
                )
            ])
        
        # Add box vertices
        for box in self.boxes:
            all_vertices.extend([box.min_point, box.max_point])
        
        # Add mesh vertices
        for vertex in self.vertices:
            all_vertices.append(vertex.position)
        
        if not all_vertices:
            return
        
        # Calculate bounds
        min_x = min(v.x for v in all_vertices)
        min_y = min(v.y for v in all_vertices)
        min_z = min(v.z for v in all_vertices)
        max_x = max(v.x for v in all_vertices)
        max_y = max(v.y for v in all_vertices)
        max_z = max(v.z for v in all_vertices)
        
        self.bounding_box.min = Vector3(min_x, min_y, min_z)
        self.bounding_box.max = Vector3(max_x, max_y, max_z)
        self.bounding_box.center = Vector3(
            (min_x + max_x) / 2,
            (min_y + max_y) / 2,
            (min_z + max_z) / 2
        )
        
        # Calculate radius
        dx = max_x - min_x
        dy = max_y - min_y
        dz = max_z - min_z
        self.bounding_box.radius = (dx*dx + dy*dy + dz*dz) ** 0.5 / 2


# ==================== COL FILE ====================

class COLFile:
    """COL file container - handles loading, parsing, and saving"""
    
    def __init__(self): #vers 1
        """Initialize empty COL file"""
        self.file_path = ""
        self.models: List[COLModel] = []
        self.is_loaded = False
        self.load_error = ""
    
    def load_from_file(self, file_path: str) -> bool: #vers 1
        """Load COL file from disk"""
        try:
            self.file_path = file_path
            self.models = []
            self.is_loaded = False
            self.load_error = ""
            
            if not os.path.exists(file_path):
                self.load_error = f"File not found: {file_path}"
                img_debugger.error(self.load_error)
                return False
            
            with open(file_path, 'rb') as f:
                data = f.read()
            
            if len(data) < 8:
                self.load_error = "File too small to be valid COL"
                img_debugger.error(self.load_error)
                return False
            
            img_debugger.info(f"Loading COL file: {os.path.basename(file_path)} ({len(data)} bytes)")
            
            return self._parse_col_data(data)
            
        except Exception as e:
            self.load_error = f"Load error: {str(e)}"
            img_debugger.error(f"COL load error: {e}")
            import traceback
            img_debugger.error(traceback.format_exc())
            return False
    
    def save_to_file(self, file_path: str) -> bool: #vers 1
        """Save COL file to disk"""
        try:
            if not self.models:
                img_debugger.error("No models to save")
                return False
            
            data = b''
            
            for model in self.models:
                if model.version == COLVersion.COL_1:
                    data += self._build_col1_model(model)
                else:
                    data += self._build_col23_model(model)
            
            with open(file_path, 'wb') as f:
                f.write(data)
            
            img_debugger.success(f"Saved {len(self.models)} models to {file_path}")
            return True
            
        except Exception as e:
            img_debugger.error(f"Save error: {e}")
            return False
    
    def _parse_col_data(self, data: bytes) -> bool: #vers 1
        """Parse COL file data"""
        try:
            offset = 0
            model_count = 0
            
            while offset < len(data):
                model, consumed = self._parse_col_model(data, offset)
                
                if model is None:
                    break
                
                self.models.append(model)
                offset += consumed
                model_count += 1
                
                # Safety check to prevent infinite loops
                if consumed == 0:
                    img_debugger.warning("Zero bytes consumed, stopping parse")
                    break
            
            self.is_loaded = len(self.models) > 0
            
            if self.is_loaded:
                img_debugger.success(f"Successfully loaded {len(self.models)} COL models")
            else:
                img_debugger.error("No valid models found in file")
            
            return self.is_loaded
            
        except Exception as e:
            self.load_error = f"Parse error: {str(e)}"
            img_debugger.error(f"COL parse error: {e}")
            import traceback
            img_debugger.error(traceback.format_exc())
            return False
    
    def _parse_col_model(self, data: bytes, offset: int) -> Tuple[Optional[COLModel], int]: #vers 1
        """Parse single COL model from data"""
        try:
            if offset + 8 > len(data):
                return None, 0
            
            # Read FourCC signature
            fourcc = data[offset:offset+4]
            
            if fourcc not in [b'COLL', b'COL\x02', b'COL\x03', b'COL\x04']:
                return None, 0
            
            # Read file size
            file_size = struct.unpack('<I', data[offset+4:offset+8])[0]
            total_size = file_size + 8
            
            if offset + total_size > len(data):
                img_debugger.warning(f"Model size ({total_size}) extends beyond data ({len(data) - offset} remaining)")
                return None, 0
            
            # Create model
            model = COLModel()
            
            # Determine version from signature
            if fourcc == b'COLL':
                model.version = COLVersion.COL_1
            elif fourcc == b'COL\x02':
                model.version = COLVersion.COL_2
            elif fourcc == b'COL\x03':
                model.version = COLVersion.COL_3
            elif fourcc == b'COL\x04':
                model.version = COLVersion.COL_4
            
            # Extract model data (skip header)
            model_data = data[offset + 8:offset + total_size]
            
            # Parse based on version
            if model.version == COLVersion.COL_1:
                self._parse_col1_model(model, model_data)
            else:
                self._parse_col23_model(model, model_data)
            
            img_debugger.debug(f"Parsed {model.version.name} model: {model.get_stats()}")
            
            return model, total_size
            
        except Exception as e:
            img_debugger.error(f"Model parse error: {e}")
            import traceback
            img_debugger.error(traceback.format_exc())
            return None, 0
    
    def _parse_col1_model(self, model: COLModel, data: bytes): #vers 2
        """Parse COL1 format model with garbage face count fix"""
        try:
            offset = 0
            
            # Parse model name (22 bytes)
            if len(data) < 22:
                img_debugger.error("COL1: Data too small for model name")
                return
            
            name_bytes = data[offset:offset+22]
            model.name = name_bytes.split(b'\x00')[0].decode('ascii', errors='ignore')
            offset += 22
            
            # Parse model ID (2 bytes)
            if offset + 2 > len(data):
                img_debugger.error("COL1: Data too small for model ID")
                return
            
            model.model_id = struct.unpack('<H', data[offset:offset+2])[0]
            offset += 2
            
            # Parse bounding box (40 bytes)
            if offset + 40 > len(data):
                img_debugger.error("COL1: Data too small for bounding box")
                return
            
            model.bounding_box.radius = struct.unpack('<f', data[offset:offset+4])[0]
            offset += 4
            
            cx, cy, cz = struct.unpack('<fff', data[offset:offset+12])
            model.bounding_box.center = Vector3(cx, cy, cz)
            offset += 12
            
            min_x, min_y, min_z = struct.unpack('<fff', data[offset:offset+12])
            model.bounding_box.min = Vector3(min_x, min_y, min_z)
            offset += 12
            
            max_x, max_y, max_z = struct.unpack('<fff', data[offset:offset+12])
            model.bounding_box.max = Vector3(max_x, max_y, max_z)
            offset += 12
            
            # Parse counts (20 bytes)
            if offset + 20 > len(data):
                img_debugger.error("COL1: Data too small for counts")
                return
            
            num_spheres = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            num_unknown = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            num_boxes = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            num_vertices = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            num_faces = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            
            img_debugger.debug(f"COL1: Counts - S:{num_spheres} B:{num_boxes} V:{num_vertices} F:{num_faces}")
            
            # CRITICAL FIX: Check for garbage face count
            num_faces = self._calculate_face_count(
                data, offset, num_spheres, num_unknown, num_boxes, num_vertices, num_faces, is_col1=True
            )
            
            if model.calculated_face_count:
                img_debugger.warning(f"COL1: Using calculated face count: {num_faces}")
            
            # Import safe parsing helpers
            try:
                from apps.methods.col_parsing_helpers import (
                    safe_parse_spheres, safe_parse_boxes,
                    safe_parse_vertices, safe_parse_faces_col1
                )
                
                # Parse spheres
                offset = safe_parse_spheres(model, data, offset, num_spheres, "COL1")
                
                # Skip unknown data
                offset += num_unknown * 4
                
                # Parse boxes
                offset = safe_parse_boxes(model, data, offset, num_boxes, "COL1")
                
                # Parse vertices
                offset = safe_parse_vertices(model, data, offset, num_vertices)
                
                # Parse faces (with corrected count)
                offset = safe_parse_faces_col1(model, data, offset, num_faces)
                
            except ImportError as e:
                img_debugger.error(f"COL1: Safe parsing helpers not available: {e}")
                img_debugger.error("Make sure apps/methods/col_parsing_helpers.py exists")
                return
            
            model.update_flags()
            img_debugger.debug(f"COL1: Parse complete - {model.get_stats()}")
            
        except Exception as e:
            img_debugger.error(f"COL1 parse error: {e}")
            import traceback
            img_debugger.error(traceback.format_exc())
    
    def _parse_col23_model(self, model: COLModel, data: bytes): #vers 2
        """Parse COL2/COL3 format model"""
        try:
            offset = 0
            
            # Parse model name (22 bytes)
            if len(data) < 22:
                img_debugger.error("COL2/3: Data too small for model name")
                return
            
            name_bytes = data[offset:offset+22]
            model.name = name_bytes.split(b'\x00')[0].decode('ascii', errors='ignore')
            offset += 22
            
            # Parse model ID (2 bytes)
            if offset + 2 > len(data):
                img_debugger.error("COL2/3: Data too small for model ID")
                return
            
            model.model_id = struct.unpack('<H', data[offset:offset+2])[0]
            offset += 2
            
            # Parse bounding box (28 bytes for COL2/3)
            if offset + 28 > len(data):
                img_debugger.error("COL2/3: Data too small for bounding box")
                return
            
            min_x, min_y, min_z = struct.unpack('<fff', data[offset:offset+12])
            model.bounding_box.min = Vector3(min_x, min_y, min_z)
            offset += 12
            
            max_x, max_y, max_z = struct.unpack('<fff', data[offset:offset+12])
            model.bounding_box.max = Vector3(max_x, max_y, max_z)
            offset += 12
            
            cx, cy, cz = struct.unpack('<fff', data[offset:offset+12])
            model.bounding_box.center = Vector3(cx, cy, cz)
            offset += 12
            
            model.bounding_box.radius = struct.unpack('<f', data[offset:offset+4])[0]
            offset += 4
            
            # Parse counts (16 bytes) - COL2/3 order: spheres, boxes, faces, vertices
            if offset + 16 > len(data):
                img_debugger.error("COL2/3: Data too small for counts")
                return
            
            num_spheres = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            num_boxes = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            num_faces = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            num_vertices = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            
            img_debugger.debug(f"COL2/3: Counts - S:{num_spheres} B:{num_boxes} V:{num_vertices} F:{num_faces}")
            
            # Import safe parsing helpers
            try:
                from apps.methods.col_parsing_helpers import (
                    safe_parse_spheres, safe_parse_boxes,
                    safe_parse_vertices, safe_parse_faces_col23
                )
                
                # Parse spheres
                offset = safe_parse_spheres(model, data, offset, num_spheres, "COL2/3")
                
                # Parse boxes
                offset = safe_parse_boxes(model, data, offset, num_boxes, "COL2/3")
                
                # Parse vertices
                offset = safe_parse_vertices(model, data, offset, num_vertices)
                
                # Parse faces
                offset = safe_parse_faces_col23(model, data, offset, num_faces)
                
            except ImportError as e:
                img_debugger.error(f"COL2/3: Safe parsing helpers not available: {e}")
                img_debugger.error("Make sure apps/methods/col_parsing_helpers.py exists")
                return
            
            model.update_flags()
            img_debugger.debug(f"COL2/3: Parse complete - {model.get_stats()}")
            
        except Exception as e:
            img_debugger.error(f"COL2/3 parse error: {e}")
            import traceback
            img_debugger.error(traceback.format_exc())
    
    def _calculate_face_count(self, data: bytes, offset: int, num_spheres: int, 
                             num_unknown: int, num_boxes: int, num_vertices: int, 
                             num_faces: int, is_col1: bool = True) -> int: #vers 1
        """
        Calculate actual face count from file size
        Fixes garbage face count issues (e.g. 3,226,344,957 instead of 46)
        
        Args:
            data: Complete model data
            offset: Current offset (after counts)
            num_spheres: Number of spheres
            num_unknown: Unknown data count (COL1 only)
            num_boxes: Number of boxes
            num_vertices: Number of vertices
            num_faces: Stored face count (may be garbage)
            is_col1: True for COL1, False for COL2/3
        
        Returns:
            Corrected face count
        """
        try:
            # Calculate bytes used by known data
            SPHERE_SIZE = 24  # center(12) + radius(4) + material(4) + flags(4)
            BOX_SIZE = 32     # min(12) + max(12) + material(4) + flags(4)
            VERTEX_SIZE = 12  # position(12)
            FACE_SIZE_COL1 = 16  # indices(6) + mat(2) + light(2) + flags(4) + padding(2)
            FACE_SIZE_COL23 = 12 # indices(6) + mat(2) + light(2) + padding(2)
            
            data_used = (num_spheres * SPHERE_SIZE) + \
                       (num_unknown * 4 if is_col1 else 0) + \
                       (num_boxes * BOX_SIZE) + \
                       (num_vertices * VERTEX_SIZE)
            
            # Calculate remaining bytes after vertices
            offset_after_verts = offset + data_used
            remaining_bytes = len(data) - offset_after_verts
            
            # Calculate face count from remaining bytes
            face_size = FACE_SIZE_COL1 if is_col1 else FACE_SIZE_COL23
            calculated_faces = remaining_bytes // face_size
            
            # Sanity check: Is stored face count garbage?
            MAX_REASONABLE_FACES = 1000000  # 1 million faces is reasonable max
            
            if num_faces > MAX_REASONABLE_FACES or num_faces > calculated_faces * 10:
                # Face count is garbage - use calculated value
                img_debugger.warning(f"⚠️  Garbage face count detected: {num_faces}")
                img_debugger.warning(f"    Correcting to calculated value: {calculated_faces}")
                img_debugger.warning(f"    (Remaining bytes: {remaining_bytes} / Face size: {face_size})")
                
                # Set flag for debugging
                if hasattr(self, 'models') and self.models:
                    self.models[-1].calculated_face_count = True
                
                return calculated_faces
            
            # Face count seems reasonable
            return num_faces
            
        except Exception as e:
            img_debugger.error(f"Face count calculation error: {e}")
            # Return original if calculation fails
            return num_faces
    
    def _build_col1_model(self, model: COLModel) -> bytes: #vers 1
        """Build COL1 format binary data"""
        try:
            data = b''
            model_content = b''
            
            # Model name (22 bytes)
            name_bytes = model.name.encode('ascii')[:21].ljust(22, b'\x00')
            model_content += name_bytes
            
            # Model ID (2 bytes)
            model_content += struct.pack('<H', model.model_id)
            
            # Bounding box (40 bytes)
            model_content += struct.pack('<f', model.bounding_box.radius)
            model_content += struct.pack('<fff', 
                model.bounding_box.center.x, 
                model.bounding_box.center.y, 
                model.bounding_box.center.z
            )
            model_content += struct.pack('<fff', 
                model.bounding_box.min.x, 
                model.bounding_box.min.y, 
                model.bounding_box.min.z
            )
            model_content += struct.pack('<fff', 
                model.bounding_box.max.x, 
                model.bounding_box.max.y, 
                model.bounding_box.max.z
            )
            
            # Counts (20 bytes)
            model_content += struct.pack('<I', len(model.spheres))
            model_content += struct.pack('<I', 0)  # unknown
            model_content += struct.pack('<I', len(model.boxes))
            model_content += struct.pack('<I', len(model.vertices))
            model_content += struct.pack('<I', len(model.faces))
            
            # Spheres (24 bytes each)
            for sphere in model.spheres:
                model_content += struct.pack('<fff', 
                    sphere.center.x, sphere.center.y, sphere.center.z
                )
                model_content += struct.pack('<f', sphere.radius)
                model_content += struct.pack('<I', sphere.material.material_id)
                model_content += struct.pack('<I', sphere.material.flags)
            
            # Boxes (32 bytes each)
            for box in model.boxes:
                model_content += struct.pack('<fff', 
                    box.min_point.x, box.min_point.y, box.min_point.z
                )
                model_content += struct.pack('<fff', 
                    box.max_point.x, box.max_point.y, box.max_point.z
                )
                model_content += struct.pack('<I', box.material.material_id)
                model_content += struct.pack('<I', box.material.flags)
            
            # Vertices (12 bytes each)
            for vertex in model.vertices:
                model_content += struct.pack('<fff', 
                    vertex.position.x, vertex.position.y, vertex.position.z
                )
            
            # Faces (16 bytes each for COL1)
            for face in model.faces:
                model_content += struct.pack('<HHH', *face.vertex_indices)
                model_content += struct.pack('<H', face.material.material_id)
                model_content += struct.pack('<H', face.light)
                model_content += struct.pack('<I', face.material.flags)
            
            # Build header
            data += b'COLL'  # Signature
            data += struct.pack('<I', len(model_content))  # File size
            data += model_content
            
            return data
            
        except Exception as e:
            img_debugger.error(f"COL1 build error: {e}")
            return b''
    
    def _build_col23_model(self, model: COLModel) -> bytes: #vers 1
        """Build COL2/COL3 format binary data"""
        try:
            data = b''
            model_content = b''
            
            # Model name (22 bytes)
            name_bytes = model.name.encode('ascii')[:21].ljust(22, b'\x00')
            model_content += name_bytes
            
            # Model ID (2 bytes)
            model_content += struct.pack('<H', model.model_id)
            
            # Bounding box (28 bytes for COL2/3)
            model_content += struct.pack('<fff', 
                model.bounding_box.min.x, 
                model.bounding_box.min.y, 
                model.bounding_box.min.z
            )
            model_content += struct.pack('<fff', 
                model.bounding_box.max.x, 
                model.bounding_box.max.y, 
                model.bounding_box.max.z
            )
            model_content += struct.pack('<fff', 
                model.bounding_box.center.x, 
                model.bounding_box.center.y, 
                model.bounding_box.center.z
            )
            model_content += struct.pack('<f', model.bounding_box.radius)
            
            # Counts (16 bytes) - COL2/3 order
            model_content += struct.pack('<I', len(model.spheres))
            model_content += struct.pack('<I', len(model.boxes))
            model_content += struct.pack('<I', len(model.faces))
            model_content += struct.pack('<I', len(model.vertices))
            
            # Spheres (20 bytes each for COL2/3)
            for sphere in model.spheres:
                model_content += struct.pack('<fff', 
                    sphere.center.x, sphere.center.y, sphere.center.z
                )
                model_content += struct.pack('<f', sphere.radius)
                model_content += struct.pack('<I', sphere.material.material_id)
            
            # Boxes (28 bytes each for COL2/3)
            for box in model.boxes:
                model_content += struct.pack('<fff', 
                    box.min_point.x, box.min_point.y, box.min_point.z
                )
                model_content += struct.pack('<fff', 
                    box.max_point.x, box.max_point.y, box.max_point.z
                )
                model_content += struct.pack('<I', box.material.material_id)
            
            # Vertices (12 bytes each)
            for vertex in model.vertices:
                model_content += struct.pack('<fff', 
                    vertex.position.x, vertex.position.y, vertex.position.z
                )
            
            # Faces (12 bytes each for COL2/3)
            for face in model.faces:
                model_content += struct.pack('<HHH', *face.vertex_indices)
                model_content += struct.pack('<H', face.material.material_id)
                model_content += struct.pack('<H', face.light)
                model_content += struct.pack('<H', 0)  # Padding
            
            # Build header with appropriate signature
            if model.version == COLVersion.COL_2:
                data += b'COL\x02'
            elif model.version == COLVersion.COL_3:
                data += b'COL\x03'
            else:
                data += b'COL\x04'
            
            data += struct.pack('<I', len(model_content))  # File size
            data += model_content
            
            return data
            
        except Exception as e:
            img_debugger.error(f"COL2/3 build error: {e}")
            return b''
    
    def get_info(self) -> str: #vers 1
        """Get comprehensive file information"""
        lines = []
        
        filename = os.path.basename(self.file_path) if self.file_path else "Unknown"
        lines.append(f"COL File: {filename}")
        lines.append(f"Models: {len(self.models)}")
        lines.append("")
        
        for i, model in enumerate(self.models):
            lines.append(f"Model {i}: {model.name}")
            lines.append(f"  Version: {model.version.name}")
            lines.append(f"  ID: {model.model_id}")
            lines.append(f"  Spheres: {len(model.spheres)}")
            lines.append(f"  Boxes: {len(model.boxes)}")
            lines.append(f"  Vertices: {len(model.vertices)}")
            lines.append(f"  Faces: {len(model.faces)}")
            if model.calculated_face_count:
                lines.append(f"  Note: Face count was calculated (file had garbage data)")
            lines.append("")
        
        return "\n".join(lines)


# Export classes and functions
__all__ = [
    # Data structures
    'Vector3',
    'BoundingBox',
    'COLVersion',
    'COLMaterial',
    'COLSphere',
    'COLBox',
    'COLVertex',
    'COLFace',
    'COLFaceGroup',
    'COLModel',
    'COLFile',
    # Debug functions
    'set_col_debug_enabled',
    'is_col_debug_enabled',
    'diagnose_col_file'
]
