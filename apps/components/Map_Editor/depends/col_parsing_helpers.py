#this belongs in methods/col_parsing_helpers.py - Version: 1
# X-Seti - August13 2025 - IMG Factory 1.5 - COL Parsing Helper Methods

"""
COL Parsing Helper Methods - Shared functions for safe COL parsing
Used by methods.col_core_classes.py to safely parse COL data with bounds checking
"""

import struct
from typing import Tuple, Optional, Any
from apps.debug.debug_functions import img_debugger

##Methods list -
# safe_parse_faces_col1
# safe_parse_faces_col23
# safe_unpack_check
# validate_data_bounds

def safe_unpack_check(data: bytes, offset: int, format_str: str, field_name: str = "") -> Tuple[Optional[Any], int]: #vers 1
    """Safely unpack data with bounds checking"""
    try:
        size = struct.calcsize(format_str)
        
        if offset + size > len(data):
            img_debugger.warning(f"COL: Not enough data for {field_name} (need {size} bytes, have {len(data) - offset})")
            return None, offset
        
        result = struct.unpack(format_str, data[offset:offset+size])
        return result, offset + size
        
    except struct.error as e:
        img_debugger.error(f"COL: Struct unpack error for {field_name}: {e}")
        return None, offset

def validate_data_bounds(data: bytes, offset: int, required_size: int, operation: str = "") -> bool: #vers 1
    """Validate that there's enough data remaining"""
    if offset + required_size > len(data):
        img_debugger.warning(f"COL: Not enough data for {operation} (need {required_size} bytes, have {len(data) - offset})")
        return False
    return True

def safe_parse_faces_col1(model, data: bytes, offset: int, num_faces: int) -> int: #vers 1
    """Safely parse COL1 faces with bounds checking"""
    try:
        img_debugger.debug(f"COL1: Parsing {num_faces} faces starting at offset {offset}")
        
        for i in range(num_faces):
            # Check vertex indices (6 bytes: 3 shorts)
            vertex_data, new_offset = safe_unpack_check(data, offset, '<HHH', f"face {i} vertex indices")
            if vertex_data is None:
                img_debugger.warning(f"COL1: Stopping face parsing at face {i} (vertex indices)")
                break
            vertex_indices = vertex_data
            offset = new_offset
            
            # Check material ID (2 bytes)
            material_data, new_offset = safe_unpack_check(data, offset, '<H', f"face {i} material ID")
            if material_data is None:
                img_debugger.warning(f"COL1: Using default material for face {i}")
                material_id = 0
            else:
                material_id = material_data[0]
                offset = new_offset
            
            # Check light (2 bytes)
            light_data, new_offset = safe_unpack_check(data, offset, '<H', f"face {i} light")
            if light_data is None:
                img_debugger.warning(f"COL1: Using default light for face {i}")
                light = 0
            else:
                light = light_data[0]
                offset = new_offset
            
            # Check flags (4 bytes)
            flags_data, new_offset = safe_unpack_check(data, offset, '<I', f"face {i} flags")
            if flags_data is None:
                img_debugger.warning(f"COL1: Using default flags for face {i}")
                flags = 0
            else:
                flags = flags_data[0]
                offset = new_offset
            
            # Create face with safe data
            try:
                from apps.methods.col_core_classes import COLMaterial, COLFace
                material = COLMaterial(material_id, flags=flags)
                face = COLFace(vertex_indices, material, light)
                model.faces.append(face)
                
            except Exception as e:
                img_debugger.error(f"COL1: Error creating face {i}: {e}")
                continue
        
        img_debugger.debug(f"COL1: Successfully parsed {len(model.faces)} faces")
        return offset
        
    except Exception as e:
        img_debugger.error(f"COL1: Error in safe face parsing: {e}")
        return offset

def safe_parse_faces_col23(model, data: bytes, offset: int, num_faces: int) -> int: #vers 1
    """Safely parse COL2/3 faces with bounds checking"""
    try:
        img_debugger.debug(f"COL2/3: Parsing {num_faces} faces starting at offset {offset}")
        
        for i in range(num_faces):
            # Check vertex indices (6 bytes: 3 shorts)
            vertex_data, new_offset = safe_unpack_check(data, offset, '<HHH', f"face {i} vertex indices")
            if vertex_data is None:
                img_debugger.warning(f"COL2/3: Stopping face parsing at face {i} (vertex indices)")
                break
            vertex_indices = vertex_data
            offset = new_offset
            
            # Check material ID (2 bytes)
            material_data, new_offset = safe_unpack_check(data, offset, '<H', f"face {i} material ID")
            if material_data is None:
                img_debugger.warning(f"COL2/3: Using default material for face {i}")
                material_id = 0
            else:
                material_id = material_data[0]
                offset = new_offset
            
            # Check light (2 bytes)
            light_data, new_offset = safe_unpack_check(data, offset, '<H', f"face {i} light")
            if light_data is None:
                img_debugger.warning(f"COL2/3: Using default light for face {i}")
                light = 0
            else:
                light = light_data[0]
                offset = new_offset
            
            # COL2/3 has padding instead of flags
            padding_data, new_offset = safe_unpack_check(data, offset, '<H', f"face {i} padding")
            if padding_data is None:
                img_debugger.warning(f"COL2/3: Skipping padding for face {i}")
            else:
                offset = new_offset  # Skip padding
            
            # Create face with safe data
            try:
                from apps.methods.col_core_classes import COLMaterial, COLFace
                material = COLMaterial(material_id)
                face = COLFace(vertex_indices, material, light)
                model.faces.append(face)
                
            except Exception as e:
                img_debugger.error(f"COL2/3: Error creating face {i}: {e}")
                continue
        
        img_debugger.debug(f"COL2/3: Successfully parsed {len(model.faces)} faces")
        return offset
        
    except Exception as e:
        img_debugger.error(f"COL2/3: Error in safe face parsing: {e}")
        return offset

def safe_parse_spheres(model, data: bytes, offset: int, num_spheres: int, version: str = "COL1") -> int: #vers 1
    """Safely parse COL spheres with bounds checking"""
    try:
        img_debugger.debug(f"{version}: Parsing {num_spheres} spheres starting at offset {offset}")
        
        for i in range(num_spheres):
            # Parse center (12 bytes: 3 floats)
            center_data, new_offset = safe_unpack_check(data, offset, '<fff', f"sphere {i} center")
            if center_data is None:
                img_debugger.warning(f"{version}: Stopping sphere parsing at sphere {i}")
                break
            
            from apps.methods.col_core_classes import Vector3
            center = Vector3(*center_data)
            offset = new_offset
            
            # Parse radius (4 bytes: 1 float)
            radius_data, new_offset = safe_unpack_check(data, offset, '<f', f"sphere {i} radius")
            if radius_data is None:
                img_debugger.warning(f"{version}: Using default radius for sphere {i}")
                radius = 1.0
            else:
                radius = radius_data[0]
                offset = new_offset
            
            # Parse material ID (4 bytes: 1 int)
            material_data, new_offset = safe_unpack_check(data, offset, '<I', f"sphere {i} material")
            if material_data is None:
                img_debugger.warning(f"{version}: Using default material for sphere {i}")
                material_id = 0
            else:
                material_id = material_data[0]
                offset = new_offset
            
            # Parse flags (COL1 only)
            flags = 0
            if version == "COL1":
                flags_data, new_offset = safe_unpack_check(data, offset, '<I', f"sphere {i} flags")
                if flags_data is not None:
                    flags = flags_data[0]
                    offset = new_offset
            
            # Create sphere
            try:
                from apps.methods.col_core_classes import COLMaterial, COLSphere
                material = COLMaterial(material_id, flags=flags)
                sphere = COLSphere(center, radius, material)
                model.spheres.append(sphere)
                
            except Exception as e:
                img_debugger.error(f"{version}: Error creating sphere {i}: {e}")
                continue
        
        img_debugger.debug(f"{version}: Successfully parsed {len(model.spheres)} spheres")
        return offset
        
    except Exception as e:
        img_debugger.error(f"{version}: Error in safe sphere parsing: {e}")
        return offset

def safe_parse_boxes(model, data: bytes, offset: int, num_boxes: int, version: str = "COL1") -> int: #vers 1
    """Safely parse COL boxes with bounds checking"""
    try:
        img_debugger.debug(f"{version}: Parsing {num_boxes} boxes starting at offset {offset}")
        
        for i in range(num_boxes):
            # Parse min point (12 bytes: 3 floats)
            min_data, new_offset = safe_unpack_check(data, offset, '<fff', f"box {i} min point")
            if min_data is None:
                img_debugger.warning(f"{version}: Stopping box parsing at box {i}")
                break
            
            from apps.methods.col_core_classes import Vector3
            min_point = Vector3(*min_data)
            offset = new_offset
            
            # Parse max point (12 bytes: 3 floats)
            max_data, new_offset = safe_unpack_check(data, offset, '<fff', f"box {i} max point")
            if max_data is None:
                img_debugger.warning(f"{version}: Using default max for box {i}")
                max_point = Vector3(min_point.x + 1, min_point.y + 1, min_point.z + 1)
            else:
                max_point = Vector3(*max_data)
                offset = new_offset
            
            # Parse material (4 bytes: 1 int)
            material_data, new_offset = safe_unpack_check(data, offset, '<I', f"box {i} material")
            if material_data is None:
                img_debugger.warning(f"{version}: Using default material for box {i}")
                material_id = 0
            else:
                material_id = material_data[0]
                offset = new_offset
            
            # Parse flags (COL1 only)
            flags = 0
            if version == "COL1":
                flags_data, new_offset = safe_unpack_check(data, offset, '<I', f"box {i} flags")
                if flags_data is not None:
                    flags = flags_data[0]
                    offset = new_offset
            
            # Create box
            try:
                from apps.methods.col_core_classes import COLMaterial, COLBox
                material = COLMaterial(material_id, flags=flags)
                box = COLBox(min_point, max_point, material)
                model.boxes.append(box)
                
            except Exception as e:
                img_debugger.error(f"{version}: Error creating box {i}: {e}")
                continue
        
        img_debugger.debug(f"{version}: Successfully parsed {len(model.boxes)} boxes")
        return offset
        
    except Exception as e:
        img_debugger.error(f"{version}: Error in safe box parsing: {e}")
        return offset

def safe_parse_vertices(model, data: bytes, offset: int, num_vertices: int) -> int: #vers 1
    """Safely parse COL vertices with bounds checking"""
    try:
        img_debugger.debug(f"COL: Parsing {num_vertices} vertices starting at offset {offset}")
        
        for i in range(num_vertices):
            # Parse position (12 bytes: 3 floats)
            pos_data, new_offset = safe_unpack_check(data, offset, '<fff', f"vertex {i} position")
            if pos_data is None:
                img_debugger.warning(f"COL: Stopping vertex parsing at vertex {i}")
                break
            
            from apps.methods.col_core_classes import Vector3, COLVertex
            position = Vector3(*pos_data)
            offset = new_offset
            
            # Create vertex
            try:
                vertex = COLVertex(position)
                model.vertices.append(vertex)
                
            except Exception as e:
                img_debugger.error(f"COL: Error creating vertex {i}: {e}")
                continue
        
        img_debugger.debug(f"COL: Successfully parsed {len(model.vertices)} vertices")
        return offset
        
    except Exception as e:
        img_debugger.error(f"COL: Error in safe vertex parsing: {e}")
        return offset

# Export functions
__all__ = [
    'safe_parse_faces_col1',
    'safe_parse_faces_col23', 
    'safe_parse_spheres',
    'safe_parse_boxes',
    'safe_parse_vertices',
    'safe_unpack_check',
    'validate_data_bounds'
]