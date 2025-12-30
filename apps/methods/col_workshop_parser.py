#this belongs in methods/col_workshop_parser.py - Version: 1
# X-Seti - December21 2025 - Col Workshop - COL Binary Parser
"""
COL Binary Parser - Handles parsing binary COL data
Supports COL1 (GTA3/VC) initially, COL2/3 (SA) to be added
Based on GTA Wiki specification
"""

import struct
from typing import Tuple, List, Optional
from apps.methods.col_workshop_classes import (
    COLHeader, COLBounds, COLSphere, COLBox, 
    COLVertex, COLFace, COLModel, COLVersion
)

##Classes list -
# COLParser

class COLParser: #vers 1
    """Binary parser for COL files"""
    
    def __init__(self, debug: bool = False): #vers 1
        """Initialize parser"""
        self.debug = debug
        
    def parse_header(self, data: bytes, offset: int = 0) -> Tuple[COLHeader, int]: #vers 1
        """
        Parse COL header - 32 bytes total
        
        Returns: (COLHeader, new_offset)
        """
        if len(data) < offset + 32:
            raise ValueError("Data too short for COL header")
        
        # Read FourCC (4 bytes)
        fourcc = data[offset:offset+4]
        offset += 4
        
        # Read size (4 bytes)
        size = struct.unpack('<I', data[offset:offset+4])[0]
        offset += 4
        
        # Read name (22 bytes, null-terminated)
        name_bytes = data[offset:offset+22]
        name = name_bytes.split(b'\x00')[0].decode('ascii', errors='ignore')
        offset += 22
        
        # Read model ID (2 bytes)
        model_id = struct.unpack('<H', data[offset:offset+2])[0]
        offset += 2
        
        # Determine version from fourcc
        version = self._fourcc_to_version(fourcc)
        
        header = COLHeader(
            fourcc=fourcc,
            size=size,
            name=name,
            model_id=model_id,
            version=version
        )
        
        if self.debug:
            print(f"Header: {fourcc} v{version.value}, '{name}', size={size}")
        
        return header, offset
    

    def parse_header_alt(self, data: bytes, offset: int = 0) -> Tuple[str, int, str, int, COLVersion, int]: #vers 1
        """Parse COL model header (32 bytes)

        Returns: (signature, file_size, model_name, model_id, version, new_offset)
        """
        try:
            if len(data) < offset + 32:
                raise ValueError("Data too short for COL header")

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
            version = COLVersion.COL_1
            if signature.startswith('COL'):
                version_char = signature[3] if len(signature) > 3 else '1'
                if version_char == '\x02':
                    version = COLVersion.COL_2
                elif version_char == '\x03':
                    version = COLVersion.COL_3
                elif version_char == '\x04':
                    version = COLVersion.COL_4

            if self.debug:
                img_debugger.debug(f"Header: {signature} v{version.value}, '{model_name}', ID:{model_id}")

            return signature, file_size, model_name, model_id, version, offset

        except Exception as e:
            raise ValueError(f"Header parse error: {str(e)}")


    def parse_bounds(self, data: bytes, offset: int, version: COLVersion) -> Tuple[COLBounds, int]: #vers 1
        """
        Parse COL bounds - 40 bytes
        COL1: radius, center, min, max
        COL2/3: min, max, center, radius (reordered)
        """
        if len(data) < offset + 40:
            raise ValueError("Data too short for bounds")
        
        if version == COLVersion.COL_1:
            # COL1 order: radius, center, min, max
            radius = struct.unpack('<f', data[offset:offset+4])[0]
            offset += 4
            
            center = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            min_pt = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            max_pt = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
        else:
            # COL2/3 order: min, max, center, radius
            min_pt = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            max_pt = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            center = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            radius = struct.unpack('<f', data[offset:offset+4])[0]
            offset += 4
        
        bounds = COLBounds(
            radius=radius,
            center=center,
            min=min_pt,
            max=max_pt
        )
        
        return bounds, offset
    

    def parse_bounds_alt(self, data: bytes, offset: int, version: COLVersion) -> Tuple[COLBounds, int]: #vers 1
        """Parse bounding box (40 bytes for COL1, 28 bytes for COL2/3)

        Returns: (bounding_box, new_offset)
        """
        try:
            bbox = COLBounds()

            if version == COLVersion.COL_1:
                # COL1: radius + center + min + max (40 bytes)
                if len(data) < offset + 40:
                    raise ValueError("Data too short for COL1 bounds")

                bbox.radius = struct.unpack('<f', data[offset:offset+4])[0]
                offset += 4

                cx, cy, cz = struct.unpack('<fff', data[offset:offset+12])
                bbox.center = Vector3(cx, cy, cz)
                offset += 12

                min_x, min_y, min_z = struct.unpack('<fff', data[offset:offset+12])
                bbox.min = Vector3(min_x, min_y, min_z)
                offset += 12

                max_x, max_y, max_z = struct.unpack('<fff', data[offset:offset+12])
                bbox.max = Vector3(max_x, max_y, max_z)
                offset += 12
            else:
                # COL2/3: min + max + center + radius (28 bytes)
                if len(data) < offset + 28:
                    raise ValueError("Data too short for COL2/3 bounds")

                min_x, min_y, min_z = struct.unpack('<fff', data[offset:offset+12])
                bbox.min = Vector3(min_x, min_y, min_z)
                offset += 12

                max_x, max_y, max_z = struct.unpack('<fff', data[offset:offset+12])
                bbox.max = Vector3(max_x, max_y, max_z)
                offset += 12

                cx, cy, cz = struct.unpack('<fff', data[offset:offset+12])
                bbox.center = Vector3(cx, cy, cz)
                offset += 12

                bbox.radius = struct.unpack('<f', data[offset:offset+4])[0]
                offset += 4

            if self.debug:
                img_debugger.debug(f"Bounds: r={bbox.radius:.2f}, center={bbox.center}")

            return bbox, offset

        except Exception as e:
            raise ValueError(f"Bounds parse error: {str(e)}")


    def parse_spheres(self, data: bytes, offset: int, count: int) -> Tuple[List[COLSphere], int]: #vers 1
        """Parse collision spheres - 20 bytes each (COL1)"""
        spheres = []
        
        for _ in range(count):
            if len(data) < offset + 20:
                raise ValueError("Data too short for sphere")
            
            # Radius (4 bytes)
            radius = struct.unpack('<f', data[offset:offset+4])[0]
            offset += 4
            
            # Center (12 bytes)
            center = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            # Parse spheres
            num_spheres = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            spheres, offset = self.parse_spheres(data, offset, num_spheres)

            # Skip unknown (always 0)
            offset += 4

            # Parse boxes
            num_boxes = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            boxes, offset = self.parse_boxes(data, offset, num_boxes)

            # Parse vertices
            num_vertices = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            vertices, offset = self.parse_vertices(data, offset, num_vertices, header.version)

            # Parse faces
            num_faces = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4
            faces, offset = self.parse_faces(data, offset, num_faces, header.version)

            # Surface properties (4 bytes)
            material = data[offset]
            flag = data[offset + 1]
            brightness = data[offset + 2]
            light = data[offset + 3]
            offset += 4
            
            sphere = COLSphere(
                radius=radius,
                center=center,
                material=material,
                flag=flag,
                brightness=brightness,
                light=light
            )
            spheres.append(sphere)
        
        return spheres, offset
    

    def parse_spheres_alt(self, data: bytes, offset: int, count: int, version: COLVersion) -> Tuple[list, int]: #vers 1
        """Parse collision spheres

        COL1: 24 bytes each (center + radius + material + flags)
        COL2/3: 20 bytes each (center + radius + material)

        Returns: (spheres_list, new_offset)
        """
        try:
            spheres = []
            sphere_size = 24 if version == COLVersion.COL_1 else 20

            if len(data) < offset + (count * sphere_size):
                raise ValueError(f"Data too short for {count} spheres")

            for i in range(count):
                # Center (12 bytes)
                cx, cy, cz = struct.unpack('<fff', data[offset:offset+12])
                center = Vector3(cx, cy, cz)
                offset += 12

                # Radius (4 bytes)
                radius = struct.unpack('<f', data[offset:offset+4])[0]
                offset += 4

                # Material (4 bytes)
                material_id = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4

                # Flags (COL1 only, 4 bytes)
                flags = 0
                if version == COLVersion.COL_1:
                    flags = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4

                material = COLMaterial(material_id, flags)
                sphere = COLSphere(center, radius, material)
                spheres.append(sphere)

            if self.debug:
                img_debugger.debug(f"Parsed {len(spheres)} spheres")

            return spheres, offset

        except Exception as e:
            raise ValueError(f"Spheres parse error: {str(e)}")


    def parse_boxes(self, data: bytes, offset: int, count: int) -> Tuple[List[COLBox], int]: #vers 1
        """Parse collision boxes - 28 bytes each"""
        boxes = []
        
        for _ in range(count):
            if len(data) < offset + 28:
                raise ValueError("Data too short for box")
            
            # Min point (12 bytes)
            min_pt = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            # Max point (12 bytes)
            max_pt = struct.unpack('<fff', data[offset:offset+12])
            offset += 12
            
            # Surface properties (4 bytes)
            material = data[offset]
            flag = data[offset + 1]
            brightness = data[offset + 2]
            light = data[offset + 3]
            offset += 4
            
            box = COLBox(
                min=min_pt,
                max=max_pt,
                material=material,
                flag=flag,
                brightness=brightness,
                light=light
            )
            boxes.append(box)
        
        return boxes, offset
    

    def parse_boxes_alt(self, data: bytes, offset: int, count: int, version: COLVersion) -> Tuple[list, int]: #vers 1
        """Parse collision boxes

        COL1: 32 bytes each (min + max + material + flags)
        COL2/3: 28 bytes each (min + max + material)

        Returns: (boxes_list, new_offset)
        """
        try:
            boxes = []
            box_size = 32 if version == COLVersion.COL_1 else 28

            if len(data) < offset + (count * box_size):
                raise ValueError(f"Data too short for {count} boxes")

            for i in range(count):
                # Min point (12 bytes)
                min_x, min_y, min_z = struct.unpack('<fff', data[offset:offset+12])
                min_point = Vector3(min_x, min_y, min_z)
                offset += 12

                # Max point (12 bytes)
                max_x, max_y, max_z = struct.unpack('<fff', data[offset:offset+12])
                max_point = Vector3(max_x, max_y, max_z)
                offset += 12

                # Material (4 bytes)
                material_id = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4

                # Flags (COL1 only, 4 bytes)
                flags = 0
                if version == COLVersion.COL_1:
                    flags = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4

                material = COLMaterial(material_id, flags)
                box = COLBox(min_point, max_point, material)
                boxes.append(box)

            if self.debug:
                img_debugger.debug(f"Parsed {len(boxes)} boxes")

            return boxes, offset

        except Exception as e:
            raise ValueError(f"Boxes parse error: {str(e)}")


    def parse_vertices(self, data: bytes, offset: int, count: int, version: COLVersion) -> Tuple[List[COLVertex], int]: #vers 1
        """
        Parse mesh vertices
        COL1: 12 bytes each (3 floats)
        COL2/3: 6 bytes each (3 int16 - fixed point)
        """
        vertices = []
        
        if version == COLVersion.COL_1:
            # COL1: float vertices
            for _ in range(count):
                if len(data) < offset + 12:
                    raise ValueError("Data too short for vertex")
                
                x, y, z = struct.unpack('<fff', data[offset:offset+12])
                offset += 12
                
                vertices.append(COLVertex(x=x, y=y, z=z))
        else:
            # COL2/3: int16 fixed-point vertices (divide by 128.0)
            for _ in range(count):
                if len(data) < offset + 6:
                    raise ValueError("Data too short for vertex")
                
                ix, iy, iz = struct.unpack('<hhh', data[offset:offset+6])
                offset += 6
                
                # Convert fixed-point to float
                x = ix / 128.0
                y = iy / 128.0
                z = iz / 128.0
                
                vertices.append(COLVertex(x=x, y=y, z=z))
        
        return vertices, offset
    

    def parse_vertices_alt(self, data: bytes, offset: int, count: int, version: COLVersion) -> Tuple[list, int]: #vers 1
        """Parse mesh vertices

        COL1: 12 bytes each (3 floats)
        COL2/3: 6 bytes each (3 int16, CRITICAL: divide by 128.0 for fixed-point conversion)

        Returns: (vertices_list, new_offset)
        """
        try:
            vertices = []

            if version == COLVersion.COL_1:
                # COL1: float vertices (12 bytes each)
                vertex_size = 12
                if len(data) < offset + (count * vertex_size):
                    raise ValueError(f"Data too short for {count} COL1 vertices")

                for i in range(count):
                    x, y, z = struct.unpack('<fff', data[offset:offset+12])
                    position = Vector3(x, y, z)
                    offset += 12
                    vertices.append(COLVertex(position))
            else:
                # COL2/3: int16 vertices (6 bytes each) - CRITICAL FIX: divide by 128.0
                vertex_size = 6
                if len(data) < offset + (count * vertex_size):
                    raise ValueError(f"Data too short for {count} COL2/3 vertices")

                for i in range(count):
                    x_int, y_int, z_int = struct.unpack('<hhh', data[offset:offset+6])
                    # CRITICAL: Convert int16 fixed-point to float
                    x = x_int / 128.0
                    y = y_int / 128.0
                    z = z_int / 128.0
                    position = Vector3(x, y, z)
                    offset += 6
                    vertices.append(COLVertex(position))

            if self.debug:
                img_debugger.debug(f"Parsed {len(vertices)} vertices")

            return vertices, offset

        except Exception as e:
            raise ValueError(f"Vertices parse error: {str(e)}")


    def parse_faces(self, data: bytes, offset: int, count: int, version: COLVersion) -> Tuple[List[COLFace], int]: #vers 1
        """
        Parse mesh faces
        COL1: 16 bytes each (3 uint32 + 4 bytes surface)
        COL2/3: 8 bytes each (3 uint16 + 2 bytes material/light)
        """
        faces = []
        
        if version == COLVersion.COL_1:
            # COL1: uint32 indices
            for _ in range(count):
                if len(data) < offset + 16:
                    raise ValueError("Data too short for face")
                
                # Vertex indices (12 bytes)
                a, b, c = struct.unpack('<III', data[offset:offset+12])
                offset += 12
                
                # Surface properties (4 bytes)
                material = data[offset]
                flag = data[offset + 1]
                brightness = data[offset + 2]
                light = data[offset + 3]
                offset += 4
                
                face = COLFace(
                    a=a, b=b, c=c,
                    material=material,
                    flag=flag,
                    brightness=brightness,
                    light=light
                )
                faces.append(face)
        else:
            # COL2/3: uint16 indices
            for _ in range(count):
                if len(data) < offset + 8:
                    raise ValueError("Data too short for face")
                
                # Vertex indices (6 bytes)
                a, b, c = struct.unpack('<HHH', data[offset:offset+6])
                offset += 6
                
                # Material and light (2 bytes)
                material = data[offset]
                light = data[offset + 1]
                offset += 2
                
                face = COLFace(
                    a=a, b=b, c=c,
                    material=material,
                    flag=0,
                    brightness=0,
                    light=light
                )
                faces.append(face)
        
        return faces, offset
    

    def parse_faces_alt(self, data: bytes, offset: int, count: int, version: COLVersion) -> Tuple[list, int]: #vers 1
        """Parse mesh faces

        COL1: 16 bytes each (indices + material + light + flags)
        COL2/3: 12 bytes each (indices + material + light + padding)

        Returns: (faces_list, new_offset)
        """
        try:
            faces = []
            face_size = 16 if version == COLVersion.COL_1 else 12

            if len(data) < offset + (count * face_size):
                raise ValueError(f"Data too short for {count} faces")

            for i in range(count):
                # Vertex indices (6 bytes - 3 uint16)
                a, b, c = struct.unpack('<HHH', data[offset:offset+6])
                vertex_indices = (a, b, c)
                offset += 6

                # Material (2 bytes)
                material_id = struct.unpack('<H', data[offset:offset+2])[0]
                offset += 2

                # Light (2 bytes)
                light = struct.unpack('<H', data[offset:offset+2])[0]
                offset += 2

                if version == COLVersion.COL_1:
                    # Flags (4 bytes)
                    flags = struct.unpack('<I', data[offset:offset+4])[0]
                    offset += 4
                    material = COLMaterial(material_id, flags)
                else:
                    # Padding (2 bytes)
                    offset += 2
                    material = COLMaterial(material_id, 0)

                face = COLFace(vertex_indices, material, light)
                faces.append(face)

            if self.debug:
                img_debugger.debug(f"Parsed {len(faces)} faces")

            return faces, offset

        except Exception as e:
            raise ValueError(f"Faces parse error: {str(e)}")


    def parse_model(self, data: bytes, offset: int = 0) -> Tuple[Optional[COLModel], int]: #vers 1
        """Parse complete COL model

        Returns: (model, new_offset) or (None, offset) on error
        """
        try:
            model = COLModel()
            start_offset = offset

            # Parse header (32 bytes)
            header, offset = self.parse_header(data, offset)
            model_name = header.name
            model_id = header.model_id
            version = header.version
            model.name = model_name
            model.model_id = model_id
            model.version = version

            # Parse bounds
            model.bounding_box, offset = self.parse_bounds(data, offset, version)

            # Parse counts
            num_spheres, num_boxes, num_vertices, num_faces, offset = self.parse_counts(data, offset, version)

            # Sanity check counts - reject obviously corrupt data
            if num_spheres > 10000:
                if self.debug:
                    img_debugger.error(f"Invalid sphere count: {num_spheres} (max 10000)")
                return None, offset

            if num_boxes > 10000:
                if self.debug:
                    img_debugger.error(f"Invalid box count: {num_boxes} (max 10000)")
                return None, offset

            if num_vertices > 100000:
                if self.debug:
                    img_debugger.error(f"Invalid vertex count: {num_vertices} (max 100000)")
                return None, offset

            if num_faces > 100000:
                if self.debug:
                    img_debugger.error(f"Invalid face count: {num_faces} (max 100000)")
                return None, offset

            # Parse collision elements
            model.spheres, offset = self.parse_spheres(data, offset, num_spheres, version)
            model.boxes, offset = self.parse_boxes(data, offset, num_boxes, version)
            model.vertices, offset = self.parse_vertices(data, offset, num_vertices, version)


            model.faces, offset = self.parse_faces(data, offset, num_faces, version)

            # Update flags
            model.update_flags()

            bytes_read = offset - start_offset
            if self.debug:
                img_debugger.success(f"Model parsed: {bytes_read} bytes, {model.get_stats()}")

            return model, offset

        except Exception as e:
            import traceback
            if self.debug:
                img_debugger.error(f"Model parse failed: {str(e)}")
                img_debugger.error(traceback.format_exc())
            return None, offset


    def parse_counts(self, data: bytes, offset: int, version: COLVersion) -> Tuple[int, int, int, int, int]: #vers 2
        """Parse collision element counts

        Returns: (num_spheres, num_boxes, num_vertices, num_faces, new_offset)
        """
        try:
            if version == COLVersion.COL_1:
                # COL1: spheres, unknown, boxes, vertices, faces (20 bytes)
                if len(data) < offset + 20:
                    raise ValueError("Data too short for COL1 counts")

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
            else:
                # COL2/3: spheres, boxes, faces, vertices (16 bytes)
                if len(data) < offset + 16:
                    raise ValueError("Data too short for COL2/3 counts")

                num_spheres = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_boxes = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_faces = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4
                num_vertices = struct.unpack('<I', data[offset:offset+4])[0]
                offset += 4

            if self.debug:
                img_debugger.debug(f"Counts: S:{num_spheres} B:{num_boxes} V:{num_vertices} F:{num_faces}")

            return num_spheres, num_boxes, num_vertices, num_faces, offset

        except Exception as e:
            raise ValueError(f"Counts parse error: {str(e)}")


    def parse_col1_model(self, data: bytes, offset: int = 0) -> Tuple[COLModel, int]: #vers 1
        """Parse complete COL1 model"""
        start_offset = offset
        
        # Parse header
        header, offset = self.parse_header(data, offset)
        
        if header.version != COLVersion.COL_1:
            raise ValueError(f"Expected COL1, got {header.version}")
        
        # Parse bounds
        bounds, offset = self.parse_bounds(data, offset, header.version)
        
        # Read counts (COL1 format)
        if len(data) < offset + 20:
            raise ValueError("Data too short for COL1 counts")
        
        num_spheres = struct.unpack('<I', data[offset:offset+4])[0]
        offset += 4
        
        num_unknown = struct.unpack('<I', data[offset:offset+4])[0]  # Always 0
        offset += 4
        
        num_boxes = struct.unpack('<I', data[offset:offset+4])[0]
        offset += 4
        
        num_vertices = struct.unpack('<I', data[offset:offset+4])[0]
        offset += 4
        
        num_faces = struct.unpack('<I', data[offset:offset+4])[0]
        offset += 4
        
        if self.debug:
            print(f"COL1 Counts: S={num_spheres} B={num_boxes} V={num_vertices} F={num_faces}")
        
        # Parse spheres
        spheres, offset = self.parse_spheres(data, offset, num_spheres)
        
        # Parse boxes
        boxes, offset = self.parse_boxes(data, offset, num_boxes)
        
        # Parse vertices
        vertices, offset = self.parse_vertices(data, offset, num_vertices, header.version)
        
        # Parse faces
        faces, offset = self.parse_faces(data, offset, num_faces, header.version)
        
        model = COLModel(
            header=header,
            bounds=bounds,
            spheres=spheres,
            boxes=boxes,
            vertices=vertices,
            faces=faces
        )
        
        return model, offset
    
    def _fourcc_to_version(self, fourcc: bytes) -> COLVersion: #vers 1
        """Convert FourCC to version enum"""
        if fourcc == b'COLL':
            return COLVersion.COL_1
        elif fourcc == b'COL2':
            return COLVersion.COL_2
        elif fourcc == b'COL3':
            return COLVersion.COL_3
        elif fourcc == b'COL4':
            return COLVersion.COL_4
        else:
            raise ValueError(f"Unknown COL FourCC: {fourcc}")

# Export parser
__all__ = ['COLParser']
