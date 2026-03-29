#this belongs in methods/col_workshop_parser.py - Version: 1
# X-Seti - December21 2025 - Col Workshop - COL Binary Parser
"""
COL Binary Parser - Handles parsing binary COL data
Supports COL1 (GTA3/VC) initially, COL2/3 (SA) to be added
Based on GTA Wiki specification
"""

import struct
from typing import Tuple, List, Optional
from apps.debug.debug_functions import img_debugger
from apps.methods.col_workshop_classes import (
    COLHeader, COLBounds, COLSphere, COLBox,
    COLVertex, COLFace, COLModel, COLVersion
)
from apps.methods.col_core_classes import Vector3, COLMaterial, BoundingBox

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


    def parse_spheres(self, data: bytes, offset: int, count: int) -> Tuple[List[COLSphere], int]: #vers 3
        """Parse collision spheres.
        All versions: center(12) + radius(4) + surface(1) + piece(1) + pad(2) = 20 bytes.
        VERIFIED from special.col RE March 2026.
        """
        spheres = []
        for _ in range(count):
            if len(data) < offset + 20:
                raise ValueError("Data too short for sphere")
            cx, cy, cz = struct.unpack('<fff', data[offset:offset+12])
            center = (cx, cy, cz)
            offset += 12
            radius = struct.unpack('<f', data[offset:offset+4])[0]
            offset += 4
            material = data[offset]      # surface type
            flag     = data[offset + 1]  # piece
            offset += 4  # surface(1) + piece(1) + pad(2)
            sphere = COLSphere(radius=radius, center=center,
                               material=material, flag=flag,
                               brightness=0, light=0)
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
            # All versions: sphere = center(12) + radius(4) + surface(1) + piece(1) + pad(2) = 20 bytes
            # VERIFIED from special.col RE March 2026
            sphere_size = 20

            if len(data) < offset + (count * sphere_size):
                raise ValueError(f"Data too short for {count} spheres")

            for i in range(count):
                cx, cy, cz = struct.unpack('<fff', data[offset:offset+12])
                center = Vector3(cx, cy, cz)
                offset += 12
                radius = struct.unpack('<f', data[offset:offset+4])[0]
                offset += 4
                surface = data[offset]
                piece   = data[offset+1]
                offset += 4  # surface(1) + piece(1) + pad(2)
                material = COLMaterial(surface, piece)
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
            # All versions: box = min(12) + max(12) + surface(4) = 28 bytes
            # DragonFF format string "VVS" = vec3+vec3+surface = 12+12+4 = 28
            box_size = 28

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

                # Surface properties (4 bytes: material, flag, brightness, light)
                material_id = data[offset]
                flag        = data[offset + 1]
                brightness  = data[offset + 2]
                light_val   = data[offset + 3]
                offset += 4

                material = COLMaterial(material_id, flag)
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
            # COL1: uint32 indices + mat(u8) + light(u8) + pad(u16) = 16 bytes
            # VERIFIED from special.col RE March 2026
            for _ in range(count):
                if len(data) < offset + 16:
                    raise ValueError("Data too short for COL1 face")
                a, b, c = struct.unpack('<III', data[offset:offset+12])
                offset += 12
                material = data[offset]      # surface type (e.g. 63=concrete)
                light    = data[offset + 1]  # light value
                # pad uint16 at offset+2
                offset += 4
                face = COLFace(
                    a=a, b=b, c=c,
                    material=material,
                    flag=0,
                    brightness=0,
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
            # DragonFF: COL1 = "IIIS" = uint32+uint32+uint32+surface = 12+4 = 16 bytes
            #           COL2/3 = "HHHBB" = uint16+uint16+uint16+u8+u8  = 6+2  = 8 bytes
            face_size = 16 if version == COLVersion.COL_1 else 8

            if len(data) < offset + (count * face_size):
                raise ValueError(f"Data too short for {count} faces")

            for i in range(count):
                if version == COLVersion.COL_1:
                    # COL1: 3x uint32 indices + material(u8) + light(u8) + pad(u16) = 16 bytes
                    a, b, c = struct.unpack('<III', data[offset:offset+12])
                    offset += 12
                    material_id = data[offset]
                    light       = data[offset + 1]
                    offset += 4  # mat(1) + light(1) + pad(2)
                    material = COLMaterial(material_id, 0)
                else:
                    # COL2/3: 3x uint16 indices + material(u8) + light(u8) = 8 bytes
                    a, b, c = struct.unpack('<HHH', data[offset:offset+6])
                    offset += 6
                    material_id = data[offset]
                    light       = data[offset + 1]
                    offset += 2
                    material = COLMaterial(material_id, 0)

                face = COLFace((a, b, c), material, light)
                faces.append(face)

            if self.debug:
                img_debugger.debug(f"Parsed {len(faces)} faces")

            return faces, offset

        except Exception as e:
            raise ValueError(f"Faces parse error: {str(e)}")


    def parse_model(self, data: bytes, offset: int = 0) -> Tuple[Optional[COLModel], int]: #vers 3
        """Parse complete COL model (COL1/2/3/4).

        COL1 layout (VERIFIED from special.col RE, March 2026):
          header(32) -> bounds(40) -> n_spheres -> spheres[] -> n_boxes -> boxes[]
          -> n_facegroups -> facegroups[] -> n_verts -> verts[] -> n_faces -> faces[]

        COL2/3 layout:
          header(32) -> bounds(40) -> n_spheres -> spheres[] -> n_boxes -> boxes[]
          -> n_verts -> verts[] -> n_facegroups -> facegroups[] -> n_faces -> faces[]

        Returns: (COLModel, new_offset) or (None, offset) on error.
        """
        start_offset = offset
        try:
            # Parse header (32 bytes: fourcc+size+name+model_id)
            header, offset = self.parse_header(data, offset)
            version = header.version

            # Parse bounds (bounding sphere + bounding box = 40 bytes)
            bounds, offset = self.parse_bounds(data, offset, version)

            # ── COL1: interleaved counts+data ────────────────────────
            if version == COLVersion.COL_1:
                # DragonFF __read_legacy_col order:
                #   spheres → skip4(unknown) → boxes → vertices → faces
                # Spheres
                num_spheres = struct.unpack_from('<I', data, offset)[0]; offset += 4
                spheres, offset = self.parse_spheres(data, offset, num_spheres)

                # Skip num_unknown/lines (4 bytes, always 0 in COL1)
                # DragonFF: self.__incr(4) — placed AFTER spheres, BEFORE boxes
                offset += 4

                # Boxes
                num_boxes = struct.unpack_from('<I', data, offset)[0]; offset += 4
                boxes, offset = self.parse_boxes(data, offset, num_boxes)

                # Vertices (float x3 = 12 bytes each in COL1)
                num_vertices = struct.unpack_from('<I', data, offset)[0]; offset += 4
                vertices, offset = self.parse_vertices(data, offset, num_vertices, version)

                # Faces (uint32 x3 + mat + light + pad = 16 bytes in COL1)
                num_faces = struct.unpack_from('<I', data, offset)[0]; offset += 4
                faces, offset = self.parse_faces(data, offset, num_faces, version)

            # ── COL2/3/4: offset-table layout — matched to DragonFF __read_new_col ──
            # DragonFF format "<HHHBxIIIIIII" = 36 bytes:
            #   sphere_count(H) box_count(H) face_count(H) line_count(B) pad(x)
            #   flags(I) spheres_off(I) boxes_off(I) lines_off(I)
            #   verts_off(I) faces_off(I) tri_planes_off(I)
            # COL3 adds 12 more bytes: shadow_face_count(I) shadow_verts_off(I) shadow_faces_off(I)
            # COL4 adds 4 more bytes after that.
            #
            # DragonFF: offsets are relative to `pos` = file position of the new_col header
            # (i.e. directly after the bounds block). data_at(off) = pos + off + 4
            # The +4 skips the uint32 item-count embedded before each data block.
            else:
                # DragonFF: self._pos = pos + offset + 4
                # where pos = file position of the model's fourcc (= start_offset)
                # So all offsets are relative to start_offset (fourcc position).
                block_base = start_offset  # = file position of the COL fourcc

                # Read 36-byte header: counts + pad + flags + 6 offsets
                (num_spheres, num_boxes, num_faces, num_lines_byte,
                 flags,
                 spheres_off, boxes_off, lines_off,
                 verts_off, faces_off, tri_off) = \
                    struct.unpack_from('<HHHBxIIIIIII', data, offset)
                offset += 36
                model_flags = flags

                # COL3+: shadow mesh counts and offsets (12 bytes)
                shadow_face_count = 0
                shadow_verts_off  = 0
                shadow_faces_off  = 0
                if version.value >= 3:
                    shadow_face_count, shadow_verts_off, shadow_faces_off = \
                        struct.unpack_from('<III', data, offset)
                    offset += 12

                # COL4: extra 4 bytes
                if version.value >= 4:
                    offset += 4

                # DragonFF: offsets point to (count_uint32 + data).
                # Access data at: block_base + offset + 4  (skip the embedded count)
                def data_at(off):
                    return block_base + off + 4

                # ── Spheres ───────────────────────────────────────────────
                if num_spheres > 0 and spheres_off > 0:
                    spheres, _ = self.parse_spheres(
                        data, data_at(spheres_off), num_spheres)
                else:
                    spheres = []

                # ── Boxes ─────────────────────────────────────────────────
                if num_boxes > 0 and boxes_off > 0:
                    boxes, _ = self.parse_boxes(
                        data, data_at(boxes_off), num_boxes)
                else:
                    boxes = []

                # ── Faces (read before vertices — need indices for vert count) ──
                if num_faces > 0 and faces_off > 0:
                    faces, _ = self.parse_faces(
                        data, data_at(faces_off), num_faces, version)
                else:
                    faces = []

                # ── Vertices — count derived from face indices (DragonFF method) ──
                vertices = []
                if verts_off > 0 and faces:
                    num_vertices = max(
                        (max(f.a, f.b, f.c) for f in faces
                         if hasattr(f, 'a')),
                        default=-1
                    ) + 1
                    if num_vertices > 0:
                        vertices, _ = self.parse_vertices(
                            data, data_at(verts_off), num_vertices, version)
                elif verts_off > 0 and faces_off > verts_off:
                    # Fallback when no faces: infer from offset gap (6 bytes/vert)
                    num_vertices = (faces_off - verts_off) // 6
                    if num_vertices > 0:
                        vertices, _ = self.parse_vertices(
                            data, data_at(verts_off), num_vertices, version)

            # Sanity checks
            if (len(spheres) > 10000 or len(boxes) > 10000
                    or len(vertices) > 200000 or len(faces) > 200000):
                if self.debug:
                    print(f"parse_model: implausible counts S={len(spheres)} "
                          f"B={len(boxes)} V={len(vertices)} F={len(faces)}")
                return None, start_offset

            # Build model
            model = COLModel(
                header=header,
                bounds=bounds,
                spheres=spheres,
                boxes=boxes,
                vertices=vertices,
                faces=faces,
            )
            model.name     = header.name
            model.version  = header.version
            model.model_id = header.model_id

            # Always advance by header-declared size (DragonFF: pos + file_size + 8).
            # For COL2/3 the data blocks are read by jumping with data_at(), not
            # advancing `offset` sequentially, so `offset` is wrong as a return value.
            next_model_offset = start_offset + header.size + 8

            if self.debug:
                print(f"parse_model OK: '{header.name}' {version.name} "
                      f"S={len(spheres)} B={len(boxes)} "
                      f"V={len(vertices)} F={len(faces)} "
                      f"(next=0x{next_model_offset:X})")

            return model, next_model_offset

        except Exception as e:
            import traceback
            if self.debug:
                print(f"parse_model FAILED at 0x{start_offset:X}: {e}")
                traceback.print_exc()
            return None, start_offset


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
        
        # COL1 layout (DragonFF __read_legacy_col):
        #   num_spheres(4) → spheres[] → skip4(unknown) → num_boxes(4) → boxes[]
        #   → num_verts(4) → verts[] → num_faces(4) → faces[]
        # NOTE: skip4 is AFTER sphere data, BEFORE num_boxes — not a count block.

        if self.debug:
            print(f"COL1 parse at offset 0x{offset:X}")

        # Spheres
        num_spheres = struct.unpack('<I', data[offset:offset+4])[0]; offset += 4
        spheres, offset = self.parse_spheres(data, offset, num_spheres)

        # Skip unknown (4 bytes, always 0) — AFTER spheres, BEFORE boxes
        offset += 4

        # Boxes
        num_boxes = struct.unpack('<I', data[offset:offset+4])[0]; offset += 4
        boxes, offset = self.parse_boxes(data, offset, num_boxes)

        # Vertices
        num_vertices = struct.unpack('<I', data[offset:offset+4])[0]; offset += 4
        vertices, offset = self.parse_vertices(data, offset, num_vertices, header.version)

        # Faces
        num_faces = struct.unpack('<I', data[offset:offset+4])[0]; offset += 4
        faces, offset = self.parse_faces(data, offset, num_faces, header.version)

        if self.debug:
            print(f"COL1 Counts: S={num_spheres} B={num_boxes} V={num_vertices} F={num_faces}")
        
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
