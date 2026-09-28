#this belongs in apps/methods/col_workshop_parser.py - Version: 11
# X-Seti - May08 2026 - Col Workshop - COL Binary Parser
"""
COL Binary Parser - Handles parsing binary COL data
Supports COL1 (GTA3/VC) initially, COL2/3 (SA) to be added
Based on GTA Wiki specification
"""

import struct
from typing import Tuple, List, Optional
from apps.methods.col_workshop_classes import (COLHeader, COLBounds, COLSphere, COLBox, COLVertex, COLFace, COLModel, COLVersion)

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
    

    def parse_spheres(self, data: bytes, offset: int, count: int,
                      version: COLVersion = COLVersion.COL_1) -> Tuple[List[COLSphere], int]: #vers 5
        """Parse collision spheres, 20 bytes each.
        COL1: radius(4) + center(12) + surface(4); COL2+: center(12) + radius(4) + surface(4).
        """
        spheres = []
        for _ in range(count):
            if len(data) < offset + 20:
                raise ValueError("Data too short for sphere")
            if version == COLVersion.COL_1:
                radius = struct.unpack_from('<f', data, offset)[0]
                center = struct.unpack_from('<fff', data, offset + 4)
            else:
                center = struct.unpack_from('<fff', data, offset)
                radius = struct.unpack_from('<f', data, offset + 12)[0]
            offset += 16
            material, flag, bright, light = data[offset:offset + 4]   # surface
            offset += 4
            sphere = COLSphere(radius=radius, center=center,
                               material=material, flag=flag,
                               brightness=bright, light=light)
            spheres.append(sphere)
        return spheres, offset
    

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
    

    def parse_model(self, data: bytes, offset: int = 0) -> Tuple[Optional[COLModel], int]: #vers 4
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

            #    COL1: interleaved counts+data                         
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

            #    COL2/3/4: offset-table layout — matched to DragonFF __read_new_col   
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

                #    Spheres                                                
                if num_spheres > 0 and spheres_off > 0:
                    spheres, _ = self.parse_spheres(
                        data, data_at(spheres_off), num_spheres, version)
                else:
                    spheres = []

                #    Boxes                                                  
                if num_boxes > 0 and boxes_off > 0:
                    boxes, _ = self.parse_boxes(
                        data, data_at(boxes_off), num_boxes)
                else:
                    boxes = []

                #    Faces (read before vertices — need indices for vert count)   
                if num_faces > 0 and faces_off > 0:
                    faces, _ = self.parse_faces(
                        data, data_at(faces_off), num_faces, version)
                else:
                    faces = []

                #    Vertices — count derived from face indices (DragonFF method)   
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

            # Sanity checks — limits raised for large SA COL files
            # SA collision archives can have models with 500k+ vertices/faces
            if (len(spheres) > 50000 or len(boxes) > 50000
                    or len(vertices) > 2_000_000 or len(faces) > 2_000_000):
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


class COLWriter: #vers 1
    """Serialise COLModel objects back to binary COL format.
    Supports COL1 (GTA3/VC) and COL2/3 (SA).
    The output is a concatenation of model chunks — identical to the
    on-disk format so the result can be written directly to a .col file.
    """

    # COL2/3 bounds: sphere(16) + unk(4) + box(24) + unk(4) = 48 bytes
    # Actually from DragonFF reference:
    # COL2 bounds: min(12) + max(12) + center(12) + radius(4) = 40 bytes
    # COL1 bounds: min(12) + max(12) + center(12) + radius(4) = 40 bytes

    @staticmethod
    def _mat_id(face) -> int:
        """Extract integer material id from a face's material field."""
        m = face.material
        if isinstance(m, int):
            return m & 0xFF
        return getattr(m, 'material_id', 0) & 0xFF

    @staticmethod
    def _v3(v) -> bytes:
        """Pack a Vector3, tuple, list, or None to 12 bytes."""
        import struct
        if v is None:
            return struct.pack('<fff', 0.0, 0.0, 0.0)
        if isinstance(v, (tuple, list)):
            return struct.pack('<fff', float(v[0]), float(v[1]), float(v[2]))
        return struct.pack('<fff', float(v.x), float(v.y), float(v.z))

    @classmethod
    def _write_bounds(cls, bounds, ver=None) -> bytes: #vers 2
        """Serialise COLBounds to 40 bytes in the version's field order."""
        import struct
        mn = getattr(bounds, 'min',    None) or getattr(bounds, 'min_point', None)
        mx = getattr(bounds, 'max',    None) or getattr(bounds, 'max_point', None)
        ct = getattr(bounds, 'center', None)
        rd = struct.pack('<f', float(getattr(bounds, 'radius', 0.0)))
        if ver == COLVersion.COL_1:
            return rd + cls._v3(ct) + cls._v3(mn) + cls._v3(mx)
        return cls._v3(mn) + cls._v3(mx) + cls._v3(ct) + rd

    @classmethod
    def write_model(cls, model) -> bytes: #vers 2
        """Serialise one COLModel to bytes (header + payload)."""
        import struct

        ver   = model.header.version
        name  = model.header.name or ''
        mid   = model.header.model_id

        #    choose fourcc                                               
        fourcc_map = {
            COLVersion.COL_1: b'COLL',
            COLVersion.COL_2: b'COL2',
            COLVersion.COL_3: b'COL3',
            COLVersion.COL_4: b'COL4',
        }
        fourcc = fourcc_map.get(ver, b'COL2')

        #    build payload                                               
        payload = bytearray()

        # Name (22 bytes, null-padded) + model_id (2 bytes)
        name_bytes = name.encode('ascii', errors='ignore')[:22]
        name_bytes = name_bytes.ljust(22, b'\x00')
        payload += name_bytes
        payload += struct.pack('<H', mid)

        # Bounds (40 bytes)
        payload += cls._write_bounds(model.bounds, ver)

        if ver == COLVersion.COL_1:
            payload += cls._write_col1_body(model)
        else:
            payload += cls._write_col23_body(model, ver)

        #    build header: fourcc(4) + size(4) + payload                 
        size = len(payload)
        header = fourcc + struct.pack('<I', size)
        return header + bytes(payload)

    @classmethod
    def _surface(cls, item) -> bytes: #vers 1
        """4-byte surface: material, flag, brightness, light."""
        import struct
        return struct.pack('<BBBB', cls._mat_id(item),
                           int(getattr(item, 'flag', 0) or 0) & 0xFF,
                           int(getattr(item, 'brightness', 0) or 0) & 0xFF,
                           int(getattr(item, 'light', 0) or 0) & 0xFF)

    @classmethod
    def _write_col1_body(cls, model) -> bytes: #vers 3
        """COL1 body after bounds; matches COLParser.parse_model COL1 order."""
        import struct
        spheres = model.spheres  or []
        boxes   = model.boxes    or []
        verts   = model.vertices or []
        faces   = model.faces    or []
        buf = bytearray(struct.pack('<I', len(spheres)))
        for s in spheres:
            buf += struct.pack('<f', float(s.radius)) + cls._v3(s.center) + cls._surface(s)
        buf += struct.pack('<I', 0)                     # unknown / lines
        buf += struct.pack('<I', len(boxes))
        for b in boxes:
            buf += cls._v3(b.min) + cls._v3(b.max) + cls._surface(b)
        buf += struct.pack('<I', len(verts))
        for v in verts:
            buf += struct.pack('<fff', float(v.x), float(v.y), float(v.z))
        buf += struct.pack('<I', len(faces))
        for f in faces:
            buf += struct.pack('<IIIBBxx', int(f.a), int(f.b), int(f.c),
                               cls._mat_id(f), int(getattr(f, 'light', 0) or 0) & 0xFF)
        return bytes(buf)

    @classmethod
    def _write_col23_body(cls, model, ver) -> bytes: #vers 3
        """COL2/3/4 body after bounds: offset header then data blocks.
        Offsets are from the fourcc, pointing 4 bytes before each block."""
        import struct
        spheres = model.spheres  or []
        boxes   = model.boxes    or []
        verts   = model.vertices or []
        faces   = model.faces    or []

        def _i16(val):
            return max(-32768, min(32767, int(round(float(val) * 128.0))))

        hdr_len = 36 + (12 if ver.value >= 3 else 0) + (4 if ver.value >= 4 else 0)
        pos = 8 + 24 + 40 + hdr_len                     # fourcc+size, name+id, bounds
        blocks = bytearray()

        def _add(data):
            nonlocal pos, blocks
            off = pos - 4
            blocks += data
            pos += len(data)
            return off

        sph = bytearray()
        for s in spheres:
            sph += cls._v3(s.center) + struct.pack('<f', float(s.radius)) + cls._surface(s)
        box = bytearray()
        for b in boxes:
            box += cls._v3(b.min) + cls._v3(b.max) + cls._surface(b)
        vtx = bytearray()
        for v in verts:
            vtx += struct.pack('<hhh', _i16(v.x), _i16(v.y), _i16(v.z))
        while len(vtx) % 4:
            vtx += b'\x00'
        fac = bytearray()
        for f in faces:
            fac += struct.pack('<HHHBB', int(f.a), int(f.b), int(f.c),
                               cls._mat_id(f), int(getattr(f, 'light', 0) or 0) & 0xFF)

        off_sph = _add(bytes(sph))
        off_box = _add(bytes(box))
        off_vtx = _add(bytes(vtx))
        off_fac = _add(bytes(fac))
        flags = 2 if (spheres or boxes or faces) else 0
        hdr = struct.pack('<HHHBxIIIIIII', len(spheres), len(boxes), len(faces), 0,
                          flags, off_sph, off_box, 0, off_vtx, off_fac, 0)
        if ver.value >= 3:
            hdr += struct.pack('<III', 0, 0, 0)            # no shadow mesh
        if ver.value >= 4:
            hdr += struct.pack('<I', 0)
        return hdr + bytes(blocks)

    @classmethod
    def write_file(cls, models: list) -> bytes:
        """Serialise a list of COLModels to a complete .col file."""
        return b''.join(cls.write_model(m) for m in models)
