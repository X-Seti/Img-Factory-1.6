#this belongs in apps/methods/dff_parser.py - Version: 5
# X-Seti - Apr 2026 - Model Workshop - RenderWare DFF Parser
"""
Parser for GTA RenderWare DFF (Clump) model files.
Reads geometry, frames, materials, and atomic links.
Supports GTA III / VC / SA PC DFF format.
"""

import struct
from typing import Optional, List
from apps.methods.dff_classes import (
    DFFModel, Frame, Geometry, Atomic, Material, Triangle,
    Vector3, RGBA, TexCoord, BoundingSphere, RWChunkType
)

## Methods list -
# read_chunk
# DFFParser.parse
# DFFParser._parse_clump
# DFFParser._parse_frame_list
# DFFParser._parse_geometry_list
# DFFParser._parse_geometry #vers 2
# DFFParser._parse_binmesh #vers 1
# DFFParser._parse_geometry_v33
# DFFParser._parse_extension_v33
# DFFParser._parse_material_list
# DFFParser._parse_material
# DFFParser._parse_atomic
# detect_dff
# load_dff


def read_chunk(data: bytes, pos: int):
    """Read a 12-byte RW chunk header → (type, size, lib, payload_pos)."""
    if pos + 12 > len(data):
        return None, 0, 0, pos
    ct, sz, lib = struct.unpack_from('<III', data, pos)
    return ct, sz, lib, pos + 12


class DFFParser:
    """Parses a GTA RenderWare DFF (Clump) file into a DFFModel."""

    def __init__(self, data: bytes, path: str = ""):
        self.data  = data
        self.path  = path
        self.model = DFFModel(source_path=path)
        self.errors: List[str] = []

    def _read(self, pos: int, fmt: str):
        """Unpack struct at pos, return (values..., next_pos)."""
        size = struct.calcsize(fmt)
        vals = struct.unpack_from(fmt, self.data, pos)
        return vals, pos + size

    def parse(self) -> DFFModel:
        """Parse the DFF file. Returns the DFFModel."""
        data = self.data
        pos  = 0
        ct, sz, lib, p = read_chunk(data, pos)
        if ct != int(RWChunkType.CLUMP):
            self.errors.append(f"Not a Clump: chunk type 0x{ct:04X}")
            return self.model
        self.model.rw_version = lib
        self._parse_clump(p, p + sz)
        return self.model

    def _parse_clump(self, start: int, end: int):
        pos = start
        geom_list_seen = False
        while pos < end - 12:
            ct, sz, lib, p = read_chunk(self.data, pos)
            payload_end = p + sz

            if ct == int(RWChunkType.STRUCT):
                # Clump struct: atomic_count(4) + light_count(4) + camera_count(4) [SA only?]
                pass
            elif ct == int(RWChunkType.FRAME_LIST):
                self._parse_frame_list(p, payload_end)
            elif ct == int(RWChunkType.GEOMETRY_LIST):
                self._parse_geometry_list(p, payload_end)
                geom_list_seen = True
            elif ct == int(RWChunkType.ATOMIC):
                self._parse_atomic(p, payload_end)
            elif ct == int(RWChunkType.EXTENSION):
                pass  # extensions at clump level

            pos = payload_end

    def _parse_frame_list(self, start: int, end: int):
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return
        # Struct: frame_count(4)
        frame_count = struct.unpack_from('<I', self.data, p)[0]
        p += 4
        for _ in range(frame_count):
            frame = Frame()
            # rotation matrix (9 floats) + position (3 floats) + parent_idx (i) + flags (I)
            rot9 = struct.unpack_from('<9f', self.data, p); p += 36
            pos3 = struct.unpack_from('<3f', self.data, p); p += 12
            parent_idx, flags = struct.unpack_from('<iI', self.data, p); p += 8
            frame.rotation    = list(rot9)
            frame.position    = Vector3(*pos3)
            frame.parent_index = parent_idx
            frame.flags       = flags
            self.model.frames.append(frame)

        # Frame extensions (names are in Frame Name extension)
        pos = start + 12 + sz
        frame_idx = 0
        while pos < end - 12 and frame_idx < len(self.model.frames):
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.EXTENSION):
                # Walk inside extension for Frame Name (0x253F2FE)
                ep = p2; ee = p2 + sz2
                while ep < ee - 12:
                    ect, esz, _, ep2 = read_chunk(self.data, ep)
                    if ect == 0x0253F2FE:   # Frame Name plugin (VC) — data IS the name string
                        raw = self.data[ep2:ep2+esz]
                        name = raw.split(b'\x00')[0].decode('ascii','replace').strip()
                        if name:
                            self.model.frames[frame_idx].name = name
                    elif ect == 0x0253F2FF: # Frame name string (RW 3.4+)
                        raw = self.data[ep2:ep2+esz]
                        name = raw.split(b'\x00')[0].decode('ascii','replace').strip()
                        if name:
                            self.model.frames[frame_idx].name = name
                    elif ect == 0x00000002: # RW_STRING — frame name in VC format
                        raw = self.data[ep2:ep2+esz]
                        name = raw.split(b'\x00')[0].decode('ascii','replace').strip()
                        if name and not self.model.frames[frame_idx].name:
                            self.model.frames[frame_idx].name = name
                    ep = ep2 + esz
            frame_idx += 1
            pos = p2 + sz2

    def _parse_geometry_list(self, start: int, end: int):
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return
        geom_count = struct.unpack_from('<I', self.data, p)[0]
        pos = p + sz

        for _ in range(min(geom_count, 256)):
            if pos >= end - 12:
                break
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.GEOMETRY):
                geom = self._parse_geometry(p2, p2 + sz2)
                if geom:
                    self.model.geometries.append(geom)
            pos = p2 + sz2

    def _parse_geometry(self, start: int, end: int) -> Optional[Geometry]: #vers 2
        """Parse a RenderWare Geometry chunk.

        RW 3.3 (VC, 0x0C02FFFF) morph-target layout inside the struct:
          header(16) + [colors] + [UVs] + morph_target_0 {
              bsphere(16) + has_pos(4) + has_nrm(4) +
              triangles(tri_count*8) + vertices(vert_count*12) + [normals(vert_count*12)]
          }
        Triangle indices from BinMesh extension (0x050E) always take precedence
        over inline triangles when present.
        """
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            if (ct >> 16) == 0x0001 and (ct & 0xFFFF) != 0:
                return self._parse_geometry_v33(ct, p, p + sz, end)
            return None

        geom = Geometry()
        flags, uv_count, unk, tri_count, vert_count, morph_count = \
            struct.unpack_from('<HBBiii', self.data, p)
        p += 16

        geom.flags = flags
        geom.uv_layer_count = uv_count if uv_count > 0 else 1

        HAS_COLORS   = bool(flags & 0x0008)
        HAS_TEXCOORD = bool(flags & (0x0004 | 0x0080)) or uv_count > 0
        HAS_NORMALS  = bool(flags & 0x0010)

        # Prelit vertex colors
        if HAS_COLORS:
            for _ in range(vert_count):
                r,g,b,a = struct.unpack_from('<BBBB', self.data, p); p += 4
                geom.colors.append(RGBA(r,g,b,a))

        # UV layers
        layer_count = uv_count if uv_count > 0 else (1 if HAS_TEXCOORD else 0)
        for layer_i in range(layer_count):
            uvs = []
            for _ in range(vert_count):
                u,v = struct.unpack_from('<ff', self.data, p); p += 8
                uvs.append(TexCoord(u, v))
            geom.uv_layers.append(uvs)

        # Morph target header: bsphere(16) + has_pos(4) + has_nrm(4)
        # This header always precedes triangle + vertex data in the morph target.
        cx, cy, cz, r = struct.unpack_from('<4f', self.data, p); p += 16
        geom.bounding_sphere = BoundingSphere(Vector3(cx,cy,cz), r)
        has_pos, has_nrm = struct.unpack_from('<II', self.data, p); p += 8

        # Inline triangles (inside morph target, after bsphere+flags)
        inline_tris = []
        if tri_count > 0:
            struct_bytes_left = (start + 12 + sz) - p
            if struct_bytes_left >= tri_count * 8:
                for _ in range(tri_count):
                    v2, v1, mat_id, v3 = struct.unpack_from('<HHHH', self.data, p); p += 8
                    inline_tris.append(Triangle(v1, v2, v3, mat_id))

        # Force has_pos if struct contains enough bytes for vertices
        struct_bytes_left = (start + 12 + sz) - p
        if not has_pos and vert_count > 0 and struct_bytes_left >= vert_count * 12:
            has_pos = 1

        # Vertices
        if has_pos:
            for _ in range(vert_count):
                x,y,z = struct.unpack_from('<3f', self.data, p); p += 12
                geom.vertices.append(Vector3(x,y,z))

        # Normals
        if has_nrm:
            for _ in range(vert_count):
                x,y,z = struct.unpack_from('<3f', self.data, p); p += 12
                geom.normals.append(Vector3(x,y,z))

        pos = start + 12 + sz   # advance past struct chunk

        # Post-struct chunks: MATERIAL_LIST and EXTENSION (contains BinMesh)
        binmesh_tris = []
        while pos < end - 12:
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.MATERIAL_LIST):
                self._parse_material_list(p2, p2 + sz2, geom)
            elif ct2 == 0x00000003:  # EXTENSION
                binmesh_tris = self._parse_binmesh(p2, p2 + sz2)
            pos = p2 + sz2

        # BinMesh takes precedence over inline triangles when present
        geom.triangles = binmesh_tris if binmesh_tris else inline_tris

        return geom

    def _parse_binmesh(self, start: int, end: int) -> list: #vers 1
        """Parse BinMesh plugin (0x050E) from an extension chunk.
        Returns list of Triangle or empty list if not found."""
        pos = start
        while pos + 12 <= end:
            ct, sz, lib, dp = read_chunk(self.data, pos)
            if ct == 0x0000050E:
                face_type, mesh_count, total_idx = struct.unpack_from('<III', self.data, dp)
                bp = dp + 12
                indices = []
                for _ in range(mesh_count):
                    idx_count, mat_idx = struct.unpack_from('<II', self.data, bp); bp += 8
                    for j in range(idx_count):
                        indices.append((struct.unpack_from('<I', self.data, bp + j * 4)[0], mat_idx))
                    bp += idx_count * 4
                tris = []
                if face_type == 0:  # triangle list
                    i = 0
                    while i + 2 < len(indices):
                        v0, m = indices[i]
                        v1, _ = indices[i+1]
                        v2, _ = indices[i+2]
                        tris.append(Triangle(v0, v1, v2, m))
                        i += 3
                else:  # triangle strip
                    for i in range(len(indices) - 2):
                        v0, m = indices[i]
                        v1, _ = indices[i+1]
                        v2, _ = indices[i+2]
                        if v0 != v1 and v1 != v2 and v0 != v2:
                            if i % 2 == 0:
                                tris.append(Triangle(v0, v1, v2, m))
                            else:
                                tris.append(Triangle(v0, v2, v1, m))
                return tris
            pos = dp + sz
        return []

    def _parse_geometry_v33(self, chunk_type: int, struct_start: int,
                            struct_end: int, geom_end: int) -> 'Optional[Geometry]': #vers 1
        """Parse RW 3.3 / older VC geometry where chunk type encodes flags.
        Vertex data lives after the struct chunk in the raw geometry body.
        Triangle indices come from the BinMesh extension (0x050e)."""
        geom = Geometry()
        flags = chunk_type & 0xFFFF
        geom.flags = flags

        HAS_COLORS   = bool(flags & 0x0008)
        HAS_TEXCOORD = bool(flags & (0x0004 | 0x0080))
        HAS_NORMALS  = bool(flags & 0x0010)

        # The struct chunk contains: numMorphTargets(4) + bsphere(16)
        # then optionally: prelit colors, UV layers
        p = struct_start
        n_morph = struct.unpack_from('<I', self.data, p)[0]
        p += 4
        # bsphere: cx cy cz r
        p += 16  # skip bounding sphere

        # Count UVs in struct: (struct_end - p) / (8 * uv_layer_count)
        uv_count = 1 if HAS_TEXCOORD else 0
        struct_remaining = struct_end - p

        # After the struct chunk body, vertex data is raw in the geometry chunk body
        # Layout: struct_chunk | raw_vert_data | MATERIAL_LIST | EXTENSION
        # raw_vert_data starts at struct_end and ends at MATERIAL_LIST

        # Find MATERIAL_LIST and EXTENSION positions
        mat_list_pos = None
        ext_pos      = None
        pos = struct_end
        while pos + 12 <= geom_end:
            ct, cs, cv, dp = read_chunk(self.data, pos)
            if ct == int(RWChunkType.MATERIAL_LIST):
                mat_list_pos = pos
                self._parse_material_list(dp, dp + cs, geom)
            elif ct == 0x00000003:  # EXTENSION
                ext_pos = pos
                self._parse_extension_v33(dp, dp + cs, geom)
            pos = dp + cs

        # Vertex data is between struct_end and first known chunk after struct
        vert_data_start = struct_end
        vert_data_end   = mat_list_pos if mat_list_pos else geom_end

        raw_size = vert_data_end - vert_data_start
        if raw_size > 0:
            # Determine vert_count: raw_size / bytes_per_vert
            # Each vert: XYZ(12) + normals(12 if HAS_NORMALS)
            bpv = 12 + (12 if HAS_NORMALS else 0)
            vert_count = raw_size // bpv

            vp = vert_data_start
            for i in range(vert_count):
                x, y, z = struct.unpack_from('<3f', self.data, vp)
                geom.vertices.append(Vector3(x, y, z))
                vp += 12
                if HAS_NORMALS:
                    nx, ny, nz = struct.unpack_from('<3f', self.data, vp)
                    geom.normals.append(Vector3(nx, ny, nz))
                    vp += 12

        # UV data is in the struct chunk body
        vert_count = len(geom.vertices)
        if HAS_TEXCOORD and vert_count:
            uv_bytes = vert_count * 8  # u(4) + v(4) per vert
            if struct_remaining >= uv_bytes:
                uvs = []
                for i in range(vert_count):
                    u, v = struct.unpack_from('<2f', self.data, p + i * 8)
                    uvs.append(TexCoord(u, v))
                geom.uv_layers.append(uvs)

        # Triangles come from BinMesh (already populated by _parse_extension_v33)
        return geom

    def _parse_extension_v33(self, start: int, end: int, geom: Geometry): #vers 1
        """Parse extension chunk for RW 3.3 geometry — handles BinMesh (0x050e)."""
        pos = start
        while pos + 12 <= end:
            ct, cs, cv, dp = read_chunk(self.data, pos)
            if ct == 0x0000050e:  # BinMesh plugin
                face_type, mesh_count, total_idx = struct.unpack_from('<III', self.data, dp)
                bp = dp + 12
                indices = []
                for _ in range(mesh_count):
                    idx_count, mat_idx = struct.unpack_from('<II', self.data, bp)
                    bp += 8
                    for j in range(idx_count):
                        indices.append((struct.unpack_from('<I', self.data, bp + j * 4)[0], mat_idx))
                    bp += idx_count * 4
                # Build triangles from index list
                if face_type == 0:  # triangle list
                    i = 0
                    while i + 2 < len(indices):
                        v0, m = indices[i]
                        v1, _ = indices[i+1]
                        v2, _ = indices[i+2]
                        geom.triangles.append(Triangle(v0, v1, v2, m))
                        i += 3
                else:  # triangle strip
                    for i in range(len(indices) - 2):
                        v0, m = indices[i]
                        v1, _ = indices[i+1]
                        v2, _ = indices[i+2]
                        if v0 != v1 and v1 != v2 and v0 != v2:
                            if i % 2 == 0:
                                geom.triangles.append(Triangle(v0, v1, v2, m))
                            else:
                                geom.triangles.append(Triangle(v0, v2, v1, m))
            pos = dp + cs

    def _parse_material_list(self, start: int, end: int, geom: Geometry):
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return
        mat_count = struct.unpack_from('<I', self.data, p)[0]
        # material indices follow (mat_count × int32, -1 = new, else reference)
        pos = p + sz

        mats_added = 0
        while pos < end - 12:
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.MATERIAL):
                mat = self._parse_material(p2, p2 + sz2)
                geom.materials.append(mat)
                mats_added += 1
            pos = p2 + sz2

    def _parse_material(self, start: int, end: int) -> Material:
        mat = Material()
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return mat

        # flags(4) + color(4) + unk(4) + textured(4) + ambient(f) + specular(f) + diffuse(f)
        # RW Material Struct: flags(4)+RGBA(4)+unused(4)+textured(4)+ambient(f)+specular(f)+diffuse(f)
        mflags, r,g,b,a, unk, textured = struct.unpack_from('<I4BII', self.data, p)[:7]
        try:
            ambient, specular, diffuse = struct.unpack_from('<3f', self.data, p + 16)
        except Exception:
            ambient = specular = diffuse = 1.0
        mat.flags    = mflags
        mat.color    = RGBA(r,g,b,a)
        mat.ambient  = ambient
        mat.specular = specular
        mat.diffuse  = diffuse

        pos = p + sz

        # Texture chunk
        while pos < end - 12:
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.TEXTURE):
                tp = p2
                # Texture struct (flags)
                ct3, sz3, _, p3 = read_chunk(self.data, tp); tp = p3 + sz3
                # Texture name string
                ct3, sz3, _, p3 = read_chunk(self.data, tp)
                mat.texture_name = self.data[p3:p3+sz3].split(b'\x00')[0].decode('ascii','replace')
                tp = p3 + sz3
                # Mask name string
                ct3, sz3, _, p3 = read_chunk(self.data, tp)
                mat.texture_mask = self.data[p3:p3+sz3].split(b'\x00')[0].decode('ascii','replace')
            pos = p2 + sz2

        return mat

    def _parse_atomic(self, start: int, end: int):
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return
        frame_idx, geom_idx, flags, unk = struct.unpack_from('<4I', self.data, p)
        atomic = Atomic(frame_index=frame_idx, geometry_index=geom_idx, flags=flags)
        self.model.atomics.append(atomic)


def detect_dff(data: bytes) -> bool:
    """Return True if data starts with a RenderWare Clump (DFF)."""
    if len(data) < 12:
        return False
    ct = struct.unpack_from('<I', data)[0]
    return ct == int(RWChunkType.CLUMP)


def load_dff(path: str) -> Optional[DFFModel]:
    """Load a DFF file from disk. Returns DFFModel or None on failure."""
    try:
        with open(path, 'rb') as f:
            data = f.read()
        if not detect_dff(data):
            return None
        parser = DFFParser(data, path)
        return parser.parse()
    except Exception as e:
        print(f"[DFFParser] Failed to load {path}: {e}")
        return None


__all__ = ['DFFParser', 'detect_dff', 'load_dff', 'read_chunk']
