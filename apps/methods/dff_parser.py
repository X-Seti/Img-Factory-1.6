#this belongs in apps/methods/dff_parser.py - Version: 7
# X-Seti - May09 2026 - Model Workshop - RenderWare DFF Parser
"""
Parser for GTA RenderWare DFF (Clump) model files.
Reads geometry, frames, materials, and atomic links.
Supports GTA III / VC / SA PC DFF format.

Correct RW Geometry Struct layout (SA/VC/III):
  header(16): flags(H) uv_count(B) unk(B) tri_count(i) vert_count(i) morph_count(i)
  [colors: vert_count * RGBA(4)]   if HAS_COLORS (flags & 0x08)
  [uvs:    vert_count * UV(8)]     if HAS_UV     (flags & 0x04|0x80 or uv_count>0)
  triangles: tri_count * 8 bytes   (v2:H v1:H mat:H v3:H)
  morph_target_0:
    bsphere: cx cy cz r (4 floats = 16 bytes)
    vertices: vert_count * XYZ(12)
    [normals: vert_count * XYZ(12)] if HAS_NORMALS (flags & 0x10)
  [additional morph targets...]
Triangles from BinMesh (0x050E extension) replace inline triangles when present.
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
# DFFParser._parse_geometry
# DFFParser._parse_binmesh
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

    def parse(self) -> DFFModel: #vers 1
        data = self.data
        ct, sz, lib, p = read_chunk(data, 0)
        if ct != int(RWChunkType.CLUMP):
            self.errors.append(f"Not a Clump: 0x{ct:04X}")
            return self.model
        self.model.rw_version = lib
        self._parse_clump(p, p + sz)
        return self.model

    def _parse_clump(self, start: int, end: int): #vers 1
        pos = start
        while pos < end - 12:
            ct, sz, lib, p = read_chunk(self.data, pos)
            if ct == int(RWChunkType.FRAME_LIST):
                self._parse_frame_list(p, p + sz)
            elif ct == int(RWChunkType.GEOMETRY_LIST):
                self._parse_geometry_list(p, p + sz)
            elif ct == int(RWChunkType.ATOMIC):
                self._parse_atomic(p, p + sz)
            pos = p + sz

    def _parse_frame_list(self, start: int, end: int): #vers 1
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return
        frame_count = struct.unpack_from('<I', self.data, p)[0]
        p += 4
        for _ in range(frame_count):
            frame = Frame()
            rot9 = struct.unpack_from('<9f', self.data, p);  p += 36
            pos3 = struct.unpack_from('<3f', self.data, p);  p += 12
            parent_idx, flags = struct.unpack_from('<iI', self.data, p); p += 8
            frame.rotation     = list(rot9)
            frame.position     = Vector3(*pos3)
            frame.parent_index = parent_idx
            frame.flags        = flags
            self.model.frames.append(frame)

        # Frame name extensions follow
        pos = start + 12 + sz
        for frame_idx in range(len(self.model.frames)):
            if pos >= end - 12:
                break
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.EXTENSION):
                ep = p2
                while ep < p2 + sz2 - 12:
                    ect, esz, _, ep2 = read_chunk(self.data, ep)
                    if ect in (0x0253F2FE, 0x0253F2FF, 0x00000002):
                        raw = self.data[ep2:ep2 + esz]
                        name = raw.split(b'\x00')[0].decode('ascii', 'replace').strip()
                        if name and not self.model.frames[frame_idx].name:
                            self.model.frames[frame_idx].name = name
                    ep = ep2 + esz
            pos = p2 + sz2

    def _parse_geometry_list(self, start: int, end: int): #vers 1
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return
        geom_count = struct.unpack_from('<I', self.data, p)[0]
        pos = p + sz
        for _ in range(min(geom_count, 512)):
            if pos >= end - 12:
                break
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.GEOMETRY):
                geom = self._parse_geometry(p2, p2 + sz2)
                if geom:
                    self.model.geometries.append(geom)
            pos = p2 + sz2

    def _parse_geometry(self, start: int, end: int) -> Optional[Geometry]: #vers 3
        """Parse RW Geometry chunk (SA/VC/III format).

        Struct layout:
          header(16) [colors] [uvs] triangles morph{bsphere+verts+[norms]}
        BinMesh extension triangles replace inline triangles when present.
        """
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return None

        if p + 16 > len(self.data):
            return None

        flags, uv_count, unk, tri_count, vert_count, morph_count = \
            struct.unpack_from('<HBBiii', self.data, p)
        p += 16

        geom              = Geometry()
        geom.flags        = flags
        geom.uv_layer_count = uv_count if uv_count > 0 else 1

        HAS_COLORS  = bool(flags & 0x0008)
        HAS_UV      = bool(flags & (0x0004 | 0x0080)) or uv_count > 0
        HAS_NORMALS = bool(flags & 0x0010)
        uv_layers   = uv_count if uv_count > 0 else (1 if HAS_UV else 0)

        # --- Vertex colors ---
        if HAS_COLORS and p + vert_count * 4 <= len(self.data):
            for _ in range(vert_count):
                r, g, b, a = struct.unpack_from('<BBBB', self.data, p); p += 4
                geom.colors.append(RGBA(r, g, b, a))

        # --- UV layers ---
        for _ in range(uv_layers):
            if p + vert_count * 8 > len(self.data):
                break
            uvs = []
            for _ in range(vert_count):
                u, v = struct.unpack_from('<ff', self.data, p); p += 8
                uvs.append(TexCoord(u, v))
            geom.uv_layers.append(uvs)

        # --- Inline triangles (v2, v1, mat_id, v3) ---
        inline_tris = []
        if tri_count > 0 and p + tri_count * 8 <= len(self.data):
            for _ in range(tri_count):
                v2, v1, mat_id, v3 = struct.unpack_from('<HHHH', self.data, p); p += 8
                inline_tris.append(Triangle(v1, v2, v3, mat_id))

        # --- Morph target 0: bsphere(16) + vertices + [normals] ---
        for _ in range(max(1, morph_count)):
            if p + 16 > len(self.data):
                break
            cx, cy, cz, r = struct.unpack_from('<4f', self.data, p); p += 16
            if not geom.bounding_sphere:
                geom.bounding_sphere = BoundingSphere(Vector3(cx, cy, cz), r)
            if p + vert_count * 12 > len(self.data):
                break
            for _ in range(vert_count):
                x, y, z = struct.unpack_from('<3f', self.data, p); p += 12
                geom.vertices.append(Vector3(x, y, z))
            if HAS_NORMALS and p + vert_count * 12 <= len(self.data):
                for _ in range(vert_count):
                    nx, ny, nz = struct.unpack_from('<3f', self.data, p); p += 12
                    geom.normals.append(Vector3(nx, ny, nz))

        struct_end = start + 12 + sz

        # --- Post-struct: MATERIAL_LIST and EXTENSION ---
        binmesh_tris = []
        pos = struct_end
        while pos < end - 12:
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.MATERIAL_LIST):
                self._parse_material_list(p2, p2 + sz2, geom)
            elif ct2 == 0x00000003:   # EXTENSION
                binmesh_tris = self._parse_binmesh(p2, p2 + sz2)
            pos = p2 + sz2

        geom.triangles = binmesh_tris if binmesh_tris else inline_tris
        return geom

    def _parse_binmesh(self, start: int, end: int) -> list: #vers 1
        """Parse BinMesh plugin (0x050E). Returns Triangle list."""
        pos = start
        while pos + 12 <= end:
            ct, sz, lib, dp = read_chunk(self.data, pos)
            if ct == 0x0000050E:
                if dp + 12 > len(self.data):
                    break
                face_type, mesh_count, total_idx = struct.unpack_from('<III', self.data, dp)
                bp = dp + 12
                indices = []
                for _ in range(mesh_count):
                    if bp + 8 > len(self.data):
                        break
                    idx_count, mat_idx = struct.unpack_from('<II', self.data, bp); bp += 8
                    for j in range(idx_count):
                        if bp + j * 4 + 4 > len(self.data):
                            break
                        indices.append((struct.unpack_from('<I', self.data, bp + j * 4)[0], mat_idx))
                    bp += idx_count * 4
                tris = []
                if face_type == 0:  # triangle list
                    i = 0
                    while i + 2 < len(indices):
                        v0, m = indices[i];  v1, _ = indices[i+1];  v2, _ = indices[i+2]
                        tris.append(Triangle(v0, v1, v2, m));  i += 3
                else:               # triangle strip
                    for i in range(len(indices) - 2):
                        v0, m = indices[i];  v1, _ = indices[i+1];  v2, _ = indices[i+2]
                        if v0 != v1 and v1 != v2 and v0 != v2:
                            if i % 2 == 0:
                                tris.append(Triangle(v0, v1, v2, m))
                            else:
                                tris.append(Triangle(v0, v2, v1, m))
                return tris
            pos = dp + sz
        return []

    def _parse_material_list(self, start: int, end: int, geom: Geometry): #vers 1
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return
        pos = p + sz
        while pos < end - 12:
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.MATERIAL):
                geom.materials.append(self._parse_material(p2, p2 + sz2))
            pos = p2 + sz2

    def _parse_material(self, start: int, end: int) -> Material: #vers 1
        mat = Material()
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return mat
        try:
            mflags, r, g, b, a, unk, textured = struct.unpack_from('<I4BII', self.data, p)
            ambient, specular, diffuse = struct.unpack_from('<3f', self.data, p + 16)
        except Exception:
            return mat
        mat.flags    = mflags
        mat.colour   = RGBA(r, g, b, a)
        mat.color    = mat.colour
        mat.ambient  = ambient
        mat.specular = specular
        mat.diffuse  = diffuse
        pos = p + sz
        while pos < end - 12:
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.TEXTURE):
                tp = p2
                ct3, sz3, _, p3 = read_chunk(self.data, tp); tp = p3 + sz3  # tex struct
                ct3, sz3, _, p3 = read_chunk(self.data, tp)                  # name string
                mat.texture_name = self.data[p3:p3+sz3].split(b'\x00')[0].decode('ascii','replace')
                tp = p3 + sz3
                if tp + 12 <= end:
                    ct3, sz3, _, p3 = read_chunk(self.data, tp)              # mask string
                    mat.texture_mask = self.data[p3:p3+sz3].split(b'\x00')[0].decode('ascii','replace')
            pos = p2 + sz2
        return mat

    def _parse_atomic(self, start: int, end: int): #vers 1
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return
        frame_idx, geom_idx, flags, unk = struct.unpack_from('<4I', self.data, p)
        self.model.atomics.append(Atomic(frame_index=frame_idx, geometry_index=geom_idx, flags=flags))


def detect_dff(data: bytes) -> bool: #vers 1
    if len(data) < 12:
        return False
    return struct.unpack_from('<I', data)[0] == int(RWChunkType.CLUMP)


def load_dff(path: str) -> Optional[DFFModel]: #vers 1
    try:
        with open(path, 'rb') as f:
            data = f.read()
        if not detect_dff(data):
            return None
        return DFFParser(data, path).parse()
    except Exception as e:
        print(f"[DFFParser] Failed to load {path}: {e}")
        return None


__all__ = ['DFFParser', 'detect_dff', 'load_dff', 'read_chunk']
