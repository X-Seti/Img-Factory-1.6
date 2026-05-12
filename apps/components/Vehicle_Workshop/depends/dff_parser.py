#this belongs in apps/methods/dff_parser.py - Version: 8
# X-Seti - May09 2026 - Model Workshop - RenderWare DFF Parser
"""
Parser for GTA RenderWare DFF (Clump) model files.
Cross-referenced against DragonFF (gtaLib/dff.py) for correctness.

RW Geometry Struct layout:
  flags(I=4) tri_count(I=4) vert_count(I=4) [pad=4] = 16 bytes header
  [surface_props: ambient+specular+diffuse (3f=12 bytes) if rw_version < 0x34000]
  [prelit colors: vert_count * RGBA(4)]   if flags & 0x08 (rpGEOMETRYPRELIT)
  [UV layers: vert_count * UV(8) * layer_count]
  triangles: tri_count * 8 (v1:H v2:H mat:H v3:H — note DragonFF order)
  morph_target:
    bsphere: cx cy cz r (4 floats = 16 bytes)
    has_vertices (I=4)
    has_normals  (I=4)
    [vertices: vert_count * XYZ(12)]  if has_vertices
    [normals:  vert_count * XYZ(12)]  if has_normals

RW version decode: if lib & 0xFFFF0000: version = (lib>>14 & 0x3FF00)+0x30000|(lib>>16 & 0x3F)
                   else:                version = lib << 8
Old format (version < 0x34000): includes surface properties after header.
"""

import struct
from typing import Optional, List
from apps.methods.dff_classes import (
    DFFModel, Frame, Geometry, Atomic, Material, Triangle,
    Vector3, RGBA, TexCoord, BoundingSphere, RWChunkType
)

## Methods list -
# read_chunk
# _rw_version
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


def _rw_version(library_id: int) -> int:
    """Decode RW library_id to comparable version number (DragonFF algorithm)."""
    if library_id & 0xFFFF0000:
        return (library_id >> 14 & 0x3FF00) + 0x30000 | (library_id >> 16 & 0x3F)
    return library_id << 8


class DFFParser:
    """Parses a GTA RenderWare DFF (Clump) file into a DFFModel."""

    def __init__(self, data: bytes, path: str = ""):
        self.data    = data
        self.path    = path
        self.model   = DFFModel(source_path=path)
        self.rw_ver  = 0
        self.errors: List[str] = []

    def parse(self) -> DFFModel: #vers 1
        ct, sz, lib, p = read_chunk(self.data, 0)
        if ct != int(RWChunkType.CLUMP):
            self.errors.append(f"Not a Clump: 0x{ct:08X}")
            return self.model
        self.model.rw_version = lib
        self.rw_ver = _rw_version(lib)
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
            rot9       = struct.unpack_from('<9f', self.data, p); p += 36
            pos3       = struct.unpack_from('<3f', self.data, p); p += 12
            parent_idx, flags = struct.unpack_from('<iI', self.data, p); p += 8
            frame.rotation     = list(rot9)
            frame.position     = Vector3(*pos3)
            frame.parent_index = parent_idx
            frame.flags        = flags
            self.model.frames.append(frame)

        # Frame name extensions
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
                        raw  = self.data[ep2:ep2 + esz]
                        name = raw.split(b'\x00')[0].decode('ascii', 'ignore').strip()
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

    def _parse_geometry(self, start: int, end: int) -> Optional[Geometry]: #vers 4
        """Parse RW Geometry chunk.

        Header layout (DragonFF-verified):
          flags(I=4) tri_count(I=4) vert_count(I=4) pad(4) = 16 bytes
          [surface_props: 3f=12 bytes] if rw_version < 0x34000
          [prelit: vert*4]  [uvs: vert*8*layers]  triangles(tri*8)
          morph: bsphere(16) + has_verts(4) + has_norms(4) + [verts] + [norms]
        """
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return None
        if p + 16 > len(self.data):
            return None

        # Header: flags as full 32-bit (upper 16 bits encode UV layer count)
        flags      = struct.unpack_from('<I', self.data, p)[0]
        tri_count  = struct.unpack_from('<I', self.data, p + 4)[0]
        vert_count = struct.unpack_from('<I', self.data, p + 8)[0]
        p += 16   # skip flags(4)+tri_count(4)+vert_count(4)+pad(4)

        geom            = Geometry()
        geom.flags      = flags
        geom.uv_layer_count = 1

        # Surface properties — old format only (rw_version < 0x34000)
        if self.rw_ver < 0x34000:
            if p + 12 <= len(self.data):
                ambient, specular, diffuse = struct.unpack_from('<3f', self.data, p)
                p += 12

        # Geometry flags
        HAS_PRELIT  = bool(flags & 0x0008)   # rpGEOMETRYPRELIT
        HAS_UV      = bool(flags & 0x0004)   # rpGEOMETRYTEXTURED
        HAS_UV2     = bool(flags & 0x0080)   # rpGEOMETRYTEXTURED2
        HAS_NORMALS = bool(flags & 0x0010)   # rpGEOMETRYNORMALS
        IS_NATIVE   = bool(flags & 0x01000000)

        # UV layer count: bits 16-23 of flags, else 1 or 2 from flags
        uv_layers = (flags >> 16) & 0xFF
        if uv_layers == 0:
            uv_layers = 2 if HAS_UV2 else (1 if HAS_UV else 0)
        geom.uv_layer_count = uv_layers

        if IS_NATIVE:
            # Native geometry — skip vertex data (handled by platform-specific parsers)
            pass
        else:
            # --- Prelit vertex colors ---
            if HAS_PRELIT and p + vert_count * 4 <= len(self.data):
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

            # --- Triangles: v2(H) v1(H) mat(H) v3(H) ---
            inline_tris = []
            if tri_count > 0 and p + tri_count * 8 <= len(self.data):
                for _ in range(tri_count):
                    v2, v1, mat_id, v3 = struct.unpack_from('<HHHH', self.data, p); p += 8
                    inline_tris.append(Triangle(v1, v2, v3, mat_id))

            # --- Morph target: bsphere(16) + has_vertices(4) + has_normals(4) ---
            if p + 24 <= len(self.data):
                cx, cy, cz, r = struct.unpack_from('<4f', self.data, p); p += 16
                geom.bounding_sphere = BoundingSphere(Vector3(cx, cy, cz), r)
                has_verts = struct.unpack_from('<I', self.data, p)[0]; p += 4
                has_norms = struct.unpack_from('<I', self.data, p)[0]; p += 4

                if has_verts and p + vert_count * 12 <= len(self.data):
                    for _ in range(vert_count):
                        x, y, z = struct.unpack_from('<3f', self.data, p); p += 12
                        geom.vertices.append(Vector3(x, y, z))

                if has_norms and p + vert_count * 12 <= len(self.data):
                    for _ in range(vert_count):
                        nx, ny, nz = struct.unpack_from('<3f', self.data, p); p += 12
                        geom.normals.append(Vector3(nx, ny, nz))

        struct_end = start + 12 + sz

        # --- Post-struct chunks: MATERIAL_LIST and EXTENSION (BinMesh) ---
        binmesh_tris = []
        pos = struct_end
        while pos < end - 12:
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.MATERIAL_LIST):
                self._parse_material_list(p2, p2 + sz2, geom)
            elif ct2 == 0x00000003:   # EXTENSION
                binmesh_tris = self._parse_binmesh(p2, p2 + sz2)
            pos = p2 + sz2

        geom.triangles = binmesh_tris if binmesh_tris else (inline_tris if 'inline_tris' in dir() else [])
        return geom

    def _parse_binmesh(self, start: int, end: int) -> list: #vers 2
        """Parse BinMesh plugin (0x050E). Returns Triangle list.
        Algorithm matches DragonFF exactly (see gtaLib/dff.py read_matplit_plg).
        Strip: sliding window of 2 previous verts, j%2 controls winding, no restart detection.
        List:  read 3 indices at a time, winding = (v2, v1, mat, v3) -> Triangle(v1,v2,v3).
        """
        pos = start
        while pos + 12 <= end:
            ct, sz, lib, dp = read_chunk(self.data, pos)
            if ct == 0x0000050E:
                if dp + 12 > len(self.data):
                    break
                face_type, mesh_count, total_idx = struct.unpack_from('<III', self.data, dp)
                is_strip = (face_type == 1)
                bp = dp + 12
                tris = []

                for mi in range(mesh_count):
                    if bp + 8 > len(self.data):
                        break
                    idx_count, mat_idx = struct.unpack_from('<II', self.data, bp); bp += 8

                    if is_strip:
                        # DragonFF algorithm: sliding window, global j counter
                        prev = []
                        for j in range(idx_count):
                            if bp + 4 > len(self.data): break
                            vertex = struct.unpack_from('<I', self.data, bp)[0]; bp += 4
                            if len(prev) < 2:
                                prev.append(vertex)
                                continue
                            # Winding matches DragonFF:
                            # even j: Triangle(a=prev[0], b=prev[1], c=vertex)
                            # odd  j: Triangle(a=prev[1], b=prev[0], c=vertex)
                            if j % 2 == 0:
                                tris.append(Triangle(prev[0], prev[1], vertex, mat_idx))
                            else:
                                tris.append(Triangle(prev[1], prev[0], vertex, mat_idx))
                            prev[0] = prev[1]
                            prev[1] = vertex
                    else:
                        # Triangle list: ic indices as 4-byte ints, grouped by 3
                        raw = []
                        for j in range(idx_count):
                            if bp + 4 > len(self.data): break
                            raw.append(struct.unpack_from('<I', self.data, bp)[0]); bp += 4
                        i = 0
                        while i + 2 < len(raw):
                            v0, v1, v2 = raw[i], raw[i+1], raw[i+2]
                            tris.append(Triangle(v0, v1, v2, mat_idx)); i += 3

                # Filter degenerate (restart marker) triangles
                tris = [t for t in tris if t.v1!=t.v2 and t.v2!=t.v3 and t.v1!=t.v3]
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
                ct3, sz3, _, p3 = read_chunk(self.data, tp); tp = p3 + sz3
                ct3, sz3, _, p3 = read_chunk(self.data, tp)
                mat.texture_name = self.data[p3:p3+sz3].split(b'\x00')[0].decode('ascii', 'ignore').strip()
                tp = p3 + sz3
                if tp + 12 <= end:
                    ct3, sz3, _, p3 = read_chunk(self.data, tp)
                    mat.texture_mask = self.data[p3:p3+sz3].split(b'\x00')[0].decode('ascii', 'ignore').strip()
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
