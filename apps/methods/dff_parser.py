#this belongs in apps/methods/dff_parser.py - Version: 1
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
# DFFParser._parse_geometry
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
                    if ect == 0x0253F2FE:   # HAnim PLG / Frame name
                        pass
                    elif ect == 0x0253F2FF: # Frame name string
                        raw = self.data[ep2:ep2+esz]
                        name = raw.split(b'\x00')[0].decode('ascii','replace')
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

    def _parse_geometry(self, start: int, end: int) -> Optional[Geometry]:
        pos = start
        ct, sz, lib, p = read_chunk(self.data, pos)
        if ct != int(RWChunkType.STRUCT):
            return None

        geom = Geometry()
        # flags(2) + uv_layers(1) + unknown(1) + triangle_count(4) + vertex_count(4) + morph_count(4)
        flags, uv_count, unk, tri_count, vert_count, morph_count = \
            struct.unpack_from('<HBBiii', self.data, p)
        p += 16

        geom.flags = flags
        geom.uv_layer_count = uv_count if uv_count > 0 else 1

        HAS_COLORS   = bool(flags & 0x0008)
        HAS_TEXCOORD = bool(flags & 0x0004) or bool(flags & 0x0080)
        HAS_NORMALS  = bool(flags & 0x0010)

        # Prelit vertex colors
        if HAS_COLORS:
            for _ in range(vert_count):
                r,g,b,a = struct.unpack_from('<BBBB', self.data, p); p += 4
                geom.colors.append(RGBA(r,g,b,a))

        # UV layers
        layer_count = geom.uv_layer_count
        for layer_i in range(layer_count):
            uvs = []
            for _ in range(vert_count):
                u,v = struct.unpack_from('<ff', self.data, p); p += 8
                uvs.append(TexCoord(u, v))
            geom.uv_layers.append(uvs)

        # Triangles
        for _ in range(tri_count):
            v2, v1, mat_id, v3 = struct.unpack_from('<HHHH', self.data, p); p += 8
            geom.triangles.append(Triangle(v1, v2, v3, mat_id))

        # Bounding sphere + has_positions + has_normals
        cx, cy, cz, r = struct.unpack_from('<4f', self.data, p); p += 16
        geom.bounding_sphere = BoundingSphere(Vector3(cx,cy,cz), r)
        has_pos, has_nrm = struct.unpack_from('<II', self.data, p); p += 8

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

        pos = start + 12 + sz   # skip to after struct

        # Material list
        while pos < end - 12:
            ct2, sz2, lib2, p2 = read_chunk(self.data, pos)
            if ct2 == int(RWChunkType.MATERIAL_LIST):
                self._parse_material_list(p2, p2 + sz2, geom)
            pos = p2 + sz2

        return geom

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
        mflags, r,g,b,a, unk, textured = struct.unpack_from('<I4BI2I', self.data, p)[:7]
        # ambient/specular/diffuse come after but layout varies; safe read:
        try:
            ambient, specular, diffuse = struct.unpack_from('<3f', self.data, p + 20)
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
