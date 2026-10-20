#this belongs in apps/methods/dff_classes.py - Version: 1
# X-Seti - Apr 2026 - Model Workshop - DFF/RenderWare Data Classes
"""
Data classes for GTA RenderWare DFF model files.
Covers GTA III / VC / SA geometry structures.
"""

from dataclasses import dataclass, field
from typing import List, Optional, Tuple
from enum import IntEnum


class RWChunkType(IntEnum):
    STRUCT             = 0x0001
    STRING             = 0x0002
    EXTENSION          = 0x0003
    TEXTURE            = 0x0006
    MATERIAL           = 0x0007
    MATERIAL_LIST      = 0x0008
    FRAME_LIST         = 0x000E
    GEOMETRY           = 0x000F
    CLUMP              = 0x0010
    ATOMIC             = 0x0014
    GEOMETRY_LIST      = 0x001A
    MATERIAL_EFFECTS   = 0x0120
    SKIN_PLG           = 0x0116
    BONE_PLG           = 0x011E
    RIGHT_TO_RENDER    = 0x001F
    ANISOTROPY         = 0x0253F2FE


class RWVersion(IntEnum):
    """Library stamp → game."""
    GTAIII_VC  = 0x00000310   # GTA III / VC
    GTAVC_FULL = 0x0C02FFFF   # GTA VC
    GTASA      = 0x1803FFFF   # GTA SA


@dataclass
class Vector3:
    x: float = 0.0
    y: float = 0.0
    z: float = 0.0

    def __repr__(self):
        return f"({self.x:.3f}, {self.y:.3f}, {self.z:.3f})"


@dataclass
class RGBA:
    r: int = 255
    g: int = 255
    b: int = 255
    a: int = 255


@dataclass
class TexCoord:
    u: float = 0.0
    v: float = 0.0


@dataclass
class Triangle:
    """A face/triangle referencing 3 vertex indices and a material ID."""
    v1: int = 0
    v2: int = 0
    v3: int = 0
    material_id: int = 0


@dataclass
class BoundingSphere:
    center: Vector3 = field(default_factory=Vector3)
    radius: float = 0.0


@dataclass
class Frame:
    """RenderWare frame (bone/attachment point)."""
    rotation: List[float] = field(default_factory=lambda: [
        1,0,0, 0,1,0, 0,0,1
    ])  # 3×3 rotation matrix (row-major)
    position: Vector3 = field(default_factory=Vector3)
    parent_index: int = -1   # -1 = root
    flags: int = 0
    name: str = ""


@dataclass
class Material:
    """RenderWare material."""
    color: RGBA = field(default_factory=RGBA)
    texture_name: str = ""
    texture_mask: str = ""
    ambient:  float = 1.0
    diffuse:  float = 1.0
    specular: float = 0.0
    flags: int = 0


@dataclass
class Geometry:
    """RenderWare geometry (mesh)."""
    flags: int = 0
    uv_layer_count: int = 1

    # Vertex data
    vertices:       List[Vector3]    = field(default_factory=list)
    normals:        List[Vector3]    = field(default_factory=list)
    colors:         List[RGBA]       = field(default_factory=list)
    uv_layers:      List[List[TexCoord]] = field(default_factory=list)
    triangles:      List[Triangle]   = field(default_factory=list)

    # Bounding info
    bounding_sphere: BoundingSphere  = field(default_factory=BoundingSphere)

    # Materials
    materials: List[Material] = field(default_factory=list)

    # Extensions
    skin_present: bool = False
    morph_present: bool = False

    @property
    def vertex_count(self) -> int:
        return len(self.vertices)

    @property
    def triangle_count(self) -> int:
        return len(self.triangles)

    @property
    def material_count(self) -> int:
        return len(self.materials)


@dataclass
class Atomic:
    """RenderWare atomic: links a geometry to a frame."""
    frame_index:    int = 0
    geometry_index: int = 0
    flags:          int = 5   # rpATOMICCOLLISIONTEST | rpATOMICRENDER


@dataclass
class DFFModel:
    """
    A parsed RenderWare Clump (DFF file).
    Contains frames (skeleton), geometries (meshes), and atomics (links).
    """
    frames:     List[Frame]    = field(default_factory=list)
    geometries: List[Geometry] = field(default_factory=list)
    atomics:    List[Atomic]   = field(default_factory=list)
    rw_version: int = 0
    source_path: str = ""

    @property
    def frame_count(self) -> int:
        return len(self.frames)

    @property
    def geometry_count(self) -> int:
        return len(self.geometries)

    @property
    def atomic_count(self) -> int:
        return len(self.atomics)

    def get_frame_name(self, index: int) -> str:
        if 0 <= index < len(self.frames):
            return self.frames[index].name or f"Frame_{index}"
        return f"Frame_{index}"

    def get_geometry_for_atomic(self, atomic: Atomic) -> Optional[Geometry]:
        if 0 <= atomic.geometry_index < len(self.geometries):
            return self.geometries[atomic.geometry_index]
        return None
