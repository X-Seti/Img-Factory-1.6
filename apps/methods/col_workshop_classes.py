#this belongs in apps/methods/col_workshop_classes.py - Version: 3
# X-Seti - September28 2026 - IMG Factory 1.6 - COL Data Classes

"""
COL data classes - the one set used by every tool (COL1/2/3/4).
Vector3 works as .x/.y/.z and as a 3-item sequence.
"""

from dataclasses import dataclass, field
from typing import List, Tuple
from enum import Enum

##Classes list -
# BoundingBox
# COLBounds
# COLBox
# COLFace
# COLHeader
# COLMaterial
# COLModel
# COLSphere
# COLVersion
# COLVertex
# Vector3

##Methods list -
# _mat_int


class Vector3: #vers 1
    """Mutable 3D point; indexable and iterable like a tuple."""

    def __init__(self, x: float = 0.0, y: float = 0.0, z: float = 0.0): #vers 1
        self.x = float(x)
        self.y = float(y)
        self.z = float(z)

    def __iter__(self):
        return iter((self.x, self.y, self.z))

    def __len__(self):
        return 3

    def __getitem__(self, i):
        return (self.x, self.y, self.z)[i]

    def __setitem__(self, i, v):
        setattr(self, 'xyz'[i], float(v))

    def __eq__(self, other):
        try:
            return tuple(self) == tuple(other)
        except TypeError:
            return NotImplemented

    __hash__ = None

    def __str__(self):
        return f"({self.x:.2f}, {self.y:.2f}, {self.z:.2f})"

    def __repr__(self):
        return f"Vector3({self.x}, {self.y}, {self.z})"


class BoundingBox: #vers 1
    """Axis-aligned bounding box with sphere."""

    def __init__(self): #vers 1
        self.min = Vector3(-1.0, -1.0, -1.0)
        self.max = Vector3(1.0, 1.0, 1.0)
        self.center = Vector3(0.0, 0.0, 0.0)
        self.radius = 1.0

    def __str__(self):
        return f"BoundingBox(min={self.min}, max={self.max}, radius={self.radius:.2f})"


class COLMaterial: #vers 1
    """Surface material id plus flags."""

    def __init__(self, material_id: int = 0, flags: int = 0): #vers 1
        self.material_id = material_id
        self.flags = flags

    def __int__(self):
        return int(self.material_id)

    def __str__(self):
        return f"COLMaterial(id={self.material_id}, flags={self.flags})"


def _mat_int(m) -> int: #vers 1
    """Material id from int or COLMaterial."""
    return int(getattr(m, 'material_id', m) or 0)


class COLVersion(Enum): #vers 1
    """COL file format versions"""
    COL_1 = 1  # GTA III, Vice City
    COL_2 = 2  # GTA SA (PS2)
    COL_3 = 3  # GTA SA (PC/Xbox)
    COL_4 = 4  # GTA SA (unused)


@dataclass
class COLHeader: #vers 1
    """COL model header - 32 bytes total"""
    fourcc: bytes        # 4 bytes: COLL, COL2, COL3, COL4
    size: int            # 4 bytes: file size - 8
    name: str            # 22 bytes: model name
    model_id: int        # 2 bytes: model ID
    version: COLVersion  # Derived from fourcc


@dataclass
class COLBounds: #vers 3
    """COL bounding data - 40 bytes (COL1 order)"""
    radius: float = 0.0
    center: Vector3 = None
    min: Vector3 = None
    max: Vector3 = None

    def __post_init__(self):
        self.center = Vector3(*(self.center or (0.0, 0.0, 0.0)))
        self.min = Vector3(*(self.min or (0.0, 0.0, 0.0)))
        self.max = Vector3(*(self.max or (0.0, 0.0, 0.0)))


@dataclass
class COLSphere: #vers 2
    """COL collision sphere - 20 bytes"""
    radius: float
    center: Vector3
    material: int
    flag: int
    brightness: int
    light: int

    def __post_init__(self):
        self.center = Vector3(*self.center)

    @property
    def material_id(self) -> int:
        return _mat_int(self.material)


@dataclass
class COLBox: #vers 2
    """COL collision box - 28 bytes"""
    min: Vector3
    max: Vector3
    material: int
    flag: int
    brightness: int
    light: int

    def __post_init__(self):
        self.min = Vector3(*self.min)
        self.max = Vector3(*self.max)

    @property
    def min_point(self) -> Vector3:
        return self.min

    @min_point.setter
    def min_point(self, v):
        self.min = Vector3(*v)

    @property
    def max_point(self) -> Vector3:
        return self.max

    @max_point.setter
    def max_point(self, v):
        self.max = Vector3(*v)

    @property
    def material_id(self) -> int:
        return _mat_int(self.material)


@dataclass
class COLVertex: #vers 2
    """COL mesh vertex"""
    x: float
    y: float
    z: float

    @property
    def position(self) -> Vector3:
        return Vector3(self.x, self.y, self.z)

    @position.setter
    def position(self, v):
        self.x, self.y, self.z = (float(c) for c in v)


@dataclass
class COLFace: #vers 2
    """COL mesh face"""
    a: int
    b: int
    c: int
    material: int
    flag: int
    brightness: int
    light: int

    @property
    def vertex_indices(self) -> Tuple[int, int, int]:
        return (self.a, self.b, self.c)

    @vertex_indices.setter
    def vertex_indices(self, abc):
        self.a, self.b, self.c = (int(i) for i in abc)

    @property
    def material_id(self) -> int:
        return _mat_int(self.material)


@dataclass
class COLModel: #vers 3
    """Complete COL model structure; name/version/model_id live in header."""
    header: COLHeader
    bounds: COLBounds
    spheres: List[COLSphere]
    boxes: List[COLBox]
    vertices: List[COLVertex]
    faces: List[COLFace]
    shadow_vertices: List[COLVertex] = field(default_factory=list)   # COL3+
    shadow_faces: List[COLFace] = field(default_factory=list)        # COL3+
    lines_raw: bytes = b''      # COL2+ suspension lines, kept as read
    lines_count: int = 0
    flags: int = 0              # COL2+ header flags

    @property
    def name(self) -> str:
        return self.header.name

    @name.setter
    def name(self, v):
        self.header.name = str(v or '')

    @property
    def version(self) -> COLVersion:
        return self.header.version

    @version.setter
    def version(self, v):
        self.header.version = v if isinstance(v, COLVersion) else COLVersion(int(getattr(v, 'value', v)))

    @property
    def model_id(self) -> int:
        return self.header.model_id

    @model_id.setter
    def model_id(self, v):
        self.header.model_id = int(v or 0)

    @property
    def bounding_box(self) -> COLBounds:
        return self.bounds

    def get_stats(self) -> dict: #vers 2
        """Get model statistics"""
        return {
            'name': self.header.name,
            'version': self.header.version.name,
            'spheres': len(self.spheres),
            'boxes': len(self.boxes),
            'vertices': len(self.vertices),
            'faces': len(self.faces),
            'shadow_vertices': len(self.shadow_vertices),
            'shadow_faces': len(self.shadow_faces)
        }
