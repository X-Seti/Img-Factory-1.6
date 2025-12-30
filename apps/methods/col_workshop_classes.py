#this belongs in methods/col_workshop_classes.py - Version: 1
# X-Seti - December21 2025 - Col Workshop - COL Data Classes

"""
Pure data classes for collision models
Supports COL1 (GTA3/VC) initially, COL2/3 (SA) to be added
"""

from dataclasses import dataclass
from typing import List, Tuple
from enum import Enum

##Classes list -
# COLBounds
# COLBox  
# COLFace
# COLHeader
# COLModel
# COLSphere
# COLVersion
# COLVertex

class COLVersion(Enum): #vers 1
    """COL file format versions"""
    COL_1 = 1  # GTA III, Vice City
    COL_2 = 2  # GTA SA (PS2)
    COL_3 = 3  # GTA SA (PC/Xbox)
    COL_4 = 4  # GTA SA (unused)

@dataclass
class COLHeader: #vers 1
    """COL model header - 32 bytes total"""
    fourcc: bytes        # 4 bytes - b'COLL', b'COL2', b'COL3', b'COL4'
    size: int            # 4 bytes - file size from after this value
    name: str            # 22 bytes - model name (null-terminated)
    model_id: int        # 2 bytes - model ID (0-19999)
    version: COLVersion  # Derived from fourcc

@dataclass
class COLBounds: #vers 1
    """COL bounding data - 40 bytes (COL1 order)"""
    radius: float                        # 4 bytes
    center: Tuple[float, float, float]   # 12 bytes
    min: Tuple[float, float, float]      # 12 bytes
    max: Tuple[float, float, float]      # 12 bytes

@dataclass
class COLSphere: #vers 1
    """COL collision sphere - 20 bytes (COL1)"""
    radius: float                        # 4 bytes
    center: Tuple[float, float, float]   # 12 bytes
    material: int                        # 1 byte
    flag: int                            # 1 byte
    brightness: int                      # 1 byte
    light: int                           # 1 byte

@dataclass
class COLBox: #vers 1
    """COL collision box - 28 bytes"""
    min: Tuple[float, float, float]      # 12 bytes
    max: Tuple[float, float, float]      # 12 bytes
    material: int                        # 1 byte
    flag: int                            # 1 byte
    brightness: int                      # 1 byte
    light: int                           # 1 byte

@dataclass
class COLVertex: #vers 1
    """COL mesh vertex - 12 bytes (COL1 float)"""
    x: float
    y: float
    z: float

@dataclass
class COLFace: #vers 1
    """COL mesh face - 16 bytes (COL1)"""
    a: int              # 4 bytes - vertex index
    b: int              # 4 bytes - vertex index
    c: int              # 4 bytes - vertex index
    material: int       # 1 byte
    flag: int           # 1 byte
    brightness: int     # 1 byte
    light: int          # 1 byte

@dataclass
class COLModel: #vers 1
    """Complete COL model structure"""
    header: COLHeader
    bounds: COLBounds
    spheres: List[COLSphere]
    boxes: List[COLBox]
    vertices: List[COLVertex]
    faces: List[COLFace]
    
    def get_stats(self) -> dict: #vers 1
        """Get model statistics"""
        return {
            'name': self.header.name,
            'version': self.header.version.name,
            'spheres': len(self.spheres),
            'boxes': len(self.boxes),
            'vertices': len(self.vertices),
            'faces': len(self.faces)
        }

