
#this belongs in methods.rw_versions.py - Version: 4
# X-Seti - November18 2025 - IMG Factory 1.5 - RenderWare Version Constants
"""
RenderWare Version Constants - Expanded
Standalone module for all RenderWare version definitions and utilities
Used by IMG, TXD, DFF, MDL, and validation systems
Includes support for DFF models, MDL files, and other 3D formats
"""

import struct
from enum import Enum
from typing import Dict, Optional, Tuple

## Methods list -
# get_rw_version_name
# is_valid_rw_version
# get_default_version_for_game
# get_version_info
# parse_rw_version
# get_model_format_version
# is_dff_compatible_version
# get_mdl_version_info

class RWVersion(Enum):
    RW_VERSION_3_0_0_0 = 0x30000
    RW_VERSION_3_1_0_1 = 0x31001
    RW_VERSION_3_2_0_0 = 0x32000
    RW_VERSION_3_3_0_2 = 0x33002
    RW_VERSION_3_4_0_1 = 0x34001
    RW_VERSION_3_4_0_3 = 0x34003
    RW_VERSION_3_5_0_0 = 0x35000
    RW_VERSION_3_5_0_2 = 0x35002
    RW_VERSION_3_6_0_3 = 0x36003
    RW_VERSION_3_7_0_2 = 0x37002

class RWSection(Enum):
    STRUCT = 0x0001
    STRING = 0x0002
    EXTENSION = 0x0003
    TEXTURE = 0x0006
    MATERIAL = 0x0007
    MATERIAL_LIST = 0x0008
    ATOMIC = 0x000E
    PLANE_SECTION = 0x000F
    WORLD = 0x0010
    FRAME_LIST = 0x0014
    GEOMETRY = 0x0015
    CLUMP = 0x001A
    TEXTURE_DICTIONARY = 0x0016
    TEXTURE_NATIVE = 0x0015

class ModelFormat(Enum):
    DFF = "dff"
    MDL = "mdl"
    WDR = "wdr"
    YDR = "ydr"
    OBJ = "obj"
    PLY = "ply"
    COLLADA = "dae"
    GLTF = "gltf"

class DFFVersion(Enum):
    DFF_GTA3 = 0x31001
    DFF_GTAVC = 0x33002
    DFF_GTASOL = 0x34001
    DFF_GTASA = 0x36003
    DFF_BULLY = 0x36003
    DFF_MANHUNT = 0x34003

class MDLVersion(Enum):
    MDL_LCS = 0x35000
    MDL_VCS = 0x35002
    MDL_PSP_BASE = 0x35000

def get_rw_version_name(version_value: int) -> str: #vers 3
    rw_versions = {
        0x30000: "3.0.0.0",
        0x31001: "3.1.0.1",
        0x32000: "3.2.0.0",
        0x33002: "3.3.0.2",
        0x34001: "3.4.0.1",
        0x34003: "3.4.0.3",
        0x35000: "3.5.0.0",
        0x35002: "3.5.0.2",
        0x36003: "3.6.0.3",
        0x37002: "3.7.0.2",
        0x0800FFFF: "3.0.0.0 GTA3 (PS2)",
        0x0C02FFFF: "3.3.0.2 GTA3/VC (PS2)",
        0x1003FFFF: "3.1.0.0 GTA VC (PC)",
        0x1005FFFF: "3.2.0.0 GTA VC (PC)",
        0x1401FFFF: "3.4.0.1 Manhunt/SOL",
        0x1400FFFF: "3.4.0.3 GTAVC (PC)",
        0x1803FFFF: "3.6.0.3 GTA SA (PC)",
        0x1C020037: "3.7.0.2 San Andreas-P",
        0x34003: "3.4.0.3 (SA)",
        0x35000: "3.5.0.0 (LCS)",
        0x35002: "3.5.0.2 (VCS)",
        0x36003: "3.6.0.3 (SA/Bully)"
    }
    return rw_versions.get(version_value, f"Unknown (0x{version_value:X})")

def is_valid_rw_version(version_value: int) -> bool: #vers 2
    if 0x30000 <= version_value <= 0x3FFFF:
        return True
    extended_versions = {0x0800FFFF, 0x1003FFFF, 0x1005FFFF, 0x1401FFFF, 0x1400FFFF, 0x1803FFFF, 0x1C020037}
    return version_value in extended_versions

def get_default_version_for_game(game: str) -> int: #vers 2
    game_versions = {
        'gta3': 0x31001, #RWVersion.RW_VERSION_3_1_0_1.value,
        'gtavc': 0x33002, #RWVersion.RW_VERSION_3_3_0_2.value,
        'gtasol': 0x34001, #RWVersion.RW_VERSION_3_4_0_1.value,
        'gtasa': 0x36003, #RWVersion.RW_VERSION_3_6_0_3.value,
        'bully': 0x36003, #TODO
        'lcs': 0x35000, #TODO
        'vcs': 0x35002, #TODO
        'manhunt': 0x34003, #TODO
        'manhunt2': 0x36003, #TODO
    }
    return game_versions.get(game.lower(), 0x36003) #RWVersion.RW_VERSION_3_6_0_3.value

def get_version_info(version_value: int) -> Dict[str, any]: #vers 2
    return {
        'version_hex': f"0x{version_value:X}",
        'version_name': get_rw_version_name(version_value),
        'is_valid': is_valid_rw_version(version_value),
        'major': (version_value >> 16) & 0xFF,
        'minor': (version_value >> 8) & 0xFF,
        'patch': version_value & 0xFF
    }

def parse_rw_version(version_bytes: bytes) -> Tuple[int, str]: #vers 3
    """Parse RenderWare version from 4-byte header - ✅ FIXED"""
    if len(version_bytes) < 4:
        return 0, "Invalid"
    try:
        version_value = struct.unpack('<I', version_bytes)[0]
        version_name = get_rw_version_name(version_value)
        return version_value, version_name
    except struct.error:
        return 0, "Invalid"

def get_model_format_version(file_extension: str, data: bytes) -> Tuple[str, str]: #vers 2
    """Get model format and version from file data - ✅ FIXED SYNTAX"""
    ext = file_extension.lower().lstrip('.')
    if ext == 'dff' and len(data) >= 12:
        try:
            version = struct.unpack('<I', data[8:12])[0]
            return "DFF", get_rw_version_name(version)
        except:
            pass
        return "DFF", "Unknown"
    elif ext == 'mdl' and len(data) >= 12:
        try:
            version = struct.unpack('<I', data[8:12])[0]
            if version == 0x35000:
                return "MDL", "Liberty City Stories PSP"
            elif version == 0x35002:
                return "MDL", "Vice City Stories PSP"
            else:
                return "MDL", f"GTA Stories (0x{version:X})"
        except:
            pass
        return "MDL", "GTA Stories PSP"
    elif ext == 'wdr':
        return "WDR", "GTA IV World Drawable"
    elif ext == 'ydr':
        return "YDR", "GTA V Drawable"
    return ext.upper(), "Unknown"

def is_dff_compatible_version(version_value: int) -> bool: #vers 2
    compatible_versions = [0x31001, 0x33002, 0x34001, 0x34003, 0x36003, 0x34001]
    return version_value in compatible_versions

def get_mdl_version_info(mdl_version: int) -> str: #vers 2
    mdl_versions = {
        0x35000: "Liberty City Stories (PSP)",
        0x35002: "Vice City Stories (PSP)",
    }
    return mdl_versions.get(mdl_version, f"Unknown GTA Stories MDL (0x{mdl_version:X})")

__all__ = [
    'RWVersion', 'RWSection', 'ModelFormat', 'DFFVersion', 'MDLVersion',
    'get_rw_version_name', 'is_valid_rw_version', 'get_default_version_for_game',
    'get_version_info', 'parse_rw_version', 'get_model_format_version',
    'is_dff_compatible_version', 'get_mdl_version_info'
]

