
#this belongs in methods.rw_versions.py - Version: 7
# X-Seti - November18 2025 - IMG Factory 1.5 - RenderWare Version Constants
"""
RenderWare Version Constants - Expanded
Standalone module for all RenderWare version definitions and utilities
Used by IMG, TXD, DFF, MDL, and validation systems
Includes support for DFF models, MDL files, and other 3D formats
"""

"""
RenderWare Version Encoding Notes

RenderWare versions are encoded internally as:

    RW_VERSION_M_m_0_p = 0xMm0p

Where:
    M = major (always 3 for GTA-era RenderWare)
    m = minor (0..7)
    p = patch/build

Examples:
    3.4.0.3  -> 0x34003
    3.6.0.3  -> 0x36003
    3.7.0.2  -> 0x37002

Observed platform-specific variants (PS2 / PC / PSP) sometimes use
alternative packed forms such as:

    0x1403FFFF -> 3.4.0.3 (PS2 / late VC)
    0x1803FFFF -> 3.6.0.3 (SA PC)
    0x1C020037 -> 3.7.0.2 (SA Mobile/PSP)

These extended forms preserve the minor/patch information but encode
additional platform/build flags in the high and low words.

Canonical known versions:

    3.0.0.0 -> 0x30000
    3.1.0.0 -> 0x31000
    3.1.0.1 -> 0x31001
    3.2.0.0 -> 0x32000
    3.3.0.2 -> 0x33002
    3.4.0.1 -> 0x34001
    3.4.0.3 -> 0x34003
    3.5.0.0 -> 0x35000  (internal / early Stories)
    3.5.0.1 -> 0x35001  (Liberty City Stories)
    3.5.0.2 -> 0x35002  (Vice City Stories)
    3.6.0.3 -> 0x36003  (San Andreas, Bully)
    3.7.0.2 -> 0x37002  (San Andreas Mobile / PSP)

3.5.x.x exists in SDKs but is rarely seen in PC-era DFF files.
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
    RW_VERSION_3_1_0_0 = 0x31000
    RW_VERSION_3_1_0_1 = 0x31001
    RW_VERSION_3_2_0_0 = 0x32000
    RW_VERSION_3_3_0_2 = 0x33002
    RW_VERSION_3_4_0_1 = 0x34001
    RW_VERSION_3_4_0_3 = 0x34003
    RW_VERSION_3_5_0_0 = 0x35000
    RW_VERSION_3_5_0_1 = 0x35001
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

def get_rw_version_name(version_value: int) -> str:  # vers 11
    rw_versions = {
        # ---- Canonical SDK versions ----
        0x00000300: "3.0.0 (GTA3 early — plain-int)",
        0x00000304: "3.0.4 (GTA3 early — plain-int)",
        0x00000310: "3.1.0 (GTA3/VC PS2 — plain-int)",
        0x00000314: "3.1.4 (VC PS2 — plain-int)",
        0x00000320: "3.2.0 (GTA III/VC PS2 — plain-int)",
        0x20001: "2.0.0.1 (GTA3) Radar Tex",
        0x30000: "3.0.0.0",
        0x31000: "3.1.0.0",
        0x31001: "3.1.0.1",
        0x32000: "3.2.0.0",
        0x33002: "3.3.0.2",
        0x34001: "3.4.0.1 (Manhunt / SOL)",
        0x34003: "3.4.0.3",
        0x35000: "3.5.0.0 (Internal / Dev)",
        0x35001: "3.5.0.1 (LCS / MDL)",
        0x35002: "3.5.0.2 (VCS)",
        0x36003: "3.6.0.3 (SA / Bully)",
        0x37002: "3.7.0.2",

        # ---- Extended / platform-packed forms ----
        0x0401FFFF: "2.0.0.1 (GTA3 early TXD)",
        0x0401FFFE: "2.0.0.0 (GTA3 early)",
        0x0800FFFF: "3.0.0.0", #GTA3 (PS2)
        0x0C00FFFF: "3.1.0.0", #GTA3/VC (PC)
        0x0C01FFFF: "3.1.0.1", #GTA VC (PC)
        0x0C02FFFF: "3.1.0.2", #GTA III PC / GTA VC (PS2)
        0x1000FFFF: "3.2.0.0", #GTA3 (PC)
        0x1003FFFF: "3.2.0.3", #GTA3 (PC TXD)
        0x1005FFFF: "3.2.0.5", #GTA VC (PC)
        0x1402FFFF: "3.3.0.2", #GTA3/VC (PS2)
        0x1401FFFF: "3.4.0.1", #Manhunt / SOL
        0x1403FFFF: "3.4.0.3", #GTA VC (late)
        0x1400FFFF: "3.4.0.0", #GTA III/VC (Xbox)
        0x1800FFFF: "3.5.0.0", #Internal Dev (SA Alpha)
        0x1801FFFF: "3.5.0.1", #Internal Dev (LCS)
        0x1802FFFF: "3.5.0.2", #Internal Dev (VCS)
        0x1803FFFF: "3.6.0.3", #GTA SA (PC)
        0x1C020037: "3.7.0.2", #San Andreas Mobile / PSP
        0x1C02000A: "3.7.0.2-Bully (PS2/PC)",  #Bully / Canis Canem Edit
        0x1C020085: "3.7.0.2-Bully variant",   #Bully alternate build
        0x35002000: "3.5.0.2 (VCS PS2/PSP)",
        0x35001000: "3.5.0.1 (LCS PS2/PSP)",
    }


    return rw_versions.get(version_value, f"Unknown (0x{version_value:X})")

def is_valid_rw_version(version_value: int) -> bool: #vers 6
    """Check if value is a plausible RenderWare version number."""
    if not version_value:
        return False
    # Plain early format: 0x300..0x3FF (GTA3 PS2 old-style, e.g. 0x310=3.1.0)
    if 0x300 <= version_value <= 0x3FF:
        return True
    # Old compact format: 3.0.0 .. 3.7.x (0x30000 - 0x3FFFF)
    if 0x30000 <= version_value <= 0x3FFFF:
        return True
    # Packed format: low word == 0xFFFF, high word 0x0400..0x1C03
    # Covers 0x0401FFFF (early GTA3 TXD) through 0x1C03FFFF
    if (version_value & 0xFFFF) == 0xFFFF and 0x0400 <= (version_value >> 16) <= 0x1C03:
        return True
    # Known named non-standard versions (SA mobile, Bully, etc.)
    # Check against the get_rw_version_name dict directly
    _named = {
        0x1C020037,  # SA Mobile / PSP
        0x1C02000A,  # Bully PS2/PC
        0x1C020085,  # Bully variant
        0x00000310, 0x00000300, 0x00000304,  # GTA3 PS2 plain-int
        0x00000314, 0x00000320,              # VC PS2 plain-int
    }
    if version_value in _named:
        return True
    # Fallback: any version that has a named entry in rw_versions dict
    # (covers future additions without needing to update this function)
    return f"Unknown" not in get_rw_version_name(version_value)

def get_default_version_for_game(game: str) -> int: #vers 2
    game_versions = {
        'gta3': 0x31001, #RWVersion.RW_VERSION_3_1_0_1.value,
        'gtavc': 0x33002, #RWVersion.RW_VERSION_3_3_0_2.value,
        'gtasol': 0x34001, #RWVersion.RW_VERSION_3_4_0_1.value,
        'gtasa': 0x36003, #RWVersion.RW_VERSION_3_6_0_3.value,
        'bully': 0x36003, #STUB: version needs verification
        'lcs': 0x35000, #STUB: version needs verification
        'vcs': 0x35002, #STUB: version needs verification
        'manhunt': 0x34003, #STUB: version needs verification
        'manhunt2': 0x36003, #STUB: version needs verification
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

