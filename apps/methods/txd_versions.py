#this belongs in Components/Txd_Editor/depends/txd_versions.py - Version: 10
# X-Seti - October13 2025 - IMG Factory 1.5 - TXD Version Detection and Format Utilities

"""
TXD Version Detection and Format Utilities - MASTER VERSION

Provides version detection, platform identification, and capability checking 
for TXD texture files across all platforms (PC, PS2, Xbox, GameCube, PSP) 
for GTA III, Vice City, San Andreas, and State of Liberty.

This is the authoritative version used by both ImgFactory and standalone components.
"""

import struct
from typing import Dict, Tuple, Optional, List
from enum import IntEnum

## Methods list -
# detect_platform_from_data
# detect_txd_version
# get_all_platform_versions
# get_d3d_format_info
# get_d3d_format_name
# get_device_id_name
# get_game_from_version
# get_platform_capabilities
# get_platform_name
# get_recommended_version_for_game
# get_version_capabilities
# get_version_string
# is_bump_map_format
# is_bumpmap_supported
# is_compressed_format
# is_mipmap_supported
# is_palettized_format
# pack_version_id
# unpack_version_id
# validate_txd_format

class TXDPlatform(IntEnum):
    """TXD Platform/Device IDs"""
    DEVICE_NONE = 0
    DEVICE_D3D8 = 1
    DEVICE_D3D9 = 2
    DEVICE_GC = 3
    DEVICE_PS2 = 6
    DEVICE_XBOX = 8
    DEVICE_PSP = 9

class TXDVersion(IntEnum):
    """Known TXD RenderWare versions by platform"""
    # GTA III
    GTA3_PS2 = 0x00000310      # 3.1.0.0
    GTA3_PC = 0x0C02FFFF       # 3.3.0.2
    GTA3_XBOX = 0x35000        # 3.5.0.0
    
    # Vice City
    GTAVC_PS2 = 0x0C02FFFF     # 3.3.0.2
    GTAVC_PC = 0x1003FFFF      # 3.4.0.3
    GTAVC_XBOX = 0x35000       # 3.5.0.0
    
    # San Andreas
    GTASA_ALL = 0x1803FFFF     # 3.6.0.3 (all platforms)
    
    # State of Liberty
    GTASOL = 0x1003FFFF        # 3.4.0.3
    
    # GameCube (Unknown game)
    GAMECUBE = 0x35000         # 3.5.0.0
    
    # Android ports
    GTA3_ANDROID = 0x34005     # 3.4.0.5
    GTAVC_ANDROID = 0x34005    # 3.4.0.5

class D3DFormat(IntEnum):
    """Direct3D Texture Format Constants"""
    # Standard RGBA formats
    D3DFMT_R8G8B8 = 20          # 24-bit RGB
    D3DFMT_A8R8G8B8 = 21        # 32-bit ARGB
    D3DFMT_X8R8G8B8 = 22        # 32-bit RGB (no alpha)
    D3DFMT_R5G6B5 = 23          # 16-bit RGB
    D3DFMT_X1R5G5B5 = 24        # 16-bit RGB
    D3DFMT_A1R5G5B5 = 25        # 16-bit ARGB
    D3DFMT_A4R4G4B4 = 26        # 16-bit ARGB
    D3DFMT_A8 = 28              # 8-bit alpha only
    D3DFMT_R3G3B2 = 27          # 8-bit RGB
    D3DFMT_A8R3G3B2 = 29        # 16-bit ARGB
    D3DFMT_X4R4G4B4 = 30        # 16-bit RGB
    D3DFMT_A2B10G10R10 = 31     # 32-bit ARGB
    D3DFMT_A8B8G8R8 = 32        # 32-bit ARGB
    D3DFMT_X8B8G8R8 = 33        # 32-bit RGB
    D3DFMT_G16R16 = 34          # 32-bit RG
    D3DFMT_A2R10G10B10 = 35     # 32-bit ARGB
    
    # Palette formats
    D3DFMT_A8P8 = 40            # 8-bit palettized with alpha
    D3DFMT_P8 = 41              # 8-bit palettized
    
    # Luminance formats
    D3DFMT_L8 = 50              # 8-bit luminance
    D3DFMT_A8L8 = 51            # 16-bit alpha + luminance
    D3DFMT_A4L4 = 52            # 8-bit alpha + luminance
    D3DFMT_L16 = 81             # 16-bit luminance
    
    # Bump map formats
    D3DFMT_V8U8 = 60            # 16-bit signed bump map
    D3DFMT_L6V5U5 = 61          # 16-bit bump map with luminance
    D3DFMT_X8L8V8U8 = 62        # 32-bit bump map with luminance
    D3DFMT_Q8W8V8U8 = 63        # 32-bit signed bump map
    D3DFMT_V16U16 = 64          # 32-bit signed bump map
    D3DFMT_A2W10V10U10 = 67     # 32-bit signed bump map with alpha
    
    # DXT compressed formats
    D3DFMT_DXT1 = 0x31545844    # DXT1 compression (BC1)
    D3DFMT_DXT2 = 0x32545844    # DXT2 compression (BC2 premultiplied)
    D3DFMT_DXT3 = 0x33545844    # DXT3 compression (BC2)
    D3DFMT_DXT4 = 0x34545844    # DXT4 compression (BC3 premultiplied)
    D3DFMT_DXT5 = 0x35545844    # DXT5 compression (BC3)

def detect_txd_version(data: bytes) -> Tuple[int, int, str]: #vers 3
    """
    Detect TXD version from file data
    
    Args:
        data: TXD file bytes (needs at least 28 bytes)
    
    Returns:
        Tuple of (version_id, device_id, version_string)
    """
    if len(data) < 28:
        return (0, 0, "Invalid - File too small")
    
    try:
        # Read section header
        section_type = struct.unpack('<I', data[0:4])[0]
        
        # Check for Texture Dictionary section (0x16)
        if section_type != 0x16:
            return (0, 0, "Invalid - Not a TXD file")
        
        # Version is at offset 8-11
        version_id = struct.unpack('<I', data[8:12])[0]
        
        # Device ID is at offset 20-21 (in the struct section)
        # Skip to struct: 12 bytes header + 12 bytes section header = 24
        device_id = struct.unpack('<H', data[26:28])[0]
        
        # Get readable version string
        version_str = get_version_string(version_id, device_id)
        
        return (version_id, device_id, version_str)
        
    except struct.error:
        return (0, 0, "Invalid - Read error")

def detect_platform_from_data(data: bytes, offset: int = 12) -> int: #vers 2
    """
    Detect platform ID from native texture data
    Used when reading Texture Native sections
    
    Args:
        data: TXD texture native data
        offset: Offset to platform ID (default 12 for struct start)
    
    Returns:
        Platform ID (NativePlatformType)
    """
    if len(data) < offset + 4:
        return 0
    
    try:
        platform_id = struct.unpack('<I', data[offset:offset+4])[0]
        
        # Check for PS2 FourCC format (0x00325350 = "PS2\0")
        if platform_id == 0x00325350:
            return TXDPlatform.DEVICE_PS2
        
        # GameCube uses platform ID with upper byte
        if (platform_id >> 24) == 6:
            return TXDPlatform.DEVICE_GC
            
        return platform_id
        
    except struct.error:
        return 0

def get_version_string(version_id: int, device_id: int = 0) -> str: #vers 3
    """Convert version ID to readable string with platform info"""
    # Check known versions first
    version_map = {
        (0x00000310, 6): "3.1.0.0 (GTA III PS2)",
        (0x00000310, 0): "3.1.0.0 (GTA III)",
        (0x0C02FFFF, 6): "3.3.0.2 (GTA III PC / VC PS2)",
        (0x0C02FFFF, 0): "3.3.0.2 (GTA III/VC PC)",
        (0x1003FFFF, 0): "3.4.0.3 (Vice City PC / SOL)",
        (0x1003FFFF, 1): "3.4.0.3 (Vice City PC D3D8)",
        (0x1003FFFF, 2): "3.4.0.3 (Vice City PC D3D9)",
        (0x1803FFFF, 0): "3.6.0.3 (San Andreas)",
        (0x1803FFFF, 6): "3.6.0.3 (San Andreas PS2)",
        (0x1803FFFF, 8): "3.6.0.3 (San Andreas Xbox)",
        (0x1803FFFF, 9): "3.6.0.3 (San Andreas PSP)",
        (0x0800FFFF, 0): "3.0.0.3",
        (0x35000, 3): "3.5.0.0 (Unknown GameCube)",
        (0x35000, 8): "3.5.0.0 (GTA III/VC Xbox)",
        (0x35000, 9): "3.5.0.0 (LCS PSP)",
        (0x35002, 9): "3.5.0.2 (VCS PSP)",
        (0x34005, 0): "3.4.0.5 (Android)",
    }
    
    # Try with device ID
    key = (version_id, device_id)
    if key in version_map:
        return version_map[key]
    
    # Try without device ID
    key = (version_id, 0)
    if key in version_map:
        return version_map[key]
    
    # Decode version using RW format
    if version_id & 0xFFFF0000:
        # Extended format (version >= 3.1.0.1)
        v = ((version_id >> 14) & 0x3FF00) + 0x30000
        v |= (version_id >> 16) & 0x3F
        major = (v >> 16) & 0xFF
        minor = (v >> 8) & 0xFF
        patch = v & 0xFF
        platform_str = f" ({get_platform_name(device_id)})" if device_id else ""
        return f"{major}.{minor}.{patch & 0xF}.{patch >> 4}{platform_str}"
    else:
        # Old format
        return f"Unknown (0x{version_id:08X})"

def get_platform_name(device_id: int) -> str: #vers 3
    """Get platform name from device ID"""
    platforms = {
        0: "PC",
        1: "Direct3D 8",
        2: "Direct3D 9", 
        3: "GameCube",
        6: "PlayStation 2",
        8: "Xbox",
        9: "PSP"
    }
    return platforms.get(device_id, f"Unknown ({device_id})")

def get_device_id_name(device_id: int) -> str: #vers 2
    """Alias for get_platform_name for consistency"""
    return get_platform_name(device_id)

def get_game_from_version(version_id: int, device_id: int = 0) -> str: #vers 3
    """
    Determine which GTA game a TXD is from based on version
    
    Args:
        version_id: RenderWare version ID
        device_id: Device/platform ID
    
    Returns:
        Game name string
    """
    # Check version + platform combinations
    if version_id == 0x00000310:
        return "GTA III (PS2)"
    elif version_id == 0x0C02FFFF:
        if device_id == 6:
            return "GTA III (PC) or Vice City (PS2)"
        return "GTA III (PC)"
    elif version_id == 0x1003FFFF:
        return "Vice City (PC) or State of Liberty"
    elif version_id == 0x1803FFFF:
        if device_id == 6:
            return "San Andreas (PS2)"
        elif device_id == 8:
            return "San Andreas (Xbox)"
        elif device_id == 9:
            return "San Andreas (PSP)"
        return "San Andreas (PC)"
    elif version_id == 0x35000:
        if device_id == 3:
            return "Unknown (GameCube)"
        elif device_id == 8:
            return "GTA III or Vice City (Xbox)"
        elif device_id == 9:
            return "Liberty City Stories (PSP)"
        return "Xbox/PSP"
    elif version_id == 0x35002:
        return "Vice City Stories (PSP)"
    elif version_id == 0x34005:
        return "GTA III or Vice City (Android)"
    else:
        return "Unknown GTA version"

def get_version_capabilities(version_id: int) -> Dict[str, any]: #vers 3
    """
    Get format capabilities for a given version
    
    Returns dict with supported features:
    - bit_depths: list of supported bit depths
    - mipmaps: bool
    - bumpmaps: bool  
    - dxt_compression: bool
    - palette: bool
    """
    caps = {
        'bit_depths': [],
        'mipmaps': False,
        'bumpmaps': False,
        'dxt_compression': False,
        'palette': False,
        'swizzled': False
    }
    
    # GTA III
    if version_id in [0x00000310, 0x0C02FFFF]:
        caps['bit_depths'] = [8]
        caps['palette'] = True
        if version_id == 0x0C02FFFF:  # PC version
            caps['bit_depths'] = [8, 16, 32]
            caps['mipmaps'] = False  # Limited support
    
    # Vice City
    elif version_id == 0x1003FFFF:
        caps['bit_depths'] = [8, 16, 32]
        caps['mipmaps'] = True
        caps['dxt_compression'] = True
        caps['palette'] = True
    
    # San Andreas
    elif version_id == 0x1803FFFF:
        caps['bit_depths'] = [32]
        caps['mipmaps'] = True
        caps['bumpmaps'] = True  # Limited with hacks
        caps['dxt_compression'] = True
    
    # Xbox versions
    elif version_id == 0x35000:
        caps['bit_depths'] = [4, 8, 16, 32]
        caps['mipmaps'] = True
        caps['dxt_compression'] = True
        caps['palette'] = True
        caps['swizzled'] = True
    
    # PSP versions
    elif version_id in [0x35000, 0x35002]:
        caps['bit_depths'] = [4, 8, 16, 32]
        caps['mipmaps'] = True
        caps['palette'] = True
        caps['swizzled'] = True
    
    # Android
    elif version_id == 0x34005:
        caps['bit_depths'] = [8, 16, 32]
        caps['mipmaps'] = True
    
    return caps

def get_platform_capabilities(device_id: int) -> Dict[str, any]: #vers 2
    """
    Get platform-specific texture capabilities
    
    Args:
        device_id: Platform/device ID
    
    Returns:
        Dict with platform capabilities
    """
    platform_caps = {
        TXDPlatform.DEVICE_D3D8: {
            'name': 'Direct3D 8',
            'compression': ['DXT1', 'DXT2', 'DXT3', 'DXT4', 'DXT5'],
            'formats': ['8888', '888', '565', '555', '1555', '4444', 'L8', 'A8L8', 'A8', 'A4L4', 'V8U8'],
            'supports_bump': True,
            'swizzled': False,
            'paletted': True
        },
        TXDPlatform.DEVICE_D3D9: {
            'name': 'Direct3D 9',
            'compression': ['DXT1', 'DXT2', 'DXT3', 'DXT4', 'DXT5'],
            'formats': ['8888', '888', '565', '555', '1555', '4444', 'L8', 'L16', 'A8L8', 'A4L4', 'A8', 'V8U8', 'V16U16', 'Q8W8V8U8'],
            'supports_bump': True,
            'swizzled': False,
            'paletted': True
        },
        TXDPlatform.DEVICE_PS2: {
            'name': 'PlayStation 2',
            'compression': [],
            'formats': ['8888', '4444', 'Palette8', 'Palette4'],
            'supports_bump': False,
            'swizzled': True,
            'paletted': True
        },
        TXDPlatform.DEVICE_XBOX: {
            'name': 'Xbox',
            'compression': ['DXT1', 'DXT2', 'DXT3', 'DXT5'],
            'formats': ['8888', '565', '1555', '4444', 'Palette8', 'Palette4', 'V8U8'],
            'supports_bump': True,
            'swizzled': True,
            'paletted': True
        },
        TXDPlatform.DEVICE_GC: {
            'name': 'GameCube',
            'compression': ['CMPR'],
            'formats': ['RGB565', 'RGB5A3', 'RGBA8888', 'I4', 'I8', 'IA4', 'IA8', 'Palette4', 'Palette8'],
            'supports_bump': False,
            'swizzled': True,
            'paletted': True
        },
        TXDPlatform.DEVICE_PSP: {
            'name': 'PSP',
            'compression': [],
            'formats': ['8888', '5650', '5551', '4444', 'Palette8', 'Palette4'],
            'supports_bump': False,
            'swizzled': True,
            'paletted': True
        }
    }
    
    return platform_caps.get(device_id, {
        'name': 'Unknown',
        'compression': [],
        'formats': [],
        'supports_bump': False,
        'swizzled': False,
        'paletted': False
    })

def is_mipmap_supported(version_id: int, device_id: int = 0) -> bool: #vers 3
    """Check if version/platform supports mipmaps"""
    # Most platforms support mipmaps from VC onwards
    if version_id >= 0x1003FFFF:
        return True
    
    # Xbox and PSP support mipmaps even in earlier versions
    if device_id in [TXDPlatform.DEVICE_XBOX, TXDPlatform.DEVICE_PSP]:
        return True
    
    return False

def is_bumpmap_supported(version_id: int, device_id: int = 0) -> bool: #vers 3
    """Check if version/platform supports bumpmaps"""
    # Only San Andreas PC with D3D9 had limited bumpmap support
    if version_id == 0x1803FFFF and device_id in [0, TXDPlatform.DEVICE_D3D9]:
        return True
    
    # SOL (State of Liberty) hybrid format supported both
    if version_id == 0x1003FFFF:
        return True  # With hacks
    
    return False

def pack_version_id(major: int, minor: int, patch: int, binary: int = 3) -> int: #vers 2
    """
    Pack version numbers into RenderWare version ID
    
    Args:
        major: Major version (3 for RW3)
        minor: Minor version
        patch: Patch version
        binary: Binary revision (default 3)
    
    Returns:
        Packed version ID
    """
    version = (major << 16) | (minor << 8) | patch
    
    if version <= 0x31000:
        # Old format
        return version >> 8
    
    # Extended format
    packed = ((version - 0x30000) & 0x3FF00) << 14
    packed |= (binary & 0x3F) << 16
    packed |= 0xFFFF  # Build number
    
    return packed

def unpack_version_id(version_id: int) -> Tuple[int, int, int, int]: #vers 2
    """
    Unpack RenderWare version ID into components
    
    Returns:
        Tuple of (major, minor, patch, build)
    """
    if version_id & 0xFFFF0000:
        # Extended format
        version = ((version_id >> 14) & 0x3FF00) + 0x30000
        version |= (version_id >> 16) & 0x3F
        build = version_id & 0xFFFF
        
        major = (version >> 16) & 0xFF
        minor = (version >> 8) & 0xFF
        patch = version & 0xFF
        
        return (major, minor, patch, build)
    else:
        # Old format
        version = version_id << 8
        major = (version >> 16) & 0xFF
        minor = (version >> 8) & 0xFF  
        patch = version & 0xFF
        
        return (major, minor, patch, 0)

def validate_txd_format(data: bytes, expected_version: Optional[int] = None) -> Tuple[bool, str]: #vers 2
    """
    Validate TXD file format
    
    Args:
        data: TXD file bytes
        expected_version: Optional version ID to validate against
    
    Returns:
        Tuple of (is_valid, error_message)
    """
    if len(data) < 28:
        return (False, "File too small - minimum 28 bytes required")
    
    try:
        # Check section type
        section_type = struct.unpack('<I', data[0:4])[0]
        if section_type != 0x16:
            return (False, f"Invalid section type: 0x{section_type:08X} (expected 0x16)")
        
        # Get version
        version_id, device_id, version_str = detect_txd_version(data)
        
        if version_id == 0:
            return (False, "Could not detect valid TXD version")
        
        # Check against expected version if provided
        if expected_version and version_id != expected_version:
            return (False, f"Version mismatch: found {version_str}, expected 0x{expected_version:08X}")
        
        # Check texture count
        texture_count = struct.unpack('<H', data[24:26])[0]
        if texture_count > 1000:  # Sanity check
            return (False, f"Unrealistic texture count: {texture_count}")
        
        return (True, f"Valid TXD: {version_str}, {get_platform_name(device_id)}, {texture_count} texture(s)")
        
    except Exception as e:
        return (False, f"Validation error: {str(e)}")

def get_recommended_version_for_game(game: str, platform: str = 'pc') -> int: #vers 3
    """
    Get recommended TXD version for target game and platform
    
    Args:
        game: Game identifier (gta3, gtavc, gtasa, gtasol)
        platform: Platform (pc, ps2, xbox, psp, android)
    
    Returns:
        Version ID
    """
    versions = {
        ('gta3', 'pc'): TXDVersion.GTA3_PC,
        ('gta3', 'ps2'): TXDVersion.GTA3_PS2,
        ('gta3', 'xbox'): TXDVersion.GTA3_XBOX,
        ('gta3', 'android'): TXDVersion.GTA3_ANDROID,
        ('gtavc', 'pc'): TXDVersion.GTAVC_PC,
        ('gtavc', 'ps2'): TXDVersion.GTAVC_PS2,
        ('gtavc', 'xbox'): TXDVersion.GTAVC_XBOX,
        ('gtavc', 'android'): TXDVersion.GTAVC_ANDROID,
        ('gtasa', 'pc'): TXDVersion.GTASA_ALL,
        ('gtasa', 'ps2'): TXDVersion.GTASA_ALL,
        ('gtasa', 'xbox'): TXDVersion.GTASA_ALL,
        ('gtasa', 'psp'): TXDVersion.GTASA_ALL,
        ('gtasol', 'pc'): TXDVersion.GTASOL,
        ('lcs', 'psp'): 0x35000,
        ('vcs', 'psp'): 0x35002,
    }
    
    key = (game.lower(), platform.lower())
    return versions.get(key, TXDVersion.GTASA_ALL)

def get_all_platform_versions() -> List[Dict[str, any]]: #vers 2
    """
    Get list of all known TXD platform/version combinations
    
    Returns:
        List of dicts with version info
    """
    return [
        {'game': 'GTA III', 'platform': 'PS2', 'version': 0x00000310, 'device': 6},
        {'game': 'GTA III', 'platform': 'PC', 'version': 0x0C02FFFF, 'device': 0},
        {'game': 'GTA III', 'platform': 'Xbox', 'version': 0x35000, 'device': 8},
        {'game': 'GTA III', 'platform': 'Android', 'version': 0x34005, 'device': 0},
        {'game': 'Vice City', 'platform': 'PS2', 'version': 0x0C02FFFF, 'device': 6},
        {'game': 'Vice City', 'platform': 'PC', 'version': 0x1003FFFF, 'device': 0},
        {'game': 'Vice City', 'platform': 'Xbox', 'version': 0x35000, 'device': 8},
        {'game': 'Vice City', 'platform': 'Android', 'version': 0x34005, 'device': 0},
        {'game': 'San Andreas', 'platform': 'All', 'version': 0x1803FFFF, 'device': 0},
        {'game': 'State of Liberty', 'platform': 'PC', 'version': 0x1003FFFF, 'device': 0},
        {'game': 'Liberty City Stories', 'platform': 'PSP', 'version': 0x35000, 'device': 9},
        {'game': 'Vice City Stories', 'platform': 'PSP', 'version': 0x35002, 'device': 9},
        {'game': 'Unknown', 'platform': 'GameCube', 'version': 0x35000, 'device': 3},
    ]

def get_d3d_format_name(d3d_format: int) -> str: #vers 1
    """
    Get readable name for D3D format code
    
    Args:
        d3d_format: D3D format constant
    
    Returns:
        Format name string
    """
    format_names = {
        # Standard RGBA
        20: "R8G8B8",
        21: "A8R8G8B8",
        22: "X8R8G8B8",
        23: "R5G6B5",
        24: "X1R5G5B5",
        25: "A1R5G5B5",
        26: "A4R4G4B4",
        28: "A8",
        # Luminance
        50: "L8",
        51: "A8L8",
        52: "A4L4",
        81: "L16",
        # Bump maps
        60: "V8U8",
        61: "L6V5U5",
        62: "X8L8V8U8",
        63: "Q8W8V8U8",
        64: "V16U16",
        67: "A2W10V10U10",
        # Palette
        40: "A8P8",
        41: "P8",
        # DXT
        0x31545844: "DXT1",
        0x32545844: "DXT2",
        0x33545844: "DXT3",
        0x34545844: "DXT4",
        0x35545844: "DXT5",
    }
    
    return format_names.get(d3d_format, f"Unknown (0x{d3d_format:08X})")

def get_d3d_format_info(d3d_format: int) -> Dict[str, any]: #vers 1
    """
    Get detailed information about a D3D format
    
    Args:
        d3d_format: D3D format constant
    
    Returns:
        Dict with format information
    """
    info = {
        'name': get_d3d_format_name(d3d_format),
        'bits_per_pixel': 0,
        'compressed': False,
        'has_alpha': False,
        'is_bump': False,
        'is_luminance': False,
        'is_palette': False
    }
    
    # Set properties based on format
    if d3d_format in [20]:  # R8G8B8
        info['bits_per_pixel'] = 24
    elif d3d_format in [21, 22, 32, 33, 34, 35, 62, 63]:  # 32-bit formats
        info['bits_per_pixel'] = 32
        info['has_alpha'] = d3d_format in [21, 32, 35]
    elif d3d_format in [23, 24, 25, 26, 51, 60, 61]:  # 16-bit formats
        info['bits_per_pixel'] = 16
        info['has_alpha'] = d3d_format in [25, 26, 51]
    elif d3d_format in [28, 41, 50, 52]:  # 8-bit formats
        info['bits_per_pixel'] = 8
        info['has_alpha'] = d3d_format in [28, 52]
    elif d3d_format == 64:  # V16U16
        info['bits_per_pixel'] = 32
    elif d3d_format == 81:  # L16
        info['bits_per_pixel'] = 16
    
    # Bump map formats
    if d3d_format in [60, 61, 62, 63, 64, 67]:
        info['is_bump'] = True
    
    # Luminance formats
    if d3d_format in [50, 51, 52, 81, 61, 62]:
        info['is_luminance'] = True
    
    # Palette formats
    if d3d_format in [40, 41]:
        info['is_palette'] = True
    
    # Compressed formats (DXT)
    if d3d_format in [0x31545844, 0x32545844, 0x33545844, 0x34545844, 0x35545844]:
        info['compressed'] = True
        info['bits_per_pixel'] = 4 if d3d_format == 0x31545844 else 8
        info['has_alpha'] = d3d_format != 0x31545844
    
    return info

def is_compressed_format(d3d_format: int) -> bool: #vers 1
    """Check if D3D format is compressed (DXT)"""
    return d3d_format in [0x31545844, 0x32545844, 0x33545844, 0x34545844, 0x35545844]

def is_bump_map_format(d3d_format: int) -> bool: #vers 1
    """Check if D3D format is a bump map format"""
    return d3d_format in [60, 61, 62, 63, 64, 67]

def is_palettized_format(d3d_format: int) -> bool: #vers 1
    """Check if D3D format uses a palette"""
    return d3d_format in [40, 41]
