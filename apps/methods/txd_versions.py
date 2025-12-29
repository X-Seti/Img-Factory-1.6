#this belongs in methods/txd_versions.py - Version: 3
# X-Seti - October13 2025 - IMG Factory 1.5 - TXD Version Detection (Redirect)

"""
TXD Version Detection - Import Redirect

This file redirects all imports to the master version in methods/txd_versions.py
to avoid code duplication and ensure consistency between ImgFactory and standalone components.
"""

# Import everything from the master version
from apps.methods.txd_versions import (
    # Classes
    TXDPlatform,
    TXDVersion,
    D3DFormat,
    
    # Functions
    detect_txd_version,
    detect_platform_from_data,
    get_version_string,
    get_platform_name,
    get_device_id_name,
    get_game_from_version,
    get_version_capabilities,
    get_platform_capabilities,
    is_mipmap_supported,
    is_bumpmap_supported,
    pack_version_id,
    unpack_version_id,
    validate_txd_format,
    get_recommended_version_for_game,
    get_all_platform_versions,
    get_d3d_format_name,
    get_d3d_format_info,
    is_compressed_format,
    is_bump_map_format,
    is_palettized_format
)

# Make all imports available for 'from txd_versions import *'
__all__ = [
    'TXDPlatform',
    'TXDVersion',
    'D3DFormat',
    'detect_txd_version',
    'detect_platform_from_data',
    'get_version_string',
    'get_platform_name',
    'get_device_id_name',
    'get_game_from_version',
    'get_version_capabilities',
    'get_platform_capabilities',
    'is_mipmap_supported',
    'is_bumpmap_supported',
    'pack_version_id',
    'unpack_version_id',
    'validate_txd_format',
    'get_recommended_version_for_game',
    'get_all_platform_versions',
    'get_d3d_format_name',
    'get_d3d_format_info',
    'is_compressed_format',
    'is_bump_map_format',
    'is_palettized_format'
]
