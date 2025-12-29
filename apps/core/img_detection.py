#this belongs in core/img_platform_detection.py - Version: 1
# X-Seti - July20 2025 - IMG Factory 1.5 - Enhanced Platform Detection
# Expands IMG loading for PS2, Android, and Stories versions

"""
IMG Platform Detection - Enhanced Support
Detects and handles PS2, Android, and GTA Stories IMG formats
Uses existing methods.rw_versions.py and core/img_formats.py functions
"""

import os
import struct
from typing import Dict, List, Optional, Tuple, Any
from enum import Enum
from pathlib import Path

# Import existing functions - NO NEW FUNCTIONALITY
from apps.methods.rw_versions import get_rw_version_name, get_mdl_version_info
from apps.methods.img_core_classes import IMGVersion, IMGEntry, IMGFile

##Methods list -
# detect_img_platform
# detect_ps2_img_format
# detect_android_img_format  
# detect_stories_img_format
# get_platform_specific_specs
# parse_ps2_img_entry
# parse_android_img_entry
# parse_stories_img_entry

##Classes -
# IMGPlatform
# PlatformSpecs

class IMGPlatform(Enum):
    """Extended platform detection for IMG files"""
    PC = "pc"
    PS2 = "ps2" 
    XBOX = "xbox"
    PSP = "psp"
    ANDROID = "android"
    IOS = "ios"
    UNKNOWN = "unknown"

class PlatformSpecs:
    """Platform-specific IMG specifications"""
    
    SPECS = {
        IMGPlatform.PC: {
            'sector_size': 2048,
            'entry_size': 32,
            'name_length': 24,
            'endianness': 'little',
            'supports_compression': True,
            'max_entries': 65535
        },
        
        IMGPlatform.PS2: {
            'sector_size': 2048,
            'entry_size': 32,
            'name_length': 24,
            'endianness': 'little',  # PS2 uses little-endian for IMG
            'supports_compression': False,
            'max_entries': 16000,
            'special_alignment': True
        },
        
        IMGPlatform.ANDROID: {
            'sector_size': 2048,
            'entry_size': 32,
            'name_length': 24,
            'endianness': 'little',
            'supports_compression': True,
            'max_entries': 32000,
            'mobile_optimized': True
        },
        
        IMGPlatform.PSP: {
            'sector_size': 2048,
            'entry_size': 32, 
            'name_length': 24,
            'endianness': 'little',
            'supports_compression': False,
            'max_entries': 8000,
            'stories_format': True
        }
    }

def detect_img_platform(file_path: str) -> Tuple[IMGPlatform, Dict[str, Any]]: #vers 1
    """Detect IMG platform and return platform + detection info"""
    try:
        detection_info = {
            'confidence': 0,
            'indicators': [],
            'version': IMGVersion.UNKNOWN,
            'special_features': []
        }
        
        # Check file size and path indicators first
        file_size = os.path.getsize(file_path)
        filename = os.path.basename(file_path).lower()
        
        # Platform indicators from filename/path
        path_indicators = _get_path_platform_indicators(file_path, filename)
        detection_info['indicators'].extend(path_indicators)
        
        # Read file header for platform detection
        with open(file_path, 'rb') as f:
            header_data = f.read(64)  # Read first 64 bytes
            
        # Try different platform detection methods
        platform_scores = {}
        
        # Test PS2 format
        ps2_score, ps2_info = detect_ps2_img_format(header_data, file_size)
        if ps2_score > 0:
            platform_scores[IMGPlatform.PS2] = ps2_score
            detection_info['indicators'].extend(ps2_info.get('indicators', []))
            
        # Test Android format
        android_score, android_info = detect_android_img_format(header_data, file_size)
        if android_score > 0:
            platform_scores[IMGPlatform.ANDROID] = android_score
            detection_info['indicators'].extend(android_info.get('indicators', []))
            
        # Test Stories/PSP format
        stories_score, stories_info = detect_stories_img_format(header_data, file_size)
        if stories_score > 0:
            platform_scores[IMGPlatform.PSP] = stories_score
            detection_info['indicators'].extend(stories_info.get('indicators', []))
            
        # Default to PC if no specific platform detected
        if not platform_scores:
            platform_scores[IMGPlatform.PC] = 50  # Default confidence
            detection_info['indicators'].append('Default PC format assumed')
            
        # Get highest scoring platform
        best_platform = max(platform_scores.items(), key=lambda x: x[1])
        detection_info['confidence'] = best_platform[1]
        
        print(f"[DEBUG] Platform detection: {best_platform[0].value} (confidence: {best_platform[1]}%)")
        
        return best_platform[0], detection_info
        
    except Exception as e:
        print(f"[ERROR] Platform detection failed: {e}")
        return IMGPlatform.UNKNOWN, {'error': str(e)}

def detect_ps2_img_format(header_data: bytes, file_size: int) -> Tuple[int, Dict[str, Any]]: #vers 1
    """Detect PS2-specific IMG format characteristics"""
    score = 0
    info = {'indicators': [], 'features': []}
    
    try:
        # PS2 IMG files often have specific characteristics
        if len(header_data) >= 8:
            # Check for VER2 header (PS2 SA uses this)
            if header_data[:4] == b'VER2':
                score += 30
                info['indicators'].append('VER2 header found')
                
                # Check entry count for PS2 typical ranges
                entry_count = struct.unpack('<I', header_data[4:8])[0]
                if 100 <= entry_count <= 16000:  # PS2 typical range
                    score += 20
                    info['indicators'].append(f'PS2-typical entry count: {entry_count}')
                    
        # PS2 files often have specific size ranges
        if 10 * 1024 * 1024 <= file_size <= 700 * 1024 * 1024:  # 10MB to 700MB typical for PS2
            score += 15
            info['indicators'].append('PS2-typical file size range')
            
        # Check for PS2-specific file alignment patterns
        if len(header_data) >= 32:
            # PS2 often has specific padding patterns
            padding_check = header_data[16:32]
            if padding_check.count(b'\x00') > 12:  # Lots of null padding
                score += 10
                info['indicators'].append('PS2-style null padding detected')
                
    except Exception as e:
        print(f"[WARNING] PS2 detection error: {e}")
        
    return score, info

def detect_android_img_format(header_data: bytes, file_size: int) -> Tuple[int, Dict[str, Any]]: #vers 1
    """Detect Android/Mobile IMG format characteristics"""
    score = 0
    info = {'indicators': [], 'features': []}
    
    try:
        # Android IMG files have specific characteristics
        if len(header_data) >= 8:
            if header_data[:4] == b'VER2':
                score += 25
                info['indicators'].append('VER2 header (Android compatible)')
                
                # Android typically has higher entry counts
                entry_count = struct.unpack('<I', header_data[4:8])[0]
                if entry_count > 20000:  # Android often has more entries
                    score += 25
                    info['indicators'].append(f'High entry count typical of Android: {entry_count}')
                    
        # Android files are often larger due to higher resolution textures
        if file_size > 500 * 1024 * 1024:  # > 500MB suggests mobile with HD textures
            score += 20
            info['indicators'].append('Large file size suggests mobile/HD format')
            
        # Check for mobile-specific compression patterns
        if len(header_data) >= 16:
            # Look for compression signatures that Android uses
            compression_indicators = [b'\x78\x9c', b'\x78\xda', b'\x1f\x8b']  # zlib/gzip signatures
            for sig in compression_indicators:
                if sig in header_data:
                    score += 15
                    info['indicators'].append('Mobile compression signature detected')
                    break
                    
    except Exception as e:
        print(f"[WARNING] Android detection error: {e}")
        
    return score, info

def detect_stories_img_format(header_data: bytes, file_size: int) -> Tuple[int, Dict[str, Any]]: #vers 1
    """Detect GTA Stories (PSP) IMG format characteristics"""
    score = 0
    info = {'indicators': [], 'features': []}
    
    try:
        # Stories uses different format variations
        if len(header_data) >= 8:
            # Stories can use both VER2 and custom headers
            if header_data[:4] == b'VER2':
                score += 20
                info['indicators'].append('VER2 header (Stories compatible)')
                
                # Stories typically has moderate entry counts
                entry_count = struct.unpack('<I', header_data[4:8])[0]
                if 500 <= entry_count <= 8000:  # PSP limitations
                    score += 25
                    info['indicators'].append(f'PSP-typical entry count: {entry_count}')
                    
            # Check for Stories-specific signatures
            elif header_data[:4] in [b'PSPS', b'UMD\x00']:  # PSP-specific signatures
                score += 40
                info['indicators'].append('PSP-specific header signature')
                
        # PSP file size limitations (UMD capacity)
        if file_size <= 1800 * 1024 * 1024:  # UMD max ~1.8GB
            score += 10
            info['indicators'].append('Within PSP/UMD size limits')
            
        # Look for Stories-specific RW version patterns in header
        if len(header_data) >= 32:
            # Stories uses specific RenderWare versions
            for offset in range(8, 28, 4):
                if offset + 4 <= len(header_data):
                    potential_rw = struct.unpack('<I', header_data[offset:offset+4])[0]
                    # Check for Stories RW versions: 0x35000 (LCS), 0x35002 (VCS)
                    if potential_rw in [0x35000, 0x35002]:
                        score += 30
                        version_name = get_mdl_version_info(potential_rw)
                        info['indicators'].append(f'Stories RW version detected: {version_name}')
                        break
                        
    except Exception as e:
        print(f"[WARNING] Stories detection error: {e}")
        
    return score, info

def _get_path_platform_indicators(file_path: str, filename: str) -> List[str]: #vers 1
    """Get platform indicators from file path and name"""
    indicators = []
    
    # Path-based indicators
    path_lower = file_path.lower()
    
    if any(keyword in path_lower for keyword in ['ps2', 'playstation2', 'pcsx2']):
        indicators.append('PS2 path indicator')
        
    if any(keyword in path_lower for keyword in ['android', 'mobile', 'apk']):
        indicators.append('Android path indicator')
        
    if any(keyword in path_lower for keyword in ['psp', 'stories', 'lcs', 'vcs', 'liberty', 'vice city stories']):
        indicators.append('PSP/Stories path indicator')
        
    # Filename-based indicators
    if any(keyword in filename for keyword in ['ps2', 'android', 'mobile', 'psp', 'stories']):
        indicators.append('Platform-specific filename')
        
    return indicators

def get_platform_specific_specs(platform: IMGPlatform) -> Dict[str, Any]: #vers 1
    """Get platform-specific specifications for IMG handling"""
    return PlatformSpecs.SPECS.get(platform, PlatformSpecs.SPECS[IMGPlatform.PC])

def parse_ps2_img_entry(entry_data: bytes, platform_specs: Dict[str, Any]) -> Optional[IMGEntry]: #vers 1
    """Parse PS2-specific IMG entry format"""
    try:
        if len(entry_data) < 32:
            return None
            
        # PS2 uses same basic structure as PC but may have alignment differences
        entry_offset, entry_size = struct.unpack('<II', entry_data[:8])
        entry_name = entry_data[8:32].rstrip(b'\x00').decode('ascii', errors='ignore')
        
        if entry_name:
            entry = IMGEntry()
            entry.name = entry_name
            entry.offset = entry_offset * platform_specs['sector_size']
            entry.size = entry_size * platform_specs['sector_size']
            
            # PS2-specific handling
            if platform_specs.get('special_alignment'):
                # PS2 may need special alignment handling
                entry.offset = (entry.offset + 2047) & ~2047  # Align to 2048
                
            return entry
            
    except Exception as e:
        print(f"[ERROR] PS2 entry parsing failed: {e}")
        
    return None

def parse_android_img_entry(entry_data: bytes, platform_specs: Dict[str, Any]) -> Optional[IMGEntry]: #vers 1
    """Parse Android-specific IMG entry format"""
    try:
        if len(entry_data) < 32:
            return None
            
        # Android uses standard format but may have different compression handling
        entry_offset, entry_size = struct.unpack('<II', entry_data[:8])
        entry_name = entry_data[8:32].rstrip(b'\x00').decode('ascii', errors='ignore')
        
        if entry_name:
            entry = IMGEntry()
            entry.name = entry_name
            entry.offset = entry_offset * platform_specs['sector_size']
            entry.size = entry_size * platform_specs['sector_size']
            
            # Android-specific handling
            if platform_specs.get('mobile_optimized'):
                # Android may use compression more aggressively
                entry.compression_type = "mobile_optimized"
                
            return entry
            
    except Exception as e:
        print(f"[ERROR] Android entry parsing failed: {e}")
        
    return None

def parse_stories_img_entry(entry_data: bytes, platform_specs: Dict[str, Any]) -> Optional[IMGEntry]: #vers 1
    """Parse GTA Stories (PSP) IMG entry format"""
    try:
        if len(entry_data) < 32:
            return None
            
        # Stories may use slightly different format
        entry_offset, entry_size = struct.unpack('<II', entry_data[:8])
        entry_name = entry_data[8:32].rstrip(b'\x00').decode('ascii', errors='ignore')
        
        if entry_name:
            entry = IMGEntry()
            entry.name = entry_name
            entry.offset = entry_offset * platform_specs['sector_size']
            entry.size = entry_size * platform_specs['sector_size']
            
            # Stories-specific handling
            if platform_specs.get('stories_format'):
                # PSP has memory limitations, smaller files
                if entry.size > 50 * 1024 * 1024:  # 50MB limit for PSP
                    print(f"[WARNING] Large file for PSP: {entry.name} ({entry.size} bytes)")
                    
            return entry
            
    except Exception as e:
        print(f"[ERROR] Stories entry parsing failed: {e}")
        
    return None

# Export functions for integration
__all__ = [
    'IMGPlatform',
    'PlatformSpecs', 
    'detect_img_platform',
    'detect_ps2_img_format',
    'detect_android_img_format',
    'detect_stories_img_format',
    'get_platform_specific_specs',
    'parse_ps2_img_entry',
    'parse_android_img_entry', 
    'parse_stories_img_entry'
]
