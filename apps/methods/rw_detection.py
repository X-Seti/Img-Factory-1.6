#this belongs in methods/rw_detection.py - Version: 2
# X-Seti - September09 2025 - IMG Factory 1.5 - RW Detection Methods

"""
RW Detection Methods - Comprehensive RenderWare version detection system
Fixes the RW Address/Version detection issues for new entries
Uses comprehensive RW version data from apps.methods.rw_versions.py
"""

import struct
from typing import Optional, Tuple, Any

# Import comprehensive RW version data from existing core module
from apps.methods.rw_versions import (
    get_rw_version_name, is_valid_rw_version, parse_rw_version,
    get_model_format_version, RWVersion, DFFVersion, ModelFormat
)

##Methods list -
# analyze_all_entries_rw_versions_working
# analyze_entry_rw_version_working
# detect_col_version
# detect_rw_version_from_data
# ensure_entry_has_rw_data
# get_file_type_from_name
# integrate_rw_detection_working

def detect_rw_version_from_data(data: bytes, filename: str = "") -> Tuple[Optional[int], str, Tuple[str, str]]: #vers 1
    """Detect RW version from raw data using comprehensive RW version database"""
    if len(data) < 4:
        return None, "Unknown", (get_file_type_from_name(filename), "Unknown")
    
    # Get file extension for type detection
    file_type = get_file_type_from_name(filename)
    
    # COL files have different detection
    if file_type == "COL":
        return detect_col_version(data)
    
    # RenderWare files (DFF/TXD) - use comprehensive version data
    if file_type in ['DFF', 'TXD'] and len(data) >= 12:
        # RW version is at bytes 8-12
        version_bytes = data[8:12]
        
        # Use comprehensive parsing from apps.methods.rw_versions.py
        version_value, version_name = parse_rw_version(version_bytes)
        
        if is_valid_rw_version(version_value):
            # Get detailed model format info for DFF files
            if file_type == 'DFF':
                format_type, format_version = get_model_format_version(filename, data)
                return version_value, version_name, (format_type, format_version)
            else:
                return version_value, version_name, (file_type, version_name)
    
    # Non-RenderWare files
    return None, "N/A", (file_type, "Non-RW")

def detect_col_version(data: bytes) -> Tuple[Optional[int], str, Tuple[str, str]]: #vers 1
    """Detect COL file version"""
    if len(data) < 4:
        return None, "Unknown", ("COL", "Unknown")
    
    # COL files start with version identifier
    version_header = data[:4]
    
    if version_header == b'COL1':
        return None, "COL1 (GTA III/VC)", ("COL", "COL1")
    elif version_header == b'COL2':
        return None, "COL2 (GTA SA)", ("COL", "COL2")
    elif version_header == b'COL3':
        return None, "COL3 (GTA SA Advanced)", ("COL", "COL3")
    elif version_header == b'COL4':
        return None, "COL4 (Extended)", ("COL", "COL4")
    else:
        return None, "Unknown COL", ("COL", "Unknown")

def get_file_type_from_name(filename: str) -> str: #vers 1
    """Get file type from filename extension"""
    if '.' in filename:
        return filename.split('.')[-1].upper()
    return "UNKNOWN"

def analyze_entry_rw_version_working(entry, img_file): #vers 1
    """Analyze RW version for single entry using comprehensive RW database"""
    # If entry has data in memory (new entry)
    if hasattr(entry, 'data') and entry.data:
        version_value, version_name, format_info = detect_rw_version_from_data(entry.data, entry.name)
        
        # Set RW attributes
        entry._rw_version = version_value
        entry._rw_version_name = version_name
        entry._format_info = format_info
        
        return True
    
    # Read from file for existing entries
    elif img_file and hasattr(entry, 'actual_offset') and hasattr(entry, 'actual_size'):
        with open(img_file.file_path, 'rb') as f:
            f.seek(entry.actual_offset)
            header_data = f.read(min(64, entry.actual_size))
            
            version_value, version_name, format_info = detect_rw_version_from_data(header_data, entry.name)
            
            # Set RW attributes
            entry._rw_version = version_value
            entry._rw_version_name = version_name
            entry._format_info = format_info
            
            return True
    
    # Set defaults if detection failed
    entry._rw_version = None
    entry._rw_version_name = "N/A"
    entry._format_info = (get_file_type_from_name(entry.name), "Unknown")
    
    return False

def analyze_all_entries_rw_versions_working(img_file): #vers 1
    """Analyze RW versions for all entries using comprehensive RW database"""
    if not hasattr(img_file, 'entries') or not img_file.entries:
        return 0
    
    analyzed_count = 0
    
    for entry in img_file.entries:
        if analyze_entry_rw_version_working(entry, img_file):
            analyzed_count += 1
    
    return analyzed_count

def ensure_entry_has_rw_data(entry, img_file=None): #vers 1
    """Ensure entry has RW version data - FIXES MISSING RW COLUMNS"""
    # Check if entry already has RW data
    if hasattr(entry, '_rw_version_name') and entry._rw_version_name and entry._rw_version_name != "Unknown":
        return True
    
    # Analyze the entry
    return analyze_entry_rw_version_working(entry, img_file)

def integrate_rw_detection_working(main_window) -> bool: #vers 1
    """Integrate RW detection methods using comprehensive RW version database"""
    # Add working detection methods
    main_window.detect_rw_version_from_data = detect_rw_version_from_data
    main_window.analyze_entry_rw_version_working = lambda entry, img_file: analyze_entry_rw_version_working(entry, img_file)
    main_window.analyze_all_entries_rw_versions_working = lambda img_file: analyze_all_entries_rw_versions_working(img_file)
    main_window.ensure_entry_has_rw_data = lambda entry, img_file=None: ensure_entry_has_rw_data(entry, img_file)
    
    # Add RW version functions from apps.core.module
    main_window.get_file_type_from_name = get_file_type_from_name
    main_window.get_rw_version_name = get_rw_version_name
    main_window.is_valid_rw_version = is_valid_rw_version
    main_window.parse_rw_version = parse_rw_version
    main_window.get_model_format_version = get_model_format_version
    
    if hasattr(main_window, 'log_message'):
        main_window.log_message("RW detection methods integrated with comprehensive database")
        main_window.log_message("   • Uses methods.rw_versions.py comprehensive data")
        main_window.log_message("   • Fixes RW Address/Version detection")
        main_window.log_message("   • Handles DFF, TXD, COL file detection")
        main_window.log_message("   • Supports new entries with in-memory data")
    
    return True

# Export functions
__all__ = [
    'detect_rw_version_from_data',
    'analyze_entry_rw_version_working',
    'analyze_all_entries_rw_versions_working', 
    'ensure_entry_has_rw_data',
    'integrate_rw_detection_working'
]