#this belongs in methods/ img_operations_shared.py - Version: 2
# X-Seti - August26 2025 - IMG Factory 1.5 - Shared IMG Operations

import os
import struct
import tempfile
import shutil
from pathlib import Path
from typing import Optional, Callable, List, Tuple, Dict, Any
from PyQt6.QtWidgets import QApplication


##Methods list -
# create_progress_callback
# sanitize_filename
# get_img_version_info
# calculate_sector_aligned_size
# create_temp_file_path
# atomic_file_replace
# validate_img_structure
# get_entry_data_safely
# write_img_header
# write_img_directory
# consolidate_img_data
# cleanup_temp_files
# log_operation_progress

def create_progress_callback(main_window, operation_name: str) -> Callable:
    """Create unified progress callback for IMG operations"""
    def progress_callback(percent: int, message: str = ""):
        try:
            full_message = f"{operation_name}: {message}" if message else operation_name
            
            if hasattr(main_window, 'log_message') and message:
                main_window.log_message(f"Msg: {full_message} ({percent}%)")
            
            # Update progress bar if available
            try:
                from apps.methods.progressbar_functions import update_progress
                update_progress(main_window, percent, full_message)
            except ImportError:
                # Fallback to basic status update
                if hasattr(main_window, 'statusBar'):
                    main_window.statusBar().showMessage(f"{full_message} - {percent}%")
            
            # Keep UI responsive
            QApplication.processEvents()
            
        except Exception:
            pass  # Ignore progress callback errors
    
    return progress_callback


def sanitize_filename(filename: str) -> str:
    """Clean filename for IMG entry (24 byte max, null terminated)"""
    try:
        if not filename:
            return "file.dat"
        
        # Remove problematic characters
        clean_name = filename.replace('\x00', '').replace('\xcd', '').replace('\xff', '')
        clean_name = ''.join(c for c in clean_name if 32 <= ord(c) <= 126)
        clean_name = clean_name.replace('\\', '_').replace('/', '_').replace('|', '_')
        clean_name = clean_name.strip()[:23]  # Leave room for null terminator
        
        return clean_name if clean_name else "file.dat"
        
    except Exception:
        return "file.dat"


def get_img_version_info(img_file) -> Dict[str, Any]:
    """Extract IMG version information for native operations"""
    try:
        version_info = {
            'version': 2,  # Default to V2 (GTA SA)
            'header_size': 8,
            'entry_size': 32,
            'magic_bytes': b'VER2'
        }
        
        # Detect version from IMG file
        if hasattr(img_file, 'version'):
            if img_file.version == 1:
                version_info.update({
                    'version': 1,
                    'header_size': 0,  # V1 has no header
                    'magic_bytes': b''
                })
            elif img_file.version == 3:
                version_info.update({
                    'version': 3,
                    'header_size': 12,  # V3 extended header
                    'magic_bytes': b'VER3'
                })
        
        return version_info
        
    except Exception:
        # Return safe V2 defaults
        return {
            'version': 2,
            'header_size': 8, 
            'entry_size': 32,
            'magic_bytes': b'VER2'
        }


def calculate_sector_aligned_size(size_bytes: int) -> int:
    """Calculate size aligned to 2048-byte sectors"""
    SECTOR_SIZE = 2048
    return (size_bytes + SECTOR_SIZE - 1) // SECTOR_SIZE


def create_temp_file_path(original_path: str, operation: str = "temp") -> str:
    """Create safe temporary file path for atomic operations"""
    try:
        base_path = Path(original_path)
        parent_dir = base_path.parent
        filename = base_path.stem
        extension = base_path.suffix
        
        # Create unique temp name
        temp_name = f"{filename}_{operation}.tmp{extension}"
        temp_path = parent_dir / temp_name
        
        return str(temp_path)
        
    except Exception:
        # Fallback to simple approach
        return f"{original_path}.{operation}.tmp"


def atomic_file_replace(temp_path: str, target_path: str, main_window=None) -> bool:
    """Perform atomic file replacement (prevents corruption)"""
    try:
        if not os.path.exists(temp_path):
            if main_window and hasattr(main_window, 'log_message'):
                main_window.log_message("Temporary file not found for atomic replace")
            return False
        
        # Create backup if target exists
        backup_path = None
        if os.path.exists(target_path):
            backup_path = f"{target_path}.backup"
            shutil.copy2(target_path, backup_path)
            if main_window and hasattr(main_window, 'log_message'):
                main_window.log_message(f"Created backup: {os.path.basename(backup_path)}")
        
        # Atomic replace
        if os.name == 'nt':  # Windows
            # Remove target first on Windows
            if os.path.exists(target_path):
                os.remove(target_path)
            os.rename(temp_path, target_path)
        else:  # Unix-like systems
            os.rename(temp_path, target_path)
        
        # Clean up backup after successful replace
        if backup_path and os.path.exists(backup_path):
            os.remove(backup_path)
        
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message("Atomic file replacement completed")
        
        return True
        
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Atomic replace failed: {str(e)}")
        
        # Restore backup if replace failed
        if backup_path and os.path.exists(backup_path):
            try:
                if os.path.exists(target_path):
                    os.remove(target_path)
                os.rename(backup_path, target_path)
            except:
                pass
        
        return False


def validate_img_structure(img_file, main_window=None) -> Tuple[bool, str]:
    """Validate IMG file structure before operations"""
    try:
        if not hasattr(img_file, 'entries') or not img_file.entries:
            return False, "No entries found in IMG file"
        
        if not hasattr(img_file, 'file_path') or not img_file.file_path:
            return False, "No file path available"
        
        if not os.path.exists(img_file.file_path):
            return False, f"IMG file not found: {img_file.file_path}"
        
        # Check entry validity
        invalid_entries = 0
        for entry in img_file.entries:
            if not hasattr(entry, 'name') or not entry.name:
                invalid_entries += 1
        
        if invalid_entries > 0:
            warning = f"Found {invalid_entries} entries with invalid names"
            if main_window and hasattr(main_window, 'log_message'):
                main_window.log_message(f"Warn: {warning}")
        
        return True, f"IMG structure valid ({len(img_file.entries)} entries)"
        
    except Exception as e:
        return False, f"Structure validation failed: {str(e)}"


def get_entry_data_safely(entry, img_file, main_window=None) -> Optional[bytes]:
    """Safely extract entry data using multiple methods"""
    try:
        # Method 1: Check for cached/new data
        if hasattr(entry, 'data') and entry.data is not None:
            return entry.data
        
        # Method 2: Use get_data method if available
        if hasattr(entry, 'get_data'):
            try:
                return entry.get_data()
            except Exception:
                pass
        
        # Method 3: Read from IMG file directly
        if (hasattr(entry, 'offset') and hasattr(entry, 'size') and 
            hasattr(img_file, 'file_path')):
            try:
                with open(img_file.file_path, 'rb') as f:
                    f.seek(entry.offset)
                    return f.read(entry.size)
            except Exception:
                pass
        
        # Method 4: Try extract_data method
        if hasattr(entry, 'extract_data'):
            try:
                return entry.extract_data()
            except Exception:
                pass
        
        return None
        
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            entry_name = getattr(entry, 'name', 'Unknown')
            main_window.log_message(f"Failed to get data for {entry_name}: {str(e)}")
        return None


def write_img_header(file_handle, version_info: Dict, entry_count: int):
    """Write IMG header based on version"""
    try:
        if version_info['version'] == 1:
            # V1 has no header
            pass
        elif version_info['version'] == 2:
            # V2 header: "VER2" + entry count
            file_handle.write(version_info['magic_bytes'])
            file_handle.write(struct.pack('<I', entry_count))
        elif version_info['version'] == 3:
            # V3 header: "VER3" + entry count + additional data
            file_handle.write(version_info['magic_bytes'])
            file_handle.write(struct.pack('<I', entry_count))
            file_handle.write(struct.pack('<I', 0))  # Additional V3 field
            
    except Exception as e:
        raise Exception(f"Header write failed: {str(e)}")


def write_img_directory(file_handle, entries, version_info: Dict, data_start_offset: int):
    """Write IMG directory section"""
    try:
        current_offset = data_start_offset
        
        for entry in entries:
            # Sanitize filename
            clean_name = sanitize_filename(getattr(entry, 'name', ''))
            
            # Calculate size in sectors
            entry_size = getattr(entry, 'size', 0)
            if entry_size == 0 and hasattr(entry, 'data') and entry.data:
                entry_size = len(entry.data)
            
            size_sectors = calculate_sector_aligned_size(entry_size)
            
            # Write directory entry
            name_bytes = clean_name.encode('ascii', errors='replace')[:24]
            name_bytes = name_bytes.ljust(24, b'\x00')  # Pad to 24 bytes
            
            file_handle.write(struct.pack('<I', current_offset // 2048))  # Offset in sectors
            file_handle.write(struct.pack('<I', size_sectors))  # Size in sectors
            file_handle.write(name_bytes)  # Filename (24 bytes)
            
            current_offset += size_sectors * 2048
            
    except Exception as e:
        raise Exception(f"Directory write failed: {str(e)}")


def consolidate_img_data(file_handle, entries, img_file, data_start_offset: int, progress_callback: Optional[Callable]):
    """Write consolidated entry data to IMG file"""
    try:
        file_handle.seek(data_start_offset)
        total_entries = len(entries)
        
        for i, entry in enumerate(entries):
            # Get entry data
            entry_data = get_entry_data_safely(entry, img_file)
            if entry_data is None:
                entry_name = getattr(entry, 'name', f'Entry_{i}')
                raise Exception(f"Could not get data for entry: {entry_name}")
            
            # Write data
            file_handle.write(entry_data)
            
            # Pad to sector boundary
            data_size = len(entry_data)
            sector_size = 2048
            padding_needed = (sector_size - (data_size % sector_size)) % sector_size
            
            if padding_needed > 0:
                file_handle.write(b'\x00' * padding_needed)
            
            # Update progress
            if progress_callback:
                progress_callback(
                    int((i + 1) * 100 / total_entries),
                    f"Writing entry {i + 1}/{total_entries}"
                )
        
    except Exception as e:
        raise Exception(f"Data consolidation failed: {str(e)}")


def cleanup_temp_files(base_path: str, main_window=None):
    """Clean up temporary files from operations"""
    try:
        base_path_obj = Path(base_path)
        parent_dir = base_path_obj.parent
        base_name = base_path_obj.stem
        
        # Find and remove temp files
        temp_patterns = [
            f"{base_name}_rebuild.tmp*",
            f"{base_name}_temp.tmp*", 
            f"{base_name}.backup*"
        ]
        
        removed_count = 0
        for pattern in temp_patterns:
            for temp_file in parent_dir.glob(pattern):
                try:
                    temp_file.unlink()
                    removed_count += 1
                except:
                    pass
        
        if removed_count > 0 and main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Cleaned up {removed_count} temporary files")
            
    except Exception:
        pass  # Ignore cleanup errors


def log_operation_progress(main_window, operation: str, step: str, details: str = ""):
    """Unified logging for IMG operations"""
    try:
        if hasattr(main_window, 'log_message'):
            message = f"{operation}: {step}"
            if details:
                message += f" - {details}"
            main_window.log_message(message)
    except Exception:
        pass


# Export essential functions
__all__ = [
    'create_progress_callback',
    'sanitize_filename', 
    'get_img_version_info',
    'calculate_sector_aligned_size',
    'create_temp_file_path',
    'atomic_file_replace',
    'validate_img_structure',
    'get_entry_data_safely',
    'write_img_header',
    'write_img_directory', 
    'consolidate_img_data',
    'cleanup_temp_files',
    'log_operation_progress'
]
