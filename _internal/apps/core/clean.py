#this belongs in core/clean.py - Version: 2
# X-Seti - August09 2025 - IMG Factory 1.5 - Clean Utility Functions

"""
IMG Factory Clean Utilities
Contains only utility functions for IMG Factory operations
All rebuild functions moved to core/rebuild.py and core/rebuild_all.py
"""

import os
import shutil
from typing import Optional, Dict, Any, List, Tuple

##Methods list -
# cleanup_temp_files
# create_backup_file  
# format_file_size
# get_file_extension
# integrate_clean_utilities
# sanitize_path
# validate_img_file_path

def format_file_size(size_bytes: int) -> str: #vers 1
    """Format file size in human readable format"""
    try:
        if size_bytes < 1024:
            return f"{size_bytes} B"
        elif size_bytes < 1024 * 1024:
            return f"{size_bytes / 1024:.1f} KB"
        else:
            return f"{size_bytes / (1024 * 1024):.1f} MB"
    except Exception:
        return "Unknown"


def create_backup_file(file_path: str, backup_suffix: str = ".backup") -> bool: #vers 1
    """Create backup of file with specified suffix"""
    try:
        if not os.path.exists(file_path):
            return False
            
        backup_path = f"{file_path}{backup_suffix}"
        
        # Don't overwrite existing backup
        if os.path.exists(backup_path):
            return True
            
        shutil.copy2(file_path, backup_path)
        return os.path.exists(backup_path)
        
    except Exception as e:
        print(f"Backup creation failed: {e}")
        return False


def cleanup_temp_files(directory: str, pattern: str = "*.tmp") -> int: #vers 1
    """Clean up temporary files in directory"""
    try:
        import glob
        
        temp_files = glob.glob(os.path.join(directory, pattern))
        cleaned_count = 0
        
        for temp_file in temp_files:
            try:
                os.remove(temp_file)
                cleaned_count += 1
            except Exception:
                continue
                
        return cleaned_count
        
    except Exception:
        return 0


def sanitize_path(file_path: str) -> str: #vers 1
    """Sanitize file path for safe operations"""
    try:
        if not file_path:
            return ""
            
        # Normalize path separators
        clean_path = os.path.normpath(file_path)
        
        # Remove problematic characters
        clean_path = clean_path.replace('..', '')
        
        return clean_path
        
    except Exception:
        return ""


def get_file_extension(file_path: str) -> str: #vers 1
    """Get file extension in lowercase"""
    try:
        if not file_path:
            return ""
            
        return os.path.splitext(file_path)[1].lower()
        
    except Exception:
        return ""


def validate_img_file_path(file_path: str) -> bool: #vers 1
    """Validate that file path is a valid IMG file"""
    try:
        if not file_path or not os.path.exists(file_path):
            return False
            
        extension = get_file_extension(file_path)
        valid_extensions = ['.img', '.dir']
        
        if extension not in valid_extensions:
            return False
            
        # Check file is not empty
        if os.path.getsize(file_path) < 32:
            return False
            
        return True
        
    except Exception:
        return False


def get_directory_img_files(directory: str) -> List[str]: #vers 1
    """Get all IMG files in directory"""
    try:
        if not os.path.exists(directory):
            return []
            
        import glob
        
        img_files = []
        img_files.extend(glob.glob(os.path.join(directory, "*.img")))
        img_files.extend(glob.glob(os.path.join(directory, "*.dir")))
        
        # Filter valid files only
        valid_files = []
        for img_file in img_files:
            if validate_img_file_path(img_file):
                valid_files.append(img_file)
                
        return sorted(valid_files)
        
    except Exception:
        return []


def calculate_directory_stats(directory: str) -> Dict[str, Any]: #vers 1
    """Calculate statistics for IMG files in directory"""
    try:
        img_files = get_directory_img_files(directory)
        
        if not img_files:
            return {'error': 'No valid IMG files found'}
            
        total_files = len(img_files)
        total_size = 0
        file_sizes = []
        
        for img_file in img_files:
            try:
                size = os.path.getsize(img_file)
                total_size += size
                file_sizes.append(size)
            except Exception:
                continue
                
        return {
            'total_files': total_files,
            'total_size': total_size,
            'total_size_formatted': format_file_size(total_size),
            'average_size': total_size // total_files if total_files > 0 else 0,
            'largest_file_size': max(file_sizes) if file_sizes else 0,
            'smallest_file_size': min(file_sizes) if file_sizes else 0
        }
        
    except Exception as e:
        return {'error': str(e)}


def verify_file_integrity(file_path: str) -> Dict[str, Any]: #vers 1
    """Basic file integrity verification"""
    try:
        if not validate_img_file_path(file_path):
            return {'valid': False, 'error': 'Invalid IMG file path'}
            
        file_size = os.path.getsize(file_path)
        
        # Basic header check
        try:
            with open(file_path, 'rb') as f:
                header = f.read(32)
                
                if len(header) < 32:
                    return {'valid': False, 'error': 'File too small'}
                    
                # Very basic structure check
                has_readable_data = any(b > 0 for b in header)
                
                return {
                    'valid': has_readable_data,
                    'file_size': file_size,
                    'file_size_formatted': format_file_size(file_size),
                    'header_length': len(header)
                }
                
        except Exception as e:
            return {'valid': False, 'error': f'Read error: {str(e)}'}
            
    except Exception as e:
        return {'valid': False, 'error': str(e)}


def clean_filename_for_display(filename: str) -> str: #vers 1
    """Clean filename for safe display (no modification for actual files)"""
    try:
        if not filename:
            return "unnamed"
            
        # Remove null bytes and control characters for display only
        clean_name = filename.replace('\x00', '')
        clean_name = ''.join(c for c in clean_name if 32 <= ord(c) <= 126 or c in '\t\n')
        
        # Trim whitespace
        clean_name = clean_name.strip()
        
        if not clean_name:
            return "unnamed"
            
        return clean_name
        
    except Exception:
        return "unnamed"


def get_system_temp_directory() -> str: #vers 1
    """Get system temporary directory for operations"""
    try:
        import tempfile
        return tempfile.gettempdir()
    except Exception:
        return os.getcwd()


def ensure_directory_exists(directory: str) -> bool: #vers 1
    """Ensure directory exists, create if needed"""
    try:
        if not directory:
            return False
            
        if os.path.exists(directory):
            return True
            
        os.makedirs(directory, exist_ok=True)
        return os.path.exists(directory)
        
    except Exception:
        return False


def safe_file_operation(operation_func, *args, **kwargs) -> Tuple[bool, str]: #vers 1
    """Safely execute file operation with error handling"""
    try:
        result = operation_func(*args, **kwargs)
        return True, "Operation successful"
    except PermissionError:
        return False, "Permission denied"
    except FileNotFoundError:
        return False, "File not found"
    except OSError as e:
        return False, f"OS error: {str(e)}"
    except Exception as e:
        return False, f"Unexpected error: {str(e)}"


def integrate_clean_utilities(main_window) -> bool: #vers 1
    """Integrate clean utility functions into main window"""
    try:
        # Add utility functions to main window
        main_window.format_file_size = format_file_size
        main_window.create_backup_file = create_backup_file
        main_window.cleanup_temp_files = cleanup_temp_files
        main_window.sanitize_path = sanitize_path
        main_window.validate_img_file_path = validate_img_file_path
        main_window.get_directory_img_files = get_directory_img_files
        main_window.calculate_directory_stats = calculate_directory_stats
        main_window.verify_file_integrity = verify_file_integrity
        main_window.clean_filename_for_display = clean_filename_for_display
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Clean utilities integrated")
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Clean utilities integration failed: {str(e)}")
        return False


# Export functions
__all__ = [
    'format_file_size',
    'create_backup_file',
    'cleanup_temp_files',
    'sanitize_path',
    'get_file_extension',
    'validate_img_file_path',
    'get_directory_img_files',
    'calculate_directory_stats',
    'verify_file_integrity',
    'clean_filename_for_display',
    'get_system_temp_directory',
    'ensure_directory_exists',
    'safe_file_operation',
    'integrate_clean_utilities'
]
