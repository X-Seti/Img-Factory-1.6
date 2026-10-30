#this belongs in methods/img_export_entry.py - Version: 3
# X-Seti - November19 2025 - IMG Factory 1.5 - IMG Export Entry Helper
"""
IMG Export Entry Helper - Single entry export function
FIXED: Uses correct entry.offset and entry.size (not actual_offset/actual_size)
"""

import os
from typing import Optional

##Methods list -
# export_entry

def export_entry(img_archive, entry, output_path: Optional[str] = None, output_dir: Optional[str] = None) -> str: #vers 2
    """Export an entry from an IMG archive to a file
    Args:
        img_archive: IMG archive object
        entry: Entry object to export
        output_path: Optional full output path (if not provided, uses output_dir + entry name)
        output_dir: Optional output directory (if output_path not provided)
    Returns:
        Output path if successful, None if failed
    """
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        if not output_path and not output_dir:
            raise ValueError("Either output_path or output_dir must be provided")
        if output_dir:
            os.makedirs(output_dir, exist_ok=True)
            output_path = os.path.join(output_dir, entry.name)
        # Check if entry has in-memory data (new/modified entries)
        if hasattr(entry, 'is_new_entry') and entry.is_new_entry and hasattr(entry, 'data') and entry.data:
            if img_debugger:
                img_debugger.debug(f"Exporting new/modified entry from memory: {entry.name}")
            data_to_write = entry.data
        elif hasattr(entry, 'data') and entry.data:
            data_to_write = entry.data
        else:
            # Use img_archive.read_entry_data() which handles DIR+IMG path
            # resolution, Xbox LZO decompression, and all format variants
            if hasattr(img_archive, 'read_entry_data'):
                if img_debugger:
                    img_debugger.debug(f"Reading via read_entry_data: {entry.name}")
                data_to_write = img_archive.read_entry_data(entry)
                if data_to_write is None:
                    raise ValueError(f"read_entry_data returned None for {entry.name}")
            else:
                # Legacy fallback: direct read (only correct for single-file formats)
                if img_debugger:
                    img_debugger.debug(f"Legacy direct read: {entry.name}")
                with open(img_archive.file_path, 'rb') as f:
                    f.seek(entry.offset)
                    data_to_write = f.read(entry.size)
        # Write data to output file
        with open(output_path, 'wb') as f:
            f.write(data_to_write)
        if img_debugger:
            img_debugger.success(f"Exported entry: {entry.name} to {output_path} ({len(data_to_write)} bytes)")
        return output_path
    except Exception as e:
        try:
            from apps.debug.debug_functions import img_debugger
            img_debugger.error(f"Failed to export entry {entry.name}: {str(e)}")
        except:
            print(f"[ERROR] export_entry failed: {e}")
        return None  # ✅ Return None on failure, don't raise

# Export functions
__all__ = ['export_entry']
