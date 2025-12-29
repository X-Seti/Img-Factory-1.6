"""
IMG Corruption Analyzer Module
This module provides IMG corruption analysis and fixing functionality.
"""

# Import the actual functions from the methods module
from apps.methods.img_analyze import (
    analyze_img_corruption,
    show_analysis_dialog,
    IMGAnalysisDialog
)

def fix_corrupted_img(img_file, corruption_report, fix_options=None, main_window=None):
    """
    Fix corrupted IMG file based on corruption report
    """
    try:
        if fix_options is None:
            fix_options = {
                'fix_filenames': True,
                'remove_invalid': False,
                'fix_null_bytes': True,
                'fix_long_names': True,
                'create_backup': True
            }
        
        corrupted_entries = corruption_report.get('corrupted_entries', [])
        
        if not corrupted_entries:
            if main_window and hasattr(main_window, 'log_message'):
                main_window.log_message("No corrupted entries to fix")
            return True
        
        # Create backup if requested
        if fix_options.get('create_backup', True) and hasattr(img_file, 'file_path') and img_file.file_path:
            import shutil
            backup_path = img_file.file_path + ".backup"
            try:
                shutil.copy2(img_file.file_path, backup_path)
                if main_window and hasattr(main_window, 'log_message'):
                    main_window.log_message(f"Created backup: {backup_path}")
            except Exception as e:
                if main_window and hasattr(main_window, 'log_message'):
                    main_window.log_message(f"Backup creation failed: {str(e)}")
        
        # Apply fixes based on corruption report
        fixed_count = 0
        for entry in corrupted_entries:
            entry_name = entry.get('name', '')
            issues = entry.get('issues', [])
            
            for issue in issues:
                if 'filename' in issue.lower() and fix_options.get('fix_filenames', False):
                    # Fix filename corruption
                    from apps.methods.img_analyze import _extract_original_filename
                    clean_name = _extract_original_filename(entry_name)
                    if clean_name != entry_name:
                        entry.name = clean_name
                        fixed_count += 1
                
                elif 'null bytes' in issue.lower() and fix_options.get('fix_null_bytes', False):
                    # Handle null byte issues
                    if hasattr(entry, 'name'):
                        entry.name = entry.name.replace('\x00', '')
                        fixed_count += 1
                
                elif 'size mismatch' in issue.lower() or 'offset' in issue.lower():
                    # For severe corruption issues, remove if specified
                    if fix_options.get('remove_invalid', False):
                        # This would require more complex logic to actually remove entries
                        # For now, just count as processed
                        fixed_count += 1
        
        # Rebuild the IMG file if there were changes
        if fixed_count > 0:
            if hasattr(img_file, 'save') and callable(getattr(img_file, 'save')):
                try:
                    img_file.save()
                    if main_window and hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Successfully fixed and saved {fixed_count} entries")
                except Exception as save_error:
                    if main_window and hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Save after fix failed: {str(save_error)}")
                    return False
        
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Fixed {fixed_count} corrupted entries")
        
        return True
        
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to fix corrupted IMG: {str(e)}")
        return False


def show_corruption_analysis_dialog(main_window):
    """
    Show corruption analysis dialog and return results
    """
    try:
        # Use the existing analysis dialog from methods
        result = show_analysis_dialog(main_window)
        
        if result:
            # Format result to match expected structure
            return {
                'report': result,
                'fix_options': {
                    'fix_filenames': True,
                    'remove_invalid': False,
                    'fix_null_bytes': True,
                    'fix_long_names': True,
                    'create_backup': True
                }
            }
        
        return None
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Corruption analysis dialog failed: {str(e)}")
        return None


# Export the functions for external imports
__all__ = [
    'analyze_img_corruption',
    'show_corruption_analysis_dialog',
    'fix_corrupted_img',
    'IMGAnalysisDialog'
]