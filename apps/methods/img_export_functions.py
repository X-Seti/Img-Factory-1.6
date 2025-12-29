#this belongs in methods/ img_export_functions.py - Version: 1
# X-Seti - November16 2025 - IMG Factory 1.5 - IMG Export Functions

"""
IMG Export Functions - Clean individual file export for IMG archives
Exports selected or all entries as individual files (no combining)
Uses img_import_export.py's export_entry() for actual export operations
"""

import os
from typing import List, Optional, Tuple
from PyQt6.QtWidgets import QMessageBox, QProgressDialog, QApplication
from PyQt6.QtCore import Qt

from apps.methods.img_export_entry import export_entry
from apps.methods.export_shared import get_export_folder
from apps.methods.export_overwrite_check import handle_overwrite_check

##Methods list -
# export_img_selected
# export_img_all
# _export_img_entries
# _get_selected_img_entries
# integrate_img_export_functions

def export_img_selected(main_window, img_file) -> bool: #vers 1
    """Export selected IMG entries as individual files
    
    Args:
        main_window: Main application window
        img_file: IMG file object
        
    Returns:
        True if export successful, False otherwise
    """
    try:
        # Get selected entries
        selected_entries = _get_selected_img_entries(main_window, img_file)
        
        if not selected_entries:
            QMessageBox.information(main_window, "No Selection", 
                "Please select entries to export")
            return False
        
        # Choose export directory
        export_dir = get_export_folder(main_window, 
            f"Export {len(selected_entries)} Selected Entries")
        if not export_dir:
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Exporting {len(selected_entries)} IMG entries to: {export_dir}")
        
        # Export options - individual files only
        export_options = {
            'organize_by_type': False,
            'overwrite': True
        }
        
        # Export entries
        return _export_img_entries(main_window, img_file, selected_entries, 
            export_dir, export_options, "selected")
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export IMG selected error: {str(e)}")
        QMessageBox.critical(main_window, "Export Error", 
            f"Export failed: {str(e)}")
        return False


def export_img_all(main_window, img_file) -> bool: #vers 1
    """Export all IMG entries as individual files
    
    Args:
        main_window: Main application window
        img_file: IMG file object
        
    Returns:
        True if export successful, False otherwise
    """
    try:
        # Get all entries
        all_entries = getattr(img_file, 'entries', [])
        
        if not all_entries:
            QMessageBox.information(main_window, "No Entries", 
                "No entries found in IMG file")
            return False
        
        # Choose export directory
        export_dir = get_export_folder(main_window, 
            f"Export All {len(all_entries)} Entries")
        if not export_dir:
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Exporting all {len(all_entries)} IMG entries to: {export_dir}")
        
        # Export options - individual files only
        export_options = {
            'organize_by_type': False,
            'overwrite': True
        }
        
        # Export entries
        return _export_img_entries(main_window, img_file, all_entries, 
            export_dir, export_options, "all")
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export IMG all error: {str(e)}")
        QMessageBox.critical(main_window, "Export Error", 
            f"Export failed: {str(e)}")
        return False


def _export_img_entries(main_window, img_file, entries: List, export_dir: str, 
                        export_options: dict, operation_name: str) -> bool: #vers 1
    """Export IMG entries as individual files with overwrite check
    
    Args:
        main_window: Main application window
        img_file: IMG file object
        entries: List of entries to export
        export_dir: Export destination directory
        export_options: Export options dict
        operation_name: Operation name for logging
        
    Returns:
        True if export successful, False otherwise
    """
    try:
        # Overwrite check
        filtered_entries, should_continue = handle_overwrite_check(
            main_window, entries, export_dir, export_options, 
            f"export {operation_name}"
        )
        
        if not should_continue:
            return False
        
        entries = filtered_entries
        
        # Create progress dialog
        progress = QProgressDialog(
            f"Exporting {operation_name} entries...", 
            "Cancel", 0, len(entries), main_window
        )
        progress.setWindowModality(Qt.WindowModality.WindowModal)
        progress.setMinimumDuration(0)
        progress.show()
        QApplication.processEvents()
        
        # Export each entry individually
        success_count = 0
        failed_count = 0
        
        for i, entry in enumerate(entries):
            if progress.wasCanceled():
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("Export cancelled by user")
                break
            
            progress.setValue(i)
            progress.setLabelText(f"Exporting: {entry.name}")
            QApplication.processEvents()
            
            try:
                # Use img_import_export.py's export_entry function
                output_path = export_entry(img_file, entry, output_path=os.path.join(export_dir, entry.name))
                
                if output_path:
                    success_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"✓ Exported: {entry.name}")
                else:
                    failed_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"✗ Failed: {entry.name}")
                    
            except Exception as e:
                failed_count += 1
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"✗ Error exporting {entry.name}: {str(e)}")
        
        progress.setValue(len(entries))
        
        # Show summary
        if success_count > 0:
            summary = f"Exported {success_count} file(s)"
            if failed_count > 0:
                summary += f", {failed_count} failed"
            
            QMessageBox.information(main_window, "Export Complete", summary)
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Export complete: {summary}")
            
            return True
        else:
            QMessageBox.warning(main_window, "Export Failed", 
                "No files were exported successfully")
            return False
            
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export error: {str(e)}")
        QMessageBox.critical(main_window, "Export Error", 
            f"Export failed: {str(e)}")
        return False


def _get_selected_img_entries(main_window, img_file) -> List: #vers 1
    """Get selected entries from current tab's table
    
    Args:
        main_window: Main application window
        img_file: IMG file object
        
    Returns:
        List of selected entry objects
    """
    try:
        selected_entries = []
        
        # Try multiple methods to get the table
        table = None
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
        elif hasattr(main_window, 'entries_table'):
            table = main_window.entries_table
        elif hasattr(main_window, 'table'):
            table = main_window.table
        
        if not table:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("No table found for entry selection")
            return selected_entries
        
        # Get selected rows
        selected_rows = set()
        for item in table.selectedItems():
            selected_rows.add(item.row())
        
        # Get entries for selected rows
        if hasattr(img_file, 'entries'):
            for row in sorted(selected_rows):
                if row < len(img_file.entries):
                    selected_entries.append(img_file.entries[row])
        
        return selected_entries
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error getting selected entries: {str(e)}")
        return []


def integrate_img_export_functions(main_window) -> bool: #vers 1
    """Integrate IMG export functions into main window
    
    Args:
        main_window: Main application window
        
    Returns:
        True if integration successful
    """
    try:
        # Add export methods
        main_window.export_img_selected = lambda img_file: export_img_selected(main_window, img_file)
        main_window.export_img_all = lambda img_file: export_img_all(main_window, img_file)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("IMG export functions integrated")
            main_window.log_message("   • Individual file export only")
            main_window.log_message("   • Uses img_import_export.export_entry()")
            main_window.log_message("   • Overwrite checking support")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"IMG export integration failed: {str(e)}")
        return False


# Export functions
__all__ = [
    'export_img_selected',
    'export_img_all',
    'integrate_img_export_functions'
]
