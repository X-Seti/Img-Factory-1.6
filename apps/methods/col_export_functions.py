#this belongs in apps/methods/col_export_functions.py - Version: 4
# X-Seti - November16 2025 - IMG Factory 1.5 - COL Export Functions

"""
COL Export Functions - Clean individual COL model export
Exports selected or all COL models as individual .col files (no combining)
Handles COL file parsing, model extraction, and individual file creation
"""

import os
from typing import List
from PyQt6.QtWidgets import QMessageBox, QProgressDialog, QApplication
from PyQt6.QtCore import Qt

from apps.methods.export_shared import get_export_folder
from apps.methods.export_overwrite_check import handle_overwrite_check

##Methods list -
# export_col_selected
# export_col_all
# _export_col_models
# _get_selected_col_models
# _create_single_col_file
# integrate_col_export_functions

def export_col_selected(main_window, col_file) -> bool: #vers 1
    """Export selected COL models as individual .col files
    
    Args:
        main_window: Main application window
        col_file: COL file object
        
    Returns:
        True if export successful, False otherwise
    """
    try:
        # Get selected models
        selected_models = _get_selected_col_models(main_window, col_file)
        
        if not selected_models:
            QMessageBox.information(main_window, "No Selection", 
                "Please select COL models to export")
            return False
        
        # Choose export directory
        export_dir = get_export_folder(main_window, 
            f"Export {len(selected_models)} Selected COL Models")
        if not export_dir:
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Exporting {len(selected_models)} COL models to: {export_dir}")
        
        # Export models
        return _export_col_models(main_window, col_file, selected_models, 
            export_dir, "selected")
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export COL selected error: {str(e)}")
        QMessageBox.critical(main_window, "Export Error", 
            f"Export failed: {str(e)}")
        return False


def export_col_all(main_window, col_file) -> bool: #vers 2
    """Export all COL models as individual .col files
    
    Args:
        main_window: Main application window
        col_file: COL file object
        
    Returns:
        True if export successful, False otherwise
    """
    try:
        # Get all models
        all_models = getattr(col_file, 'models', [])
        
        if not all_models:
            QMessageBox.information(main_window, "No Models", 
                "No COL models found in file")
            return False
        
        # Choose export directory
        export_dir = get_export_folder(main_window, 
            f"Export All {len(all_models)} COL Models")
        if not export_dir:
            return False
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Exporting all {len(all_models)} COL models to: {export_dir}")
        
        # Export models
        return _export_col_models(main_window, col_file, all_models, 
            export_dir, "all")
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Export COL all error: {str(e)}")
        QMessageBox.critical(main_window, "Export Error", 
            f"Export failed: {str(e)}")
        return False


def _export_col_models(main_window, col_file, models: List, export_dir: str, 
                       operation_name: str) -> bool: #vers 1
    """Export COL models as individual .col files
    
    Args:
        main_window: Main application window
        col_file: COL file object
        models: List of COL models to export
        export_dir: Export destination directory
        operation_name: Operation name for logging
        
    Returns:
        True if export successful, False otherwise
    """
    try:
        # Create model entries for overwrite check
        model_entries = []
        for i, model in enumerate(models):
            model_name = getattr(model, 'name', f'model_{i}.col')
            if not model_name.endswith('.col'):
                model_name += '.col'
            
            # Create pseudo-entry for overwrite check
            class ModelEntry:
                def __init__(self, name):
                    self.name = name
            
            model_entries.append(ModelEntry(model_name))
        
        # Overwrite check
        export_options = {'organize_by_type': False, 'overwrite': True}
        filtered_entries, should_continue = handle_overwrite_check(
            main_window, model_entries, export_dir, export_options, 
            f"export {operation_name} COL models"
        )
        
        if not should_continue:
            return False
        
        # Filter models based on overwrite check results
        filtered_names = {entry.name for entry in filtered_entries}
        filtered_models = []
        for i, model in enumerate(models):
            model_name = getattr(model, 'name', f'model_{i}.col')
            if not model_name.endswith('.col'):
                model_name += '.col'
            if model_name in filtered_names:
                filtered_models.append(model)
        
        models = filtered_models
        
        # Create progress dialog
        progress = QProgressDialog(
            f"Exporting {operation_name} COL models...", 
            "Cancel", 0, len(models), main_window
        )
        progress.setWindowModality(Qt.WindowModality.WindowModal)
        progress.setMinimumDuration(0)
        progress.show()
        QApplication.processEvents()
        
        # Export each model individually
        success_count = 0
        failed_count = 0
        
        for i, model in enumerate(models):
            if progress.wasCanceled():
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("Export cancelled by user")
                break
            
            model_name = getattr(model, 'name', f'model_{i}.col')
            if not model_name.endswith('.col'):
                model_name += '.col'
            
            progress.setValue(i)
            progress.setLabelText(f"Exporting: {model_name}")
            QApplication.processEvents()
            
            try:
                output_path = os.path.join(export_dir, model_name)
                
                # Create individual COL file with single model
                if _create_single_col_file(col_file, model, output_path):
                    success_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Exported: {model_name}")
                else:
                    failed_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"Failed: {model_name}")
                    
            except Exception as e:
                failed_count += 1
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"Error exporting {model_name}: {str(e)}")
        
        progress.setValue(len(models))
        
        # Show summary
        if success_count > 0:
            summary = f"Exported {success_count} COL model(s)"
            if failed_count > 0:
                summary += f", {failed_count} failed"
            
            QMessageBox.information(main_window, "Export Complete", summary)
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"COL export complete: {summary}")
            
            return True
        else:
            QMessageBox.warning(main_window, "Export Failed", 
                "No COL models were exported successfully")
            return False
            
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"COL export error: {str(e)}")
        QMessageBox.critical(main_window, "Export Error", 
            f"Export failed: {str(e)}")
        return False


def _get_selected_col_models(main_window, col_file) -> List: #vers 1
    """Get selected COL models from current tab's table
    
    Args:
        main_window: Main application window
        col_file: COL file object
        
    Returns:
        List of selected COL model objects
    """
    try:
        selected_models = []
        
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
                main_window.log_message("No table found for model selection")
            return selected_models
        
        # Get selected rows
        selected_rows = set()
        for item in table.selectedItems():
            selected_rows.add(item.row())
        
        # Get models for selected rows
        if hasattr(col_file, 'models'):
            for row in sorted(selected_rows):
                if row < len(col_file.models):
                    selected_models.append(col_file.models[row])
        
        return selected_models
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error getting selected models: {str(e)}")
        return []


def _create_single_col_file(col_file, model, output_path: str) -> bool: #vers 2
    """Write one model as its own COL file; keeps original bytes if unedited."""
    try:
        from apps.methods.col_workshop_parser import COLWriter
        from apps.methods.col_splice import model_record
        with open(output_path, 'wb') as f:
            f.write(model_record(model, COLWriter, getattr(model, 'name', 'model')))
        return True
    except Exception:
        return False


def integrate_col_export_functions(main_window) -> bool: #vers 2
    """Integrate COL export functions into main window
    
    Args:
        main_window: Main application window
        
    Returns:
        True if integration successful
    """
    try:
        # Add export methods
        main_window.export_col_selected = lambda col_file: export_col_selected(main_window, col_file)
        main_window.export_col_all = lambda col_file: export_col_all(main_window, col_file)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("COL export functions integrated")
            main_window.log_message("   - Individual COL file export only")
            main_window.log_message("   - Supports COL2/COL3 formats")
            main_window.log_message("   - Overwrite checking support")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"COL export integration failed: {str(e)}")
        return False


# Export functions
__all__ = [
    'export_col_selected',
    'export_col_all',
    'integrate_col_export_functions'
]
