#this belongs in methods/ col_export_functions.py - Version: 1
# X-Seti - November16 2025 - IMG Factory 1.5 - COL Export Functions

"""
COL Export Functions - Clean individual COL model export
Exports selected or all COL models as individual .col files (no combining)
Handles COL file parsing, model extraction, and individual file creation
"""

import os
import struct
from typing import List, Optional, Tuple
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
# _write_col_header
# _write_col_model
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


def export_col_all(main_window, col_file) -> bool: #vers 1
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
                        main_window.log_message(f"✓ Exported: {model_name}")
                else:
                    failed_count += 1
                    if hasattr(main_window, 'log_message'):
                        main_window.log_message(f"✗ Failed: {model_name}")
                    
            except Exception as e:
                failed_count += 1
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"✗ Error exporting {model_name}: {str(e)}")
        
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


def _create_single_col_file(col_file, model, output_path: str) -> bool: #vers 1
    """Create a single-model COL file
    
    Args:
        col_file: Source COL file object
        model: Single COL model to export
        output_path: Output file path
        
    Returns:
        True if successful, False otherwise
    """
    try:
        with open(output_path, 'wb') as f:
            # Write COL header for single model file
            _write_col_header(f, col_file, 1)  # 1 model count
            
            # Write the single model
            _write_col_model(f, model)
        
        return True
        
    except Exception as e:
        return False


def _write_col_header(f, col_file, model_count: int): #vers 1
    """Write COL file header
    
    Args:
        f: File object
        col_file: Source COL file object
        model_count: Number of models in file
    """
    # COL file format: "COLL" signature + version + model count
    f.write(b'COLL')
    
    # Get version from source file or use default
    version = getattr(col_file, 'version', 2)  # Default to COL2
    f.write(struct.pack('<I', version))
    
    # Write model count
    f.write(struct.pack('<I', model_count))


def _write_col_model(f, model): #vers 1
    """Write COL model data to file
    
    Args:
        f: File object
        model: COL model object
    """
    # Get raw model data if available
    if hasattr(model, 'raw_data') and model.raw_data:
        f.write(model.raw_data)
        return
    
    # Otherwise, construct from model components
    # Model name (24 bytes, null-padded)
    model_name = getattr(model, 'name', 'model')[:24].encode('ascii')
    model_name = model_name.ljust(24, b'\x00')
    f.write(model_name)
    
    # Model ID
    model_id = getattr(model, 'model_id', 0)
    f.write(struct.pack('<I', model_id))
    
    # Bounding box
    bbox = getattr(model, 'bounding_box', None)
    if bbox:
        # Min/Max coordinates
        f.write(struct.pack('<fff', bbox.min.x, bbox.min.y, bbox.min.z))
        f.write(struct.pack('<fff', bbox.max.x, bbox.max.y, bbox.max.z))
        # Bounding sphere center and radius
        f.write(struct.pack('<fff', bbox.center.x, bbox.center.y, bbox.center.z))
        f.write(struct.pack('<f', bbox.radius))
    else:
        # Default bounding box/sphere
        f.write(struct.pack('<fff', -1.0, -1.0, -1.0))  # min
        f.write(struct.pack('<fff', 1.0, 1.0, 1.0))     # max
        f.write(struct.pack('<fff', 0.0, 0.0, 0.0))     # center
        f.write(struct.pack('<f', 1.0))                  # radius
    
    # Face data
    faces = getattr(model, 'faces', [])
    f.write(struct.pack('<I', len(faces)))
    
    for face in faces:
        # Face surface type and material
        surface = getattr(face, 'surface', 0)
        material = getattr(face, 'material', 0)
        f.write(struct.pack('<BB', surface, material))
        
        # Triangle vertices (3 vertices, each with x,y,z)
        if hasattr(face, 'vertices') and len(face.vertices) >= 3:
            for i in range(3):
                v = face.vertices[i]
                f.write(struct.pack('<fff', v.x, v.y, v.z))
        else:
            # Default triangle
            for i in range(3):
                f.write(struct.pack('<fff', 0.0, 0.0, 0.0))


def integrate_col_export_functions(main_window) -> bool: #vers 1
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
            main_window.log_message("   • Individual COL file export only")
            main_window.log_message("   • Supports COL2/COL3 formats")
            main_window.log_message("   • Overwrite checking support")
        
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
