#this belongs in gui/gui_infobar.py - Version: 4
# X-Seti - July17 2025 - IMG Factory 1.5 - Info Bar Functions
# Enhanced with all COL info bar functions using IMG debug system

"""
Info Bar Functions
Handles info bar updates for both IMG and COL files
CONSOLIDATED: All info bar functions from scattered locations
"""

import os
from typing import Dict, Any, Optional

# Import debug systems
from apps.debug.debug_functions import col_debug_log, is_col_debug_enabled
from apps.debug.debug_functions import img_debugger

##Methods list -
# get_col_file_statistics
# update_col_info_bar
# update_img_info_bar
# update_main_info_display

def get_col_file_statistics(col_file, file_path: str) -> Dict[str, Any]: #vers 1
    """Get comprehensive COL file statistics for info bar display"""
    try:
        col_debug_log(None, "Calculating COL file statistics for info bar", 'COL_INFOBAR')
        
        stats = {
            'file_name': os.path.basename(file_path),
            'file_size': 0,
            'file_size_formatted': 'Unknown',
            'model_count': 0,
            'total_spheres': 0,
            'total_boxes': 0,
            'total_vertices': 0,
            'total_faces': 0,
            'total_elements': 0,
            'format_version': 'Unknown',
            'complexity': 'Unknown'
        }
        
        # Basic file info
        if os.path.exists(file_path):
            stats['file_size'] = os.path.getsize(file_path)
            stats['file_size_formatted'] = format_file_size(stats['file_size'])
        
        # COL-specific stats
        if hasattr(col_file, 'models') and col_file.models:
            stats['model_count'] = len(col_file.models)
            
            # Aggregate collision data from all models
            for model in col_file.models:
                if hasattr(model, 'spheres') and model.spheres:
                    stats['total_spheres'] += len(model.spheres)
                
                if hasattr(model, 'boxes') and model.boxes:
                    stats['total_boxes'] += len(model.boxes)
                
                if hasattr(model, 'vertices') and model.vertices:
                    stats['total_vertices'] += len(model.vertices)
                
                if hasattr(model, 'faces') and model.faces:
                    stats['total_faces'] += len(model.faces)
                
                # Get version from first model
                if stats['format_version'] == 'Unknown' and hasattr(model, 'version'):
                    version = model.version
                    if hasattr(version, 'value'):
                        stats['format_version'] = f"COL{version.value}"
                    else:
                        stats['format_version'] = str(version)
        
        # Calculate total collision elements
        stats['total_elements'] = (stats['total_spheres'] + 
                                 stats['total_boxes'] + 
                                 stats['total_faces'])
        
        # Determine complexity
        if stats['total_elements'] > 1000:
            stats['complexity'] = 'Very Complex'
        elif stats['total_elements'] > 500:
            stats['complexity'] = 'Complex'
        elif stats['total_elements'] > 100:
            stats['complexity'] = 'Moderate'
        elif stats['total_elements'] > 0:
            stats['complexity'] = 'Simple'
        else:
            stats['complexity'] = 'Empty'
        
        if is_col_debug_enabled():
            col_debug_log(None, f"COL stats calculated: {stats['total_elements']} elements", 'COL_INFOBAR')
        
        return stats
        
    except Exception as e:
        col_debug_log(None, f"Error calculating COL statistics: {e}", 'COL_INFOBAR', 'ERROR')
        return stats

def update_col_info_bar(main_window, col_file, file_path: str) -> bool: #vers 1
    """Basic COL info bar update - CONSOLIDATED from apps.core.loadcol.py"""
    try:
        col_debug_log(main_window, "Updating COL info bar (basic)", 'COL_INFOBAR')
        
        stats = get_col_file_statistics(col_file, file_path)
        
        # Update window title
        main_window.setWindowTitle(f"IMG Factory 1.5 - {stats['file_name']} (COL)")
        
        # Update basic info labels if they exist
        if hasattr(main_window, 'file_path_label'):
            main_window.file_path_label.setText(file_path)
        
        if hasattr(main_window, 'version_label'):
            main_window.version_label.setText(stats['format_version'])
        
        if hasattr(main_window, 'entry_count_label'):
            main_window.entry_count_label.setText(str(stats['model_count']))
        
        # Update status bar/progress
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'show_progress'):
            status_text = f"COL: {stats['model_count']} models ({stats['file_size_formatted']})"
            main_window.gui_layout.show_progress(-1, status_text)
        
        col_debug_log(main_window, f"Basic COL info bar updated: {stats['file_name']}", 'COL_INFOBAR', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error updating basic COL info bar: {str(e)}", 'COL_INFOBAR', 'ERROR')
        return False


""" old_ prefix
def old_update_col_info_bar_enhanced(main_window, col_file, file_path: str) -> bool: #vers 1
    try:
        col_debug_log(main_window, "Updating COL info bar (enhanced)", 'COL_INFOBAR')
        
        stats = get_col_file_statistics(col_file, file_path)
        gui_layout = main_window.gui_layout
        
        # Update file count with model information
        if hasattr(gui_layout, 'file_count_label'):
            count_text = f"Models: {stats['model_count']}"
            if stats['total_elements'] > 0:
                count_text += f" ({stats['total_elements']} elements)"
            gui_layout.file_count_label.setText(count_text)
        
        # Update file size with enhanced formatting
        if hasattr(gui_layout, 'file_size_label'):
            gui_layout.file_size_label.setText(f"Size: {stats['file_size_formatted']}")
        
        # Update format version with detailed info
        if hasattr(gui_layout, 'format_version_label'):
            version_text = f"Format: {stats['format_version']}"
            if stats['complexity'] != 'Unknown':
                version_text += f" ({stats['complexity']})"
            gui_layout.format_version_label.setText(version_text)
        
        # Update additional info with collision details
        if hasattr(gui_layout, 'additional_info_label'):
            if stats['total_elements'] > 0:
                collision_breakdown = []
                if stats['total_spheres'] > 0:
                    collision_breakdown.append(f"S:{stats['total_spheres']}")
                if stats['total_boxes'] > 0:
                    collision_breakdown.append(f"B:{stats['total_boxes']}")
                if stats['total_faces'] > 0:
                    collision_breakdown.append(f"M:{stats['total_faces']}")
                
                breakdown_text = ", ".join(collision_breakdown)
                gui_layout.additional_info_label.setText(f"Collision: {breakdown_text}")
            else:
                gui_layout.additional_info_label.setText("Collision: None")
        
        # Update progress/status with enhanced info
        if hasattr(gui_layout, 'show_progress'):
            enhanced_status = f"COL: {stats['model_count']} models, {stats['total_elements']} elements ({stats['file_size_formatted']})"
            gui_layout.show_progress(-1, enhanced_status)
        
        # Update window title with enhanced info
        title = f"IMG Factory 1.5 - {stats['file_name']} (COL)"
        if stats['model_count'] > 1:
            title += f" - {stats['model_count']} Models"
        main_window.setWindowTitle(title)
        
        col_debug_log(main_window, f"Enhanced COL info bar updated successfully", 'COL_INFOBAR', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error updating enhanced COL info bar: {str(e)}", 'COL_INFOBAR', 'ERROR')
        # Fallback to basic update
        return update_col_info_bar(main_window, col_file, file_path)
"""

def update_img_info_bar(main_window, img_file, file_path: str) -> bool: #vers 1
    """Update info bar for IMG files with IMG debug integration"""
    try:
        img_debugger.debug("Updating IMG info bar")
        
        # Update GUI layout info
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'update_img_info'):
            file_name = os.path.basename(file_path)
            main_window.gui_layout.update_img_info(f"IMG: {file_name}")
        
        # Update entry count and file size
        if hasattr(main_window, 'gui_layout'):
            entry_count = len(img_file.entries) if img_file.entries else 0
            file_size = os.path.getsize(file_path) if os.path.exists(file_path) else 0
            file_size_str = format_file_size(file_size)
            
            status_text = f"Loaded: {entry_count} entries ({file_size_str})"
            main_window.gui_layout.show_progress(-1, status_text)
        
        # Update window title
        file_name = os.path.basename(file_path)
        main_window.setWindowTitle(f"IMG Factory 1.5 - {file_name}")
        
        # Update additional labels if available
        if hasattr(main_window, 'file_path_label'):
            main_window.file_path_label.setText(file_path)
        
        if hasattr(main_window, 'entry_count_label'):
            entry_count = len(img_file.entries) if img_file.entries else 0
            main_window.entry_count_label.setText(str(entry_count))
        
        if hasattr(main_window, 'version_label'):
            version = getattr(img_file, 'version', 'Unknown')
            main_window.version_label.setText(str(version))
        
        img_debugger.debug("IMG info bar updated successfully")
        return True
        
    except Exception as e:
        img_debugger.error(f"IMG info bar update failed: {str(e)}")
        return False

def update_main_info_display(main_window, file_type: str, file_object, file_path: str) -> bool: #vers 1
    """Universal info bar update function for any file type"""
    try:
        col_debug_log(main_window, f"Updating main info display for {file_type} file", 'COL_INFOBAR')
        
        if file_type.upper() == 'COL':
            return update_col_info_bar_enhanced(main_window, file_object, file_path)
        elif file_type.upper() == 'IMG':
            return update_img_info_bar(main_window, file_object, file_path)
        else:
            col_debug_log(main_window, f"Unknown file type for info display: {file_type}", 'COL_INFOBAR', 'WARNING')
            return False
            
    except Exception as e:
        col_debug_log(main_window, f"Error in universal info display: {e}", 'COL_INFOBAR', 'ERROR')
        return False

# Export functions
__all__ = [
    'get_col_file_statistics',
    'update_col_info_bar',
    'update_col_info_bar_enhanced',
    'update_img_info_bar',
    'update_main_info_display'
]
