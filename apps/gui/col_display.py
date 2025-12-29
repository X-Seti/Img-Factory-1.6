#this belongs in gui/col_display.py - Version: 8
# X-Seti - July23 2025 - IMG Factory 1.5 - COL Display Management
# Ported from old working version with IMG debug system integration

"""
COL Display Management
Complete COL file display and table population using IMG debug system
"""

import os
from typing import Dict, List, Optional, Any
from PyQt6.QtWidgets import QTableWidget, QTableWidgetItem
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont, QColor

# Import COL classes and IMG debug system
from apps.methods.col_core_classes import COLFile, COLModel
from apps.debug.debug_functions import img_debugger
from apps.methods.col_core_classes import is_col_debug_enabled

##Methods list -
# create_table_item
# format_collision_types
# get_enhanced_model_stats
# populate_col_table_with_enhanced_data
# update_col_info_bar_enhanced

##Classes -
# COLDisplayManager

def create_table_item(text: str, data=None) -> QTableWidgetItem: #vers 1
    """Create standardized table widget item"""
    item = QTableWidgetItem(str(text))
    item.setFlags(Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEnabled)
    if data is not None:
        item.setData(Qt.ItemDataRole.UserRole, data)
    return item

def format_collision_types(stats: Dict[str, Any]) -> str: #vers 1
    """Format collision types for display"""
    types = []
    
    if stats.get('sphere_count', 0) > 0:
        types.append(f"S:{stats['sphere_count']}")
    if stats.get('box_count', 0) > 0:
        types.append(f"B:{stats['box_count']}")
    if stats.get('face_count', 0) > 0:
        types.append(f"M:{stats['face_count']}")
    
    return ", ".join(types) if types else "None"

def get_enhanced_model_stats(model: COLModel, col_file: COLFile, model_index: int) -> Dict[str, Any]: #vers 1
    """Get enhanced model statistics using IMG debug system"""
    try:
        if is_col_debug_enabled():
            img_debugger.debug(f"Getting stats for model {model_index}")
        
        stats = {
            'sphere_count': 0,
            'box_count': 0,
            'vertex_count': 0,
            'face_count': 0,
            'total_elements': 0,
            'estimated_size': 64,
            'collision_types': 'None'
        }
        
        # Get basic collision data
        if hasattr(model, 'spheres') and model.spheres:
            stats['sphere_count'] = len(model.spheres)
        
        if hasattr(model, 'boxes') and model.boxes:
            stats['box_count'] = len(model.boxes)
            
        if hasattr(model, 'vertices') and model.vertices:
            stats['vertex_count'] = len(model.vertices)
            
        if hasattr(model, 'faces') and model.faces:
            stats['face_count'] = len(model.faces)
        
        # Calculate total collision elements
        stats['total_elements'] = stats['sphere_count'] + stats['box_count'] + stats['face_count']
        
        # Estimate size in bytes
        size = stats['sphere_count'] * 20  # Sphere data
        size += stats['box_count'] * 48   # Box data
        size += stats['vertex_count'] * 12  # Vertex data
        size += stats['face_count'] * 12    # Face data
        stats['estimated_size'] = max(size, 64)
        
        # Format collision types
        stats['collision_types'] = format_collision_types(stats)
        
        if is_col_debug_enabled():
            img_debugger.debug(f"Model {model_index} stats: {stats}")
        
        return stats
        
    except Exception as e:
        img_debugger.error(f"Error getting model stats: {e}")
        return stats


class COLDisplayManager: #vers 1
    """Enhanced COL display management with IMG debug integration"""
    
    def __init__(self, main_window):
        self.main_window = main_window
        img_debugger.debug("COL Display Manager initialized")
    
    def populate_col_table(self, col_file: COLFile): #vers 1
        """Populate table with enhanced COL data"""
        try:
            if not col_file or not hasattr(col_file, 'models') or not col_file.models:
                img_debugger.warning("No COL models to display")
                return
            
            if not hasattr(self.main_window, 'gui_layout') or not hasattr(self.main_window.gui_layout, 'table'):
                img_debugger.error("No table widget available")
                return
            
            table = self.main_window.gui_layout.table
            models = col_file.models
            
            img_debugger.debug(f"Populating table with {len(models)} COL models")
            
            # Setup table structure for COL data
            self._setup_col_table_structure(table)
            
            # Set row count
            table.setRowCount(len(models))
            
            # Populate each model row
            for row, model in enumerate(models):
                # Get enhanced model statistics
                stats = self._get_enhanced_model_stats(model, col_file, row)
                
                # Model Name (Column 0)
                model_name = getattr(model, 'name', f'Model_{row+1}')
                table.setItem(row, 0, create_table_item(model_name))
                
                # Type (Column 1) - Use version if available
                version = getattr(model, 'version', None)
                if version and hasattr(version, 'name'):
                    model_type = f"COL {version.name.replace('COL_', '')}"
                else:
                    model_type = "Collision"
                table.setItem(row, 1, create_table_item(model_type))
                
                # Calculate model size
                model_size = stats.get('estimated_size', 64)
                size_str = self._format_file_size(model_size)
                table.setItem(row, 2, create_table_item(size_str))
                
                # Surface count (total collision elements)
                surface_count = stats.get('total_elements', 0)
                table.setItem(row, 3, create_table_item(str(surface_count)))
                
                # Vertex count
                vertex_count = stats.get('vertex_count', 0)
                table.setItem(row, 4, create_table_item(str(vertex_count)))
                
                # Collision types
                collision_types = stats.get('collision_types', 'None')
                table.setItem(row, 5, create_table_item(collision_types))
                
                # Status
                status = "✅ Loaded" if stats.get('total_elements', 0) > 0 else "⚠️ Empty"
                table.setItem(row, 6, create_table_item(status))
            
            img_debugger.success("COL table populated with enhanced data")
            
        except Exception as e:
            img_debugger.error(f"Error populating COL table: {str(e)}")
    
    def _get_enhanced_model_stats(self, model, col_file, model_index): #vers 1
        """Get enhanced model statistics using centralized parser"""
        try:
            # Try using the centralized COL parser first
            try:
                from apps.methods.col_parsing_functions import get_model_collision_stats, format_model_collision_types
                
                # Get stats from centralized parser with debug
                stats = get_model_collision_stats(col_file.file_path, model_index, debug=True)
                
                # Add collision types formatting
                stats['collision_types'] = format_model_collision_types(stats)
                
                return stats
            
            except (ImportError, AttributeError):
                # Fallback to local calculation
                return get_enhanced_model_stats(model, col_file, model_index)
        
        except Exception as e:
            img_debugger.error(f"Error getting enhanced model stats: {e}")
            # Return basic stats as fallback
            return {
                'sphere_count': 0,
                'box_count': 0,
                'vertex_count': 0,
                'face_count': 0,
                'total_elements': 0,
                'estimated_size': 64,
                'collision_types': 'None'
            }
    
    def _setup_col_table_structure(self, table): #vers 1
        """Setup table structure for COL display"""
        try:
            # COL-specific columns with enhanced info
            col_headers = ["Model Name", "Type", "Size", "Surfaces", "Vertices", "Collision Types", "Status"]
            table.setColumnCount(len(col_headers))
            table.setHorizontalHeaderLabels(col_headers)
            
            # Adjust column widths for COL data
            table.setColumnWidth(0, 200)  # Model Name
            table.setColumnWidth(1, 100)  # Type
            table.setColumnWidth(2, 100)  # Size
            table.setColumnWidth(3, 80)   # Surfaces
            table.setColumnWidth(4, 80)   # Vertices
            table.setColumnWidth(5, 150)  # Collision Types
            table.setColumnWidth(6, 100)  # Status
            
        except Exception as e:
            img_debugger.error(f"Error setting up COL table structure: {e}")
    
    def _calculate_model_size_estimate(self, stats): #vers 1
        """Calculate estimated model size based on collision data"""
        size = stats['sphere_count'] * 20  # Sphere data (rough estimate)
        size += stats['box_count'] * 48   # Box data
        size += stats['vertex_count'] * 12  # Vertex data (average)
        size += stats['face_count'] * 12  # Face data (average)
        return max(size, 64)
    
    def _format_file_size(self, size_bytes): #vers 1
        """Format file size for display"""
        if size_bytes < 1024:
            return f"{size_bytes} B"
        elif size_bytes < 1024*1024:
            return f"{size_bytes/1024:.1f} KB"
        else:
            return f"{size_bytes/(1024*1024):.1f} MB"
    
    def update_col_info_bar(self, col_file, file_path): #vers 1
        """Update info bar with COL file information"""
        try:
            gui_layout = self.main_window.gui_layout
            
            # Update file count
            if hasattr(gui_layout, 'file_count_label'):
                model_count = len(col_file.models) if hasattr(col_file, 'models') else 0
                gui_layout.file_count_label.setText(f"Models: {model_count}")
            
            # Update file size
            if hasattr(gui_layout, 'file_size_label'):
                file_size = os.path.getsize(file_path)
                size_str = self._format_file_size(file_size)
                gui_layout.file_size_label.setText(f"Size: {size_str}")
            
            # Update format version
            if hasattr(gui_layout, 'format_version_label'):
                version = getattr(col_file, 'version', 'Unknown')
                gui_layout.format_version_label.setText(f"Format: COL v{version}")
            
            img_debugger.success("Info bar updated for COL file")
            
        except Exception as e:
            img_debugger.warning(f"Error updating info bar: {str(e)}")


# Standalone functions for use without class
def populate_col_table_with_enhanced_data(main_window, col_file): #vers 1
    """Standalone function to populate COL table with enhanced data"""
    display_manager = COLDisplayManager(main_window)
    display_manager.populate_col_table(col_file)

def update_col_info_bar_enhanced(main_window, col_file, file_path): #vers 4
    """Standalone function to update info bar with enhanced COL data"""
    display_manager = COLDisplayManager(main_window)
    display_manager.update_col_info_bar(col_file, file_path)


# Export main functions
__all__ = [
    'COLDisplayManager',
    'create_table_item',
    'format_collision_types',
    'get_enhanced_model_stats',
    'populate_col_table_with_enhanced_data',
    'update_col_info_bar_enhanced'
]
