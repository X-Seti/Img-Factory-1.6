#this belongs in methods/populate_col_table.py - Version: 5
# X-Seti - August14 2025 - IMG Factory 1.5 - COL Table Population Methods
"""
COL Table Population Methods - Fixed version
Handles populating the main table widget with COL file data
Uses IMG debug system instead of old COL debug calls
"""

import os
from typing import Optional, Any
from PyQt6.QtWidgets import QTableWidget, QTableWidgetItem, QHeaderView
from PyQt6.QtCore import Qt

# Import IMG debug system and COL classes
from apps.debug.debug_functions import img_debugger
from apps.methods.col_core_classes import COLFile, COLModel

##Methods list -
# load_col_file_object
# load_col_file_safely
# populate_col_table
# populate_table_with_col_data_debug
# reset_table_styling
# setup_col_tab
# setup_col_tab_integration
# setup_col_table_structure
# setup_table_for_col_data
# update_col_info_bar_enhanced
# validate_col_file

def populate_table_with_col_data_debug(main_window, col_file): #vers 1
    """Populate table with COL file data using IMG debug system"""
    try:
        if not col_file or not hasattr(col_file, 'models') or not col_file.models:
            img_debugger.warning("No COL data to populate")
            return False

        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.error("No table widget available")
            return False

        table = main_window.gui_layout.table
        models = col_file.models

        img_debugger.debug(f"Populating table with {len(models)} COL models")

        # Setup table structure first
        setup_col_table_structure(main_window)

        # Set row count
        table.setRowCount(len(models))

        # Populate each model
        for row, model in enumerate(models):
            try:
                # Model Name (Column 0)
                model_name = model.name if hasattr(model, 'name') and model.name else f"Model_{row+1}"
                table.setItem(row, 0, QTableWidgetItem(model_name))

                # Type (Column 1)
                table.setItem(row, 1, QTableWidgetItem("COL"))

                # Version (Column 2)
                if hasattr(model, 'version') and hasattr(model.version, 'value'):
                    version_text = f"v{model.version.value}"
                else:
                    version_text = "Unknown"
                table.setItem(row, 2, QTableWidgetItem(version_text))

                # Size (Column 3) - Estimate based on data
                sphere_count = len(model.spheres) if hasattr(model, 'spheres') else 0
                box_count = len(model.boxes) if hasattr(model, 'boxes') else 0
                vertex_count = len(model.vertices) if hasattr(model, 'vertices') else 0
                face_count = len(model.faces) if hasattr(model, 'faces') else 0

                estimated_size = (sphere_count * 20) + (box_count * 32) + (vertex_count * 12) + (face_count * 20)
                if estimated_size > 0:
                    size_text = f"{estimated_size:,} bytes"
                else:
                    # Zero collision data = LOD model
                    size_text = "0 bytes LOD"
                #size_text = f"{estimated_size:,} bytes" if estimated_size > 0 else "Unknown"

                table.setItem(row, 3, QTableWidgetItem(size_text))

                # Spheres (Column 4)
                table.setItem(row, 4, QTableWidgetItem(str(sphere_count)))

                # Boxes (Column 5)
                table.setItem(row, 5, QTableWidgetItem(str(box_count)))

                # Vertices (Column 6)
                table.setItem(row, 6, QTableWidgetItem(str(vertex_count)))

                # Faces (Column 7)
                table.setItem(row, 7, QTableWidgetItem(str(face_count)))

            except Exception as e:
                img_debugger.error(f"Error populating row {row}: {str(e)}")
                # Fill with error data
                table.setItem(row, 0, QTableWidgetItem(f"Error_Model_{row}"))
                for col in range(1, 8):
                    table.setItem(row, col, QTableWidgetItem("Error"))

        img_debugger.success(f"COL table populated with {len(models)} models")
        return True

    except Exception as e:
        img_debugger.error(f"Error populating COL table: {str(e)}")
        return False

def validate_col_file(main_window, file_path): #vers 1
    """Validate COL file before loading"""
    if not os.path.exists(file_path):
        img_debugger.error(f"COL file not found: {file_path}")
        return False
    
    if not os.access(file_path, os.R_OK):
        img_debugger.error(f"Cannot read COL file: {file_path}")
        return False
    
    file_size = os.path.getsize(file_path)
    if file_size < 32:
        img_debugger.error(f"COL file too small ({file_size} bytes)")
        return False
    
    return True

def setup_table_for_col_data(table: QTableWidget) -> bool: #vers 1
    """Setup table structure for COL file data"""
    try:
        # COL file columns
        col_headers = ["Model Name", "Type", "Version", "Size", "Spheres", "Boxes", "Faces", "Info"]
        table.setColumnCount(len(col_headers))
        table.setHorizontalHeaderLabels(col_headers)

        # Set column widths for COL data
        table.setColumnWidth(0, 200)  # Model Name
        table.setColumnWidth(1, 80)   # Type
        table.setColumnWidth(2, 80)   # Version
        table.setColumnWidth(3, 100)  # Size
        table.setColumnWidth(4, 80)   # Spheres
        table.setColumnWidth(5, 80)   # Boxes
        table.setColumnWidth(6, 80)   # Faces
        table.setColumnWidth(7, 150)  # Info

        # Enable sorting
        table.setSortingEnabled(True)

        img_debugger.debug("Table structure setup for COL data")
        return True

    except Exception as e:
        img_debugger.error(f"Error setting up COL table structure: {e}")
        return False

def setup_col_tab(main_window, file_path): #vers 1
    """Setup or reuse tab for COL file"""
    try:
        current_index = main_window.main_tab_widget.currentIndex()
        
        # Check if current tab is empty
        if not hasattr(main_window, 'open_files') or current_index not in main_window.open_files:
            img_debugger.debug("Using current tab for COL file")
        else:
            img_debugger.debug("Creating new tab for COL file")
            if hasattr(main_window, 'close_manager'):
                main_window.create_tab()
                current_index = main_window.main_tab_widget.currentIndex()
            else:
                img_debugger.warning("Close manager not available")
                return None
        
        # Setup tab info
        file_name = os.path.basename(file_path)
        file_name_clean = file_name[:-4] if file_name.lower().endswith('.col') else file_name
        tab_name = f"{file_name_clean}"
        
        # Store tab info
        if not hasattr(main_window, 'open_files'):
            main_window.open_files = {}
        
        main_window.open_files[current_index] = {
            'type': 'COL',
            'file_path': file_path,
            'file_object': None,
            'tab_name': tab_name
        }
        
        # Update tab name
        main_window.main_tab_widget.setTabText(current_index, tab_name)
        
        return current_index
        
    except Exception as e:
        img_debugger.error(f"Error setting up COL tab: {str(e)}")
        return None

def load_col_file_object(main_window, file_path): #vers 1
    """Load COL file object"""
    try:
        from apps.methods.col_core_classes import COLFile
        
        img_debugger.debug(f"Loading COL file: {os.path.basename(file_path)}")
        
        # Create COL file object
        col_file = COLFile()
        if not col_file.load_from_file(file_path):
            img_debugger.error(f"Failed to load COL file: {col_file.load_error if hasattr(col_file, "load_error") else "Unknown error"}")
            return None
        
        # Load the file
            img_debugger.success(f"COL file loaded: {model_count} models")
            return col_file
        else:
            error_details = col_file.load_error if hasattr(col_file, 'load_error') else "Unknown error"
            img_debugger.error(f"Failed to load COL file: {error_details}")
            return None
        
    except Exception as e:
        img_debugger.error(f"Error loading COL file: {str(e)}")
        return None

def setup_col_table_structure(main_window): #vers 1
    """Setup table structure for COL data"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.error("No table widget available")
            return False
        
        table = main_window.gui_layout.table
        
        # COL-specific columns
        col_headers = ["Model Name", "Type", "Version", "Size", "Spheres", "Boxes", "Vertices", "Faces"]
        table.setColumnCount(len(col_headers))
        table.setHorizontalHeaderLabels(col_headers)
        
        # Set column widths
        table.setColumnWidth(0, 200)  # Model Name
        table.setColumnWidth(1, 80)   # Type
        table.setColumnWidth(2, 80)   # Version
        table.setColumnWidth(3, 100)  # Size
        table.setColumnWidth(4, 80)   # Spheres
        table.setColumnWidth(5, 80)   # Boxes
        table.setColumnWidth(6, 80)   # Vertices
        table.setColumnWidth(7, 80)   # Faces
        
        # Enable sorting
        table.setSortingEnabled(True)
        
        img_debugger.debug("COL table structure setup complete")
        return True
        
    except Exception as e:
        img_debugger.error(f"Error setting up COL table structure: {str(e)}")
        return False


def populate_col_table(main_window, col_file): #vers 3
    """Populate table using direct model data"""
    try:
        if not col_file or not hasattr(col_file, 'models') or not col_file.models:
            img_debugger.warning("No COL data to populate")
            return False

        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.error("No table widget available")
            return False

        table = main_window.gui_layout.table
        models = col_file.models

        img_debugger.debug(f"Populating table with {len(models)} COL models")

        # Set row count
        table.setRowCount(len(models))

        for row, model in enumerate(models):
            try:
                # Model Name
                model_name = getattr(model, 'name', f'Model_{row+1}')
                table.setItem(row, 0, QTableWidgetItem(str(model_name)))

                # Type
                table.setItem(row, 1, QTableWidgetItem("COL"))

                # Version
                version = getattr(model, 'version', 'Unknown')
                if hasattr(version, 'value'):
                    version_text = f"COL{version.value}"
                else:
                    version_text = str(version)
                table.setItem(row, 2, QTableWidgetItem(version_text))

                # Size - USE ACTUAL MODEL SIZE NOT ESTIMATED
                if hasattr(model, 'model_size') and model.model_size > 0:
                    # Use actual model size from file
                    actual_size = model.model_size
                    if actual_size > 1024:
                        size_text = f"{actual_size//1024}KB"
                    else:
                        size_text = f"{actual_size}B"
                else:
                    # Fallback: minimum viable COL model size
                    size_text = "64B"

                table.setItem(row, 3, QTableWidgetItem(size_text))

                # Collision data counts
                spheres_count = len(getattr(model, 'spheres', []))
                boxes_count = len(getattr(model, 'boxes', []))
                vertices_count = len(getattr(model, 'vertices', []))
                faces_count = len(getattr(model, 'faces', []))

                table.setItem(row, 4, QTableWidgetItem(str(spheres_count)))
                table.setItem(row, 5, QTableWidgetItem(str(boxes_count)))
                table.setItem(row, 6, QTableWidgetItem(str(vertices_count)))
                table.setItem(row, 7, QTableWidgetItem(str(faces_count)))

            except Exception as e:
                img_debugger.error(f"Error populating row {row}: {str(e)}")
                # Fill with safe defaults
                table.setItem(row, 0, QTableWidgetItem(f"Model_{row+1}"))
                table.setItem(row, 1, QTableWidgetItem("COL"))
                table.setItem(row, 2, QTableWidgetItem("COL1"))
                table.setItem(row, 3, QTableWidgetItem("64B"))  # Safe minimum
                for col in range(4, 8):
                    table.setItem(row, col, QTableWidgetItem("0"))

        img_debugger.success(f"COL table populated with {len(models)} models")
        return True

    except Exception as e:
        img_debugger.error(f"Enhanced table population failed: {str(e)}")
        return False

def update_col_info_bar_enhanced(main_window, col_file, file_path): #vers 2
    """Update info bar using direct method - NO col_display dependency"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'info_bar'):
            img_debugger.warning("Info bar not available")
            return False
        
        if not col_file or not hasattr(col_file, 'models'):
            return False
        
        # Calculate totals
        total_models = len(col_file.models)
        total_spheres = sum(len(getattr(model, 'spheres', [])) for model in col_file.models)
        total_boxes = sum(len(getattr(model, 'boxes', [])) for model in col_file.models)
        total_vertices = sum(len(getattr(model, 'vertices', [])) for model in col_file.models)
        total_faces = sum(len(getattr(model, 'faces', [])) for model in col_file.models)
        
        # Create info text
        file_name = os.path.basename(file_path)
        info_text = f"{file_name} | {total_models} models | {total_spheres} spheres | {total_boxes} boxes | {total_vertices} vertices | {total_faces} faces"
        
        # Update info bar
        # Update info bar safely
        if hasattr(main_window.gui_layout, 'info_bar') and main_window.gui_layout.info_bar:
            main_window.gui_layout.info_bar.setText(info_text)

        img_debugger.success("COL info bar updated")
        return True
        
    except Exception as e:
        img_debugger.error(f"Enhanced info bar update failed: {str(e)}")
        return False

def reset_table_styling(main_window): #vers 1
    """Completely reset table styling to default"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            return

        table = main_window.gui_layout.table
        header = table.horizontalHeader()

        # Clear all styling
        table.setStyleSheet("")
        header.setStyleSheet("")
        table.setObjectName("")

        # Reset to basic alternating colors
        table.setAlternatingRowColors(True)

        img_debugger.debug("Table styling completely reset")

    except Exception as e:
        img_debugger.warning(f"Error resetting table styling: {str(e)}")

def setup_col_tab_integration(main_window): #vers 1
    """Setup COL tab integration with main window"""
    try:
        # Add COL loading method to main window
        main_window.load_col_file_safely = lambda file_path: load_col_file_safely(main_window, file_path)

        # Add styling reset method
        main_window._reset_table_styling = lambda: reset_table_styling(main_window)

        img_debugger.success("COL tab integration ready")
        return True

    except Exception as e:
        img_debugger.error(f"COL tab integration failed: {str(e)}")
        return False

def load_col_file_safely(main_window, file_path): #vers 1
    """Load COL file safely with proper tab management"""
    try:
        # Validate file
        if not validate_col_file(main_window, file_path):
            return False

        # Setup tab
        tab_index = setup_col_tab(main_window, file_path)
        if tab_index is None:
            return False

        # Load COL file
        col_file = load_col_file_object(main_window, file_path)
        if col_file is None:
            return False

        # Setup table structure for COL data
        setup_col_table_structure(main_window)

        # Populate table with COL data (try enhanced first, fallback to debug version)
        try:
            populate_col_table(main_window, col_file)
        except Exception as e:
            img_debugger.warning(f"Enhanced table population failed: {str(e)}, using fallback")
            populate_table_with_col_data_debug(main_window, col_file)

        # Update main window state
        main_window.current_col = col_file
        main_window.open_files[tab_index]['file_object'] = col_file

        # Update info bar (try enhanced first, fallback to basic)
        try:
            update_col_info_bar_enhanced(main_window, col_file, file_path)
        except Exception as e:
            img_debugger.warning(f"Enhanced info bar update failed: {str(e)}")
            # If enhanced fails, we continue without it

        img_debugger.success(f"COL file loaded: {os.path.basename(file_path)}")
        return True

    except Exception as e:
        img_debugger.error(f"Error loading COL file: {str(e)}")
        return False
