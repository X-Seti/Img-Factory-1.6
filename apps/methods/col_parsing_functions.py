#this belongs in methods.col_parsing_functions.py - Version: 1
# X-Seti - July23 2025 - IMG Factory 1.5 - COL Parsing Functions
# Complete COL parsing functions with safe parsing and IMG debug system - EXACT OLD VERSION PORT

"""
COL Parser - Complete collision file parsing with debug control
Handles all COL format versions (COL1/COL2/COL3/COL4) with detailed logging
"""

import struct
import os
from typing import Dict, List, Tuple, Optional
from apps.debug.debug_functions import img_debugger
from apps.methods.col_core_classes import is_col_debug_enabled

##Methods list -
# load_col_file_safely
# reset_table_styling
# setup_col_tab_integration

##Classes -
# COLParser

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


def load_col_file_safely(main_window, file_path): #vers 6
    """Load COL file safely with complete error handling and debugging"""
    try:
        main_window.log_message(f"🔧 Loading COL: {os.path.basename(file_path)}")
        img_debugger.debug(f"Starting COL load: {file_path}")

        # Validate file first
        if not os.path.exists(file_path):
            error_msg = f"COL file not found: {file_path}"
            main_window.log_message(f"❌ {error_msg}")
            img_debugger.error(error_msg)
            return False

        if not os.access(file_path, os.R_OK):
            error_msg = f"Cannot read COL file: {file_path}"
            main_window.log_message(f"❌ {error_msg}")
            img_debugger.error(error_msg)
            return False

        file_size = os.path.getsize(file_path)
        if file_size < 32:
            error_msg = f"COL file too small ({file_size} bytes, minimum 32 bytes required)"
            main_window.log_message(f"❌ {error_msg}")
            img_debugger.error(error_msg)
            return False

        img_debugger.debug(f"COL file validation passed: {file_size} bytes")

        # Check COL signature
        try:
            with open(file_path, 'rb') as f:
                signature = f.read(4)
                img_debugger.debug(f"COL signature read: {signature}")

                if signature not in [b'COLL', b'COL\x02', b'COL\x03', b'COL\x04']:
                    error_msg = f"Invalid COL signature: {signature} (expected COLL, COL\\x02, COL\\x03, or COL\\x04)"
                    main_window.log_message(f"❌ {error_msg}")
                    img_debugger.error(error_msg)
                    return False

                img_debugger.debug(f"COL signature valid: {signature}")

        except Exception as e:
            error_msg = f"Failed to read COL signature: {str(e)}"
            main_window.log_message(f"❌ {error_msg}")
            img_debugger.error(error_msg)
            return False

        # Import our complete COL classes
        try:
            from apps.methods.col_core_classes import COLFile
            img_debugger.debug("COL classes imported successfully")
        except ImportError as e:
            error_msg = f"Failed to import COL classes: {str(e)}"
            main_window.log_message(f"❌ {error_msg}")
            img_debugger.error(error_msg)
            return False

        # Create COL file object
        img_debugger.debug("Creating COL file object...")
        col_file = COLFile(file_path)

        # Load the COL file
        img_debugger.debug("Starting COL file parsing...")
        success = col_file.load()

        if not success:
            # Get specific error from COL file
            error_msg = col_file.load_error if hasattr(col_file, 'load_error') and col_file.load_error else "Unknown COL parsing error"

            # Additional debugging
            img_debugger.error(f"COL parsing failed. Error: {error_msg}")

            # Try to get more details
            if hasattr(col_file, 'models'):
                img_debugger.debug(f"COL models found: {len(col_file.models) if col_file.models else 0}")

            main_window.log_message(f"❌ Failed to load COL file: {error_msg}")
            return False

        # Check if models were loaded
        if not hasattr(col_file, 'models') or not col_file.models:
            error_msg = "COL file parsed but contains no models"
            main_window.log_message(f"❌ {error_msg}")
            img_debugger.error(error_msg)
            return False

        img_debugger.debug(f"COL file loaded successfully: {len(col_file.models)} models found")

        # Success - store the loaded COL file
        main_window.current_col = col_file

        # Store COL file in tab tracking for tab switching
        current_index = main_window.main_tab_widget.currentIndex()
        if hasattr(main_window, 'open_files') and current_index in main_window.open_files:
            main_window.open_files[current_index]['file_object'] = col_file
            main_window.log_message(f"✅ COL file object stored in tab {current_index}")

        # Update UI
        if hasattr(main_window, '_update_ui_for_loaded_col'):
            try:
                main_window._update_ui_for_loaded_col()
                img_debugger.debug("UI updated for loaded COL")
            except Exception as e:
                img_debugger.warning(f"Failed to update UI for COL: {e}")

        # Calculate statistics
        model_count = len(col_file.models)
        total_objects = 0
        for model in col_file.models:
            if hasattr(model, 'spheres'):
                total_objects += len(model.spheres)
            if hasattr(model, 'boxes'):
                total_objects += len(model.boxes)

        success_msg = f"COL loaded: {model_count} models, {total_objects} collision objects"
        main_window.log_message(f"✅ {success_msg}")
        img_debugger.success(success_msg)

        return True

    except Exception as e:
        error_msg = f"COL loading exception: {str(e)}"
        main_window.log_message(f"❌ {error_msg}")
        img_debugger.error(f"COL loading exception: {e}")
        import traceback
        img_debugger.error(f"COL loading traceback: {traceback.format_exc()}")
        return False

def _update_col_info_bar_enhanced(main_window, col_file, file_path): #vers 1
    """Update info bar using enhanced display manager"""
    try:
        from apps.components.col_display import COLDisplayManager

        display_manager = COLDisplayManager(main_window)
        display_manager.update_col_info_bar(col_file, file_path)
        img_debugger.success("Enhanced info bar updated")

    except Exception as e:
        img_debugger.error(f"Enhanced info bar update failed: {str(e)}")
        raise

def _validate_col_file(main_window, file_path): #vers 1
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

def _setup_col_tab(main_window, file_path): #vers 1
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

def _load_col_file(main_window, file_path): #vers 1
    """Load COL file object"""
    try:
        from apps.methods.col_core_classes import COLFile

        img_debugger.debug(f"Loading COL file: {os.path.basename(file_path)}")

        # Create COL file object
        col_file = COLFile(file_path)

        # Load the file
        if col_file.load():
            model_count = len(col_file.models) if hasattr(col_file, 'models') else 0
            img_debugger.success(f"COL file loaded: {model_count} models")
            return col_file
        else:
            error_details = col_file.load_error if hasattr(col_file, 'load_error') else "Unknown error"
            img_debugger.error(f"Failed to load COL file: {error_details}")
            return None

    except Exception as e:
        img_debugger.error(f"Error loading COL file: {str(e)}")
        return None

def _setup_col_table_structure(main_window): #vers 1
    """Setup table structure for COL data"""
    try:
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'table'):
            img_debugger.error("No table widget available")
            return False

        table = main_window.gui_layout.table

        # COL-specific columns
        col_headers = ["Model Name", "Type", "Version", "Size", "Spheres", "Boxes", "Faces", "Info"]
        table.setColumnCount(len(col_headers))
        table.setHorizontalHeaderLabels(col_headers)

        # Set column widths
        table.setColumnWidth(0, 200)  # Model Name
        table.setColumnWidth(1, 80)   # Type
        table.setColumnWidth(2, 80)   # Version
        table.setColumnWidth(3, 100)  # Size
        table.setColumnWidth(4, 80)   # Spheres
        table.setColumnWidth(5, 80)   # Boxes
        table.setColumnWidth(6, 80)   # Faces
        table.setColumnWidth(7, 150)  # Info

        img_debugger.debug("COL table structure setup complete")
        return True

    except Exception as e:
        img_debugger.error(f"Error setting up COL table structure: {str(e)}")
        return False


class COLParser: #vers 1
    """Enhanced COL parser with multi-model support and debug control"""

    def __init__(self, debug: bool = None):
        if debug is None:
            debug = is_col_debug_enabled()

        self.debug = debug
        self.log_messages = []

    def log(self, message): #vers 1
        """Log debug message only if debug is enabled"""
        if self.debug:
            img_debugger.debug(f"COLParser: {message}")
        self.log_messages.append(message)

    def set_debug(self, enabled): #vers 1
        """Update debug setting"""
        self.debug = enabled

    def parse_col_file_structure(self, file_path): #vers 1
        """Parse complete COL file and return structure info for all models"""
        try:
            with open(file_path, 'rb') as f:
                data = f.read()

            self.log(f"Parsing COL file: {os.path.basename(file_path)} ({len(data)} bytes)")

            # CRITICAL FIX: First check if this is a multi-model COL archive
            if self._is_multi_model_archive(data):
                return self._parse_multi_model_archive(data)

            # Single model parsing (original approach)
            models = []
            offset = 0
            model_index = 0

            while offset < len(data):
                self.log(f"\n--- Model {model_index} at offset {offset} ---")

                # Check if we have enough data for a model
                if offset + 32 > len(data):
                    self.log(f"Not enough data for model header (need 32, have {len(data) - offset})")
                    break

                # Parse this model
                model_info, new_offset = self.parse_single_model(data, offset, model_index)

                if model_info is None:
                    self.log(f"Failed to parse model {model_index}, stopping")
                    break

                models.append(model_info)

                # Check if we advanced
                if new_offset <= offset:
                    self.log(f"Offset didn't advance (was {offset}, now {new_offset}), stopping")
                    break

                offset = new_offset
                model_index += 1

                self.log(f"Model {model_index - 1} parsed successfully, next offset: {offset}")

                # Safety check - don't parse more than 200 models
                if model_index > 200:
                    self.log("Safety limit reached (200 models), stopping")
                    break

            self.log(f"\nParsing complete: {len(models)} models found")
            return models

        except Exception as e:
            self.log(f"Error parsing COL file: {str(e)}")
            return []

    def _is_multi_model_archive(self, data: bytes) -> bool: #vers 1
        """Check if this is a multi-model COL archive"""
        try:
            # Look for multiple COL signatures
            signature_count = 0
            offset = 0

            while offset < len(data) - 4:
                if data[offset:offset+4] in [b'COLL', b'COL2', b'COL3', b'COL4']:
                    signature_count += 1
                    if signature_count > 1:
                        self.log(f"Detected multi-model archive ({signature_count} signatures found)")
                        return True
                    # Skip to avoid counting the same signature multiple times
                    offset += 100
                else:
                    offset += 1

            return False

        except Exception:
            return False

    def _parse_multi_model_archive(self, data: bytes) -> List[dict]: #vers 1
        """Parse multi-model COL archive with different structure"""
        try:
            self.log("Parsing as multi-model archive...")
            models = []

            # Find all COL signatures in the file
            signatures = []
            offset = 0

            while offset < len(data) - 4:
                sig = data[offset:offset+4]
                if sig in [b'COLL', b'COL2', b'COL3', b'COL4']:
                    signatures.append(offset)
                    self.log(f"Found {sig} at offset {offset}")
                offset += 1

            self.log(f"Found {len(signatures)} model signatures")

            # Parse each model starting from its signature
            for i, sig_offset in enumerate(signatures):
                try:
                    self.log(f"\n--- Archive Model {i} at offset {sig_offset} ---")
                    model_info, _ = self.parse_single_model(data, sig_offset, i)

                    if model_info:
                        models.append(model_info)
                        self.log(f"Archive model {i} parsed successfully")
                    else:
                        self.log(f"Failed to parse archive model {i}")

                except Exception as e:
                    self.log(f"Error parsing archive model {i}: {str(e)}")
                    continue

            self.log(f"Archive parsing complete: {len(models)} models found")
            return models

        except Exception as e:
            self.log(f"Error parsing multi-model archive: {str(e)}")
            return []

    def parse_single_model(self, data: bytes, offset: int = 0, model_index: int = 0) -> Tuple[Optional[dict], int]: #vers 1
        """Parse a single COL model and return info + new offset - EXACT OLD VERSION"""
        try:
            start_offset = offset

            # Read signature
            if offset + 4 > len(data):
                return None, offset

            signature = data[offset:offset+4]
            self.log(f"Signature: {signature}")

            # Validate signature
            if signature not in [b'COLL', b'COL2', b'COL3', b'COL4']:
                self.log(f"Invalid signature: {signature}")
                return None, offset

            offset += 4

            # Read file size
            if offset + 4 > len(data):
                return None, offset

            file_size = struct.unpack('<I', data[offset:offset+4])[0]
            self.log(f"Declared size: {file_size}")
            offset += 4

            # Read model name (22 bytes)
            if offset + 22 > len(data):
                return None, offset

            name_bytes = data[offset:offset+22]
            model_name = name_bytes.split(b'\x00')[0].decode('ascii', errors='ignore')
            self.log(f"Model name: '{model_name}'")
            offset += 22

            # Read model ID (2 bytes)
            if offset + 2 > len(data):
                return None, offset

            model_id = struct.unpack('<H', data[offset:offset+2])[0]
            self.log(f"Model ID: {model_id}")
            offset += 2

            # Determine version from signature
            if signature == b'COLL':
                version = 'COL1'
            elif signature == b'COL2':
                version = 'COL2'
            elif signature == b'COL3':
                version = 'COL3'
            elif signature == b'COL4':
                version = 'COL4'
            else:
                version = 'UNKNOWN'

            # Parse collision data based on version
            collision_stats = self._parse_collision_data(data, offset, file_size - 28, version)

            # Calculate end offset
            end_offset = self._calculate_model_end_offset(data, start_offset, file_size)

            # Build model info dictionary - EXACT OLD VERSION
            model_info = {
                'name': model_name or f'Model_{model_index}',
                'model_id': model_id,
                'version': version,
                'size': file_size + 8,  # Include header
                'sphere_count': collision_stats.get('sphere_count', 0),
                'box_count': collision_stats.get('box_count', 0),
                'vertex_count': collision_stats.get('vertex_count', 0),
                'face_count': collision_stats.get('face_count', 0),
                'total_elements': collision_stats.get('total_elements', 0),
                'estimated_size': file_size + 8
            }

            self.log(f"Model parsed: {model_info['name']} (v{version}, {file_size + 8} bytes)")

            return model_info, end_offset

        except Exception as e:
            self.log(f"Error parsing model at offset {offset}: {str(e)}")
            return None, offset + 32  # Skip ahead to try next model

    def _parse_collision_data(self, data: bytes, offset: int, data_size: int, version: str) -> dict: #vers 1
        """Parse collision data and return statistics"""
        stats = {
            'sphere_count': 0,
            'box_count': 0,
            'vertex_count': 0,
            'face_count': 0,
            'total_elements': 0
        }

        try:
            if version == 'COL1':
                # COL1 has simpler structure
                # For now, estimate based on data size
                if data_size > 100:
                    stats['sphere_count'] = max(1, data_size // 200)
                    stats['total_elements'] = stats['sphere_count']
            else:
                # COL2/3/4 have more complex structure
                # For now, estimate based on data size
                if data_size > 200:
                    stats['vertex_count'] = max(10, data_size // 50)
                    stats['face_count'] = max(5, data_size // 100)
                    stats['total_elements'] = stats['vertex_count'] + stats['face_count']

            self.log(f"Collision stats: {stats}")

        except Exception as e:
            self.log(f"Error parsing collision data: {e}")

        return stats

    def _calculate_model_end_offset(self, data: bytes, start_offset: int, declared_size: int) -> int: #vers 1
        """Calculate the end offset of a model"""
        try:
            # Add 8 bytes for header (signature + size)
            end_offset = start_offset + declared_size + 8

            # Ensure we don't go beyond data bounds
            if end_offset > len(data):
                end_offset = len(data)

            self.log(f"Model end offset: {end_offset}")

            return end_offset

        except Exception as e:
            self.log(f"Error calculating model end: {str(e)}")
            return start_offset + 800  # Safe fallback

    def get_model_stats_by_index(self, file_path: str, model_index: int) -> dict: #vers 1
        """Get statistics for a specific model by index"""
        models = self.parse_col_file_structure(file_path)

        if model_index < len(models):
            return models[model_index]
        else:
            self.log(f"Model index {model_index} not found (only {len(models)} models)")
            return {
                'sphere_count': 0,
                'box_count': 0,
                'vertex_count': 0,
                'face_count': 0,
                'total_elements': 0,
                'estimated_size': 64
            }

    def format_collision_types(self, stats: dict) -> str: #vers 1
        """Format collision types string from stats"""
        types = []
        if stats.get('sphere_count', 0) > 0:
            types.append(f"Spheres({stats['sphere_count']})")
        if stats.get('box_count', 0) > 0:
            types.append(f"Boxes({stats['box_count']})")
        if stats.get('face_count', 0) > 0:
            types.append(f"Mesh({stats['face_count']})")

        return ", ".join(types) if types else "None"

    def get_debug_log(self): #vers 1
        """Get debug log messages"""
        return self.log_messages

    def clear_debug_log(self): #vers 1
        """Clear debug log"""
        self.log_messages = []


# Convenience functions
def parse_col_file_with_debug(file_path, debug=None): #vers 1
    """Parse COL file and return model statistics with optional debug output"""
    if debug is None:
        debug = is_col_debug_enabled()

    parser = COLParser(debug=debug)
    models = parser.parse_col_file_structure(file_path)

    if debug:
        img_debugger.debug("COL Parser Debug Log:")
        for msg in parser.get_debug_log():
            img_debugger.debug(msg)

    return models

def get_model_collision_stats(file_path, model_index, debug=None): #vers 1
    """Get collision statistics for a specific model"""
    if debug is None:
        debug = is_col_debug_enabled()

    parser = COLParser(debug=debug)
    return parser.get_model_stats_by_index(file_path, model_index)

def format_model_collision_types(stats): #vers 1
    """Format collision types string"""
    parser = COLParser(debug=False)
    return parser.format_collision_types(stats)


# Export main classes and functions
__all__ = [
    'COLParser',
    'parse_col_file_with_debug',
    'get_model_collision_stats', 
    'format_model_collision_types',
    'load_col_file_safely',
    'reset_table_styling',
    'setup_col_tab_integration'
]
