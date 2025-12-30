#this belongs in methods/col_workshop_loader.py - Version: 1
# X-Seti - December21 2025 - Col Workshop - COL File Loader
"""
COL File Loader - High-level interface for loading COL files
Handles single and multi-model archives
"""

import os
from typing import List, Optional, Tuple
from apps.methods.col_workshop_parser import COLParser
from apps.methods.col_workshop_classes import COLModel, COLVersion

# Optional debug - use print if not available
try:
    from apps.debug.debug_functions import img_debugger
except ImportError:
    class SimpleDebugger:
        def debug(self, msg): print(f"DEBUG: {msg}")
        def error(self, msg): print(f"ERROR: {msg}")
        def warning(self, msg): print(f"WARN: {msg}")
        def success(self, msg): print(f"SUCCESS: {msg}")
        def info(self, msg): print(f"INFO: {msg}")
    img_debugger = SimpleDebugger()

##Methods list -
# COLFile - Class
# Init
# load
# load_from_file
# load_from_data
# get_model_count
# get_model
# get_model_by_name
# get_stats
# get_info
# is_multi_model
# _parse_all_models
# validate


class COLFile: #vers 1
    """High-level COL file interface"""
    
    def __init__(self, debug: bool = False): #vers 1
        """Initialize COL file handler"""
        self.parser = COLParser(debug=debug)
        self.debug = debug
        self.models: List[COLModel] = []
        self.is_loaded = False
        self.load_error = ""
        self.file_path: Optional[str] = None
        self.raw_data: Optional[bytes] = None
    

    def load(self, file_path: str) -> bool: #vers 1
        """
        Load COL file from disk
        
        Returns: True if successful
        """
        try:
            if not os.path.exists(file_path):
                raise FileNotFoundError(f"COL file not found: {file_path}")
            
            # Read file data
            with open(file_path, 'rb') as f:
                self.raw_data = f.read()
            
            self.file_path = file_path
            
            # Parse models
            self.models = self._parse_all_models(self.raw_data)
            
            if self.debug:
                print(f"Loaded {len(self.models)} models from {os.path.basename(file_path)}")
            
            return True
            
        except Exception as e:
            if self.debug:
                print(f"Error loading COL file: {e}")
            return False
    

    def _load_single_model(self, data: bytes) -> bool: #vers 2
        """Load single COL model

        Args:
            data: File data bytes

        Returns:
            True if successful
        """
        try:
            model, offset = self.parser.parse_model(data, 0)

            if model is None:
                self.load_error = "Parser returned None - check debug output for details"
                img_debugger.error(self.load_error)
                return False

            self.models.append(model)
            self.is_loaded = True

            if self.debug:
                img_debugger.success(f"Loaded 1 model: {model.name}")

            return True

        except Exception as e:
            import traceback
            self.load_error = f"Single model parse error: {str(e)}"
            img_debugger.error(self.load_error)
            img_debugger.error(traceback.format_exc())
            return False


    def _load_multi_model_archive(self, data: bytes) -> bool: #vers 1
        """Load multi-model COL archive

        Archives contain multiple independent COL models,
        each starting with its own signature

        Args:
            data: File data bytes

        Returns:
            True if at least one model loaded
        """
        try:
            if self.debug:
                img_debugger.info("Parsing multi-model archive...")

            # Parse models sequentially using file_size from headers
            offset = 0
            models_loaded = 0

            while offset < len(data) - 32:
                # Check for valid signature
                sig = data[offset:offset+4]
                if sig not in [b'COLL', b'COL', b'COL', b'COL']:
                    break

                # Read file_size from header to skip to next model
                file_size = struct.unpack('<I', data[offset+4:offset+8])[0]

                try:
                    model, _ = self.parser.parse_model(data, offset)

                    if model:
                        self.models.append(model)
                        models_loaded += 1
                        if self.debug:
                            img_debugger.debug(f"Model {models_loaded}: {model.name} loaded")

                    # Skip to next model using file_size
                    offset += file_size

                except Exception as e:
                    if self.debug:
                        img_debugger.warning(f"Model error: {str(e)}")
                    # Try to skip using file_size
                    offset += file_size if file_size > 0 else 100

            if not signatures:
                self.load_error = "No COL signatures found"
                return False

            # Parse each model
            models_loaded = 0
            for i, sig_offset in enumerate(signatures):
                try:
                    model, new_offset = self.parser.parse_model(data, sig_offset)

                    if model:
                        self.models.append(model)
                        models_loaded += 1
                        if self.debug:
                            img_debugger.debug(f"Archive model {i}: {model.name} loaded")
                    else:
                        if self.debug:
                            img_debugger.warning(f"Archive model {i}: parse failed")

                except Exception as e:
                    if self.debug:
                        img_debugger.warning(f"Archive model {i} error: {str(e)}")
                    continue

            if models_loaded > 0:
                self.is_loaded = True
                if self.debug:
                    img_debugger.success(f"Archive loaded: {models_loaded} models")
                return True
            else:
                self.load_error = "No models could be loaded from archive"
                return False

        except Exception as e:
            self.load_error = f"Archive parse error: {str(e)}"
            img_debugger.error(self.load_error)
            return False


    def load_from_file(self, file_path: str) -> bool:
        """Load COL file from disk

        Args:
            file_path: Path to COL file

        Returns:
            True if successful
        """
        try:
            self.file_path = file_path
            self.models = []
            self.is_loaded = False
            self.load_error = ""

            # Validate file exists
            if not os.path.exists(file_path):
                self.load_error = f"File not found: {file_path}"
                img_debugger.error(self.load_error)
                return False

            # Read file
            with open(file_path, 'rb') as f:
                data = f.read()

            file_size = len(data)
            if file_size < 32:
                self.load_error = "File too small for COL"
                img_debugger.error(self.load_error)
                return False

            if self.debug:
                img_debugger.info(f"Loading: {os.path.basename(file_path)} ({file_size} bytes)")

            # Parse models (COL files are archives of models, stored linearly)
            offset = 0
            model_count = 0

            while offset < len(data) - 32:
                # Try to parse model
                model, new_offset = self.parser.parse_model(data, offset)

                if model is None:
                    # No more valid models
                    if model_count == 0:
                        self.load_error = "Failed to parse any models"
                        img_debugger.error(self.load_error)
                        return False
                    else:
                        # We got some models, that's success
                        break

                self.models.append(model)
                model_count += 1

                # Check if offset advanced
                if new_offset <= offset:
                    if self.debug:
                        img_debugger.warning("Offset didn't advance, stopping")
                    break

                offset = new_offset

                # Safety limit
                if model_count >= 200:
                    if self.debug:
                        img_debugger.warning("Hit 200 model limit, stopping")
                    break

            if len(self.models) > 0:
                self.is_loaded = True
                if self.debug:
                    img_debugger.success(f"Loaded {len(self.models)} models")
                return True
            else:
                self.load_error = "No models loaded"
                return False

        except Exception as e:
            import traceback
            self.load_error = f"Load error: {str(e)}"
            img_debugger.error(self.load_error)
            img_debugger.error(traceback.format_exc())
            return False


    def load_from_data(self, data: bytes, name: str = "unknown.col") -> bool: #vers 1
        """
        Load COL from raw bytes
        
        Returns: True if successful
        """
        try:
            self.raw_data = data
            self.file_path = name
            
            # Parse models
            self.models = self._parse_all_models(data)
            
            if self.debug:
                print(f"Loaded {len(self.models)} models from data")
            
            return True
            
        except Exception as e:
            if self.debug:
                print(f"Error loading COL data: {e}")
            return False
    

    def get_model_count(self) -> int: #vers 1
        """Get number of models in file"""
        return len(self.models)
    

    def get_model(self, index: int) -> Optional[COLModel]: #vers 1
        """Get model by index"""
        if 0 <= index < len(self.models):
            return self.models[index]
        return None
    

    def get_model_by_name(self, name: str) -> Optional[COLModel]: #vers 1
        """Get model by name"""
        for model in self.models:
            if model.header.name == name:
                return model
        return None
    

    def get_stats(self) -> dict: #vers 1
        """Get file statistics"""
        total_spheres = sum(len(m.spheres) for m in self.models)
        total_boxes = sum(len(m.boxes) for m in self.models)
        total_vertices = sum(len(m.vertices) for m in self.models)
        total_faces = sum(len(m.faces) for m in self.models)
        
        versions = set(m.header.version for m in self.models)
        
        return {
            'file_path': self.file_path,
            'model_count': len(self.models),
            'versions': [v.name for v in versions],
            'total_spheres': total_spheres,
            'total_boxes': total_boxes,
            'total_vertices': total_vertices,
            'total_faces': total_faces,
            'file_size': len(self.raw_data) if self.raw_data else 0
        }
    

    def get_info(self) -> str:
        """Get file info summary"""
        lines = []

        filename = os.path.basename(self.file_path) if self.file_path else "Unknown"
        lines.append(f"File: {filename}")
        lines.append(f"Models: {len(self.models)}")
        lines.append("")

        for i, model in enumerate(self.models):
            lines.append(f"Model {i}: {model.name}")
            lines.append(f"  Version: {model.version.name}")
            lines.append(f"  ID: {model.model_id}")
            lines.append(f"  Spheres: {len(model.spheres)}")
            lines.append(f"  Boxes: {len(model.boxes)}")
            lines.append(f"  Vertices: {len(model.vertices)}")
            lines.append(f"  Faces: {len(model.faces)}")
            lines.append("")

        return "\n".join(lines)


    def is_multi_model(self) -> bool: #vers 1
        """Check if file contains multiple models"""
        is_multi_model_archive()
        return len(self.models) > 1
    

    def is_multi_model_archive(self, data: bytes) -> bool: #vers 1
        """Check if file contains multiple COL models

        Multi-model archives have multiple COL signatures in the file

        Args:
            data: File data bytes

        Returns:
            True if multi-model archive
        """
        try:
            signature_count = 0
            offset = 0

            # Scan for COL signatures
            while offset < len(data) - 4:
                sig = data[offset:offset+4]
                if sig in [b'COLL', b'COL\x02', b'COL\x03', b'COL\x04']:
                    signature_count += 1
                    if signature_count > 1:
                        if self.debug:
                            img_debugger.debug(f"Multi-model archive detected ({signature_count} signatures)")
                        return True
                    # Skip ahead to avoid counting same signature
                    offset += 100
                else:
                    offset += 1

            return False

        except Exception:
            return False


    def _parse_all_models(self, data: bytes) -> List[COLModel]: #vers 1
        """
        Parse all models from data
        COL files have no header - models stored linearly
        """
        models = []
        offset = 0
        
        while offset < len(data):
            try:
                # Check if we have enough data for a header
                if len(data) < offset + 8:
                    break
                
                # Peek at FourCC to validate
                fourcc = data[offset:offset+4]
                if fourcc not in [b'COLL', b'COL2', b'COL3', b'COL4']:
                    if self.debug:
                        print(f"Invalid FourCC at offset {offset}: {fourcc}")
                    break
                
                # Parse model (COL1 only for now)
                if fourcc == b'COLL':
                    model, new_offset = self.parser.parse_col1_model(data, offset)
                    models.append(model)
                    
                    # Move to next model
                    # Size in header is from after size field, so add 8 for fourcc+size
                    offset = new_offset  # Use parser returned offset
                else:
                    # COL2/3/4 not implemented yet
                    if self.debug:
                        print(f"COL2/3/4 parsing not implemented yet")
                    break
                    
            except Exception as e:
                if self.debug:
                    print(f"Error parsing model at offset {offset}: {e}")
                break
        
        return models
    

    def validate(self) -> Tuple[bool, List[str]]: #vers 1
        """
        Validate COL file
        
        Returns: (is_valid, list_of_errors)
        """
        errors = []
        
        if not self.raw_data:
            errors.append("No data loaded")
            return False, errors
        
        if len(self.models) == 0:
            errors.append("No valid models found")
            return False, errors
        
        # Validate each model
        for i, model in enumerate(self.models):
            # Check for zero-length model name
            if not model.header.name:
                errors.append(f"Model {i}: Empty name")
            
            # Check vertex indices in faces
            num_verts = len(model.vertices)
            for j, face in enumerate(model.faces):
                if face.a >= num_verts or face.b >= num_verts or face.c >= num_verts:
                    errors.append(f"Model {i}, Face {j}: Invalid vertex index")
        
        return len(errors) == 0, errors


__all__ = ['COLFile']
