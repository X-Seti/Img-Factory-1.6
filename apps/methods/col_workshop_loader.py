#this belongs in methods/col_workshop_loader.py - Version: 2
# X-Seti - December21 2025 / March2026 - Col Workshop - COL File Loader
"""
COL File Loader — high-level interface for loading COL files.
Supports single-model and multi-model COL archives (COL1/2/3/4).

All parsing goes through _parse_all_models to avoid load_from_file /
load / load_from_data diverging in behaviour.

##Methods list -
# COLFile
#   __init__
#   load
#   load_from_file
#   load_from_data
#   _load_bytes
#   _parse_all_models
#   get_model_count
#   get_model
#   get_model_by_name
#   get_stats
#   get_info
#   is_multi_model
#   validate
"""

import os
import struct
from typing import List, Optional, Tuple

from apps.methods.col_workshop_parser import COLParser
from apps.methods.col_workshop_classes import COLModel, COLVersion
from apps.debug.debug_functions import img_debugger

_VALID_FOURCCS = {b'COLL', b'COL2', b'COL3', b'COL4'}


class COLFile: #vers 2
    """High-level COL file interface."""

    def __init__(self, file_path: Optional[str] = None, debug: bool = False): #vers 2
        self.parser     = COLParser(debug=debug)
        self.debug      = debug
        self.models: List[COLModel] = []
        self.is_loaded  = False
        self.load_error = ""
        self.file_path  = file_path
        self.raw_data: Optional[bytes] = None

    # ── Public load API ───────────────────────────────────────────────────

    def load(self, file_path: str = None) -> bool: #vers 2
        """Load COL file from disk. Uses self.file_path if no arg given."""
        path = file_path or self.file_path
        if not path or not isinstance(path, str):
            self.load_error = f"Invalid file path: {path!r}"
            return False
        with open(path, 'rb') as f:
            data = f.read()
        self.file_path = path
        return self._load_bytes(data)

    def load_from_file(self, file_path: str = None) -> bool: #vers 3
        """Load COL file from disk."""
        if not file_path or not isinstance(file_path, str):
            self.load_error = f"Invalid file path: {file_path!r}"
            return False
        if not os.path.exists(file_path):
            self.load_error = f"File not found: {file_path}"
            img_debugger.error(self.load_error)
            return False

        self.file_path  = file_path
        self.models     = []
        self.is_loaded  = False
        self.load_error = ""

        if os.path.getsize(file_path) < 32:
            self.load_error = "File too small for COL"
            img_debugger.error(self.load_error)
            return False

        if self.debug:
            img_debugger.info(f"Loading: {os.path.basename(file_path)}")

        with open(file_path, 'rb') as f:
            data = f.read()
        return self._load_bytes(data)

    def load_from_data(self, data: bytes, name: str = "unknown.col") -> bool: #vers 2
        """Load COL from raw bytes."""
        self.file_path  = name
        self.models     = []
        self.is_loaded  = False
        self.load_error = ""
        return self._load_bytes(data)

    # ── Single internal parse path ────────────────────────────────────────

    def _load_bytes(self, data: bytes) -> bool: #vers 1
        """Parse bytes — single code path for all load methods."""
        try:
            self.raw_data = data
            self.models   = self._parse_all_models(data)
            if self.models:
                self.is_loaded = True
                if self.debug:
                    img_debugger.success(f"Loaded {len(self.models)} model(s)")
                return True
            self.load_error = "No models could be parsed"
            img_debugger.error(self.load_error)
            return False
        except Exception as e:
            import traceback
            self.load_error = f"Load error: {e}"
            img_debugger.error(self.load_error)
            img_debugger.error(traceback.format_exc())
            return False

    def _parse_all_models(self, data: bytes) -> List[COLModel]: #vers 3
        """
        Parse all COL models from a buffer.
        COL files are linear archives: FOURCC + size + payload, repeat.
        """
        models = []
        offset = 0

        while offset < len(data) - 8:
            fourcc = data[offset:offset + 4]
            if fourcc not in _VALID_FOURCCS:
                if self.debug:
                    print(f"_parse_all_models: unexpected at 0x{offset:X}: {fourcc.hex()}")
                # Scan forward for a valid FOURCC
                found = False
                for skip in range(1, min(64, len(data) - offset - 8)):
                    if data[offset + skip:offset + skip + 4] in _VALID_FOURCCS:
                        offset += skip
                        found = True
                        break
                if not found:
                    break
                fourcc = data[offset:offset + 4]

            block_size  = struct.unpack_from('<I', data, offset + 4)[0]
            next_offset = offset + 8 + block_size   # fourcc(4)+size(4)+payload

            try:
                model, parsed_end = self.parser.parse_model(data, offset)
                if model is not None:
                    models.append(model)
                    if self.debug:
                        print(f"  model[{len(models)-1}]: {model.name!r} {model.version}")
                    offset = parsed_end if parsed_end > offset else next_offset
                else:
                    if self.debug:
                        print(f"  parse_model None at 0x{offset:X}, skipping block")
                    offset = next_offset
            except Exception as e:
                if self.debug:
                    print(f"  exception at 0x{offset:X}: {e}")
                offset = next_offset

        return models

    # ── Query helpers ─────────────────────────────────────────────────────

    def get_model_count(self) -> int: #vers 1
        return len(self.models)

    def get_model(self, index: int) -> Optional[COLModel]: #vers 1
        return self.models[index] if 0 <= index < len(self.models) else None

    def get_model_by_name(self, name: str) -> Optional[COLModel]: #vers 1
        for model in self.models:
            if model.name == name:
                return model
        return None

    def is_multi_model(self) -> bool: #vers 2
        """Return True if more than one model was loaded."""
        return len(self.models) > 1

    def get_stats(self) -> dict: #vers 1
        return {
            'file_path':      self.file_path,
            'model_count':    len(self.models),
            'versions':       list({m.version.name for m in self.models}),
            'total_spheres':  sum(len(m.spheres)  for m in self.models),
            'total_boxes':    sum(len(m.boxes)     for m in self.models),
            'total_vertices': sum(len(m.vertices)  for m in self.models),
            'total_faces':    sum(len(m.faces)     for m in self.models),
            'file_size':      len(self.raw_data) if self.raw_data else 0,
        }

    def get_info(self) -> str: #vers 2
        lines = []
        filename = os.path.basename(self.file_path) if self.file_path else "Unknown"
        lines.append(f"File: {filename}")
        lines.append(f"Models: {len(self.models)}")
        lines.append("")
        for i, model in enumerate(self.models):
            lines.append(f"Model {i}: {model.name}")
            lines.append(f"  Version:  {model.version.name}")
            lines.append(f"  ID:       {model.model_id}")
            lines.append(f"  Spheres:  {len(model.spheres)}")
            lines.append(f"  Boxes:    {len(model.boxes)}")
            lines.append(f"  Vertices: {len(model.vertices)}")
            lines.append(f"  Faces:    {len(model.faces)}")
            lines.append("")
        return "\n".join(lines)

    def validate(self) -> Tuple[bool, List[str]]: #vers 2
        errors = []
        if not self.raw_data:
            errors.append("No data loaded")
            return False, errors
        if not self.models:
            errors.append("No valid models found")
            return False, errors
        for i, model in enumerate(self.models):
            if not model.name:
                errors.append(f"Model {i}: empty name")
            num_verts = len(model.vertices)
            for j, face in enumerate(model.faces):
                if face.a >= num_verts or face.b >= num_verts or face.c >= num_verts:
                    errors.append(f"Model {i}, Face {j}: invalid vertex index "
                                  f"(a={face.a} b={face.b} c={face.c}, verts={num_verts})")
        return len(errors) == 0, errors


__all__ = ['COLFile']
