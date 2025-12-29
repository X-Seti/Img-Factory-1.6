#this belongs in methods/col_validation.py - Version: 2
# X-Seti - July17 2025 - IMG Factory 1.5 - COL Validation with IMG Debug System

"""
COL Validation Functions - Using IMG Debug Framework
Handles validation of COL files using the proven IMG debug system
Converted from img_validator.py template
"""

import os
import struct
from typing import Dict, List, Optional
from pathlib import Path

# Import IMG debug system (proven to work)
from apps.debug.debug_functions import img_debugger, debug_trace

## Methods list -
# validate_col_file
# validate_col_file_structure
# validate_col_file_complete
# validate_col_creation_settings
# validate_col_for_import
# _validate_col_format
# _validate_col_structure
# _validate_col_entries

class COLValidationResult:
    """COL validation result using IMG pattern"""
    
    def __init__(self):
        self.is_valid = True
        self.errors = []
        self.warnings = []
        self.info = []
    
    def add_error(self, message: str):
        """Add error message"""
        self.errors.append(message)
        self.is_valid = False
        img_debugger.error(f"COL VALIDATION ERROR: {message}")
    
    def add_warning(self, message: str):
        """Add warning message"""
        self.warnings.append(message)
        img_debugger.warning(f"COL VALIDATION WARNING: {message}")
    
    def add_info(self, message: str):
        """Add info message"""
        self.info.append(message)
        img_debugger.info(f"COL VALIDATION INFO: {message}")
    
    def get_summary(self) -> str:
        """Get validation summary"""
        summary = []
        if self.errors:
            summary.append(f"Errors: {len(self.errors)}")
        if self.warnings:
            summary.append(f"Warnings: {len(self.warnings)}")
        if self.info:
            summary.append(f"Info: {len(self.info)}")
        return ", ".join(summary) if summary else "No issues"


class COLValidator:
    """COL file validator using IMG debug framework"""
    
    # Known COL signatures for validation
    KNOWN_SIGNATURES = {
        b'COL\x01': 'COL Version 1',
        b'COL\x02': 'COL Version 2', 
        b'COL\x03': 'COL Version 3',
        b'COL\x04': 'COL Version 4',
        b'COLL': 'COL Version 1 (COLL)',
    }
    
    # Maximum reasonable COL file sizes (in MB)
    MAX_COL_SIZE_MB = 50
    
    @staticmethod
    @debug_trace
    def validate_col_file(col_file_path: str) -> COLValidationResult: #vers 1
        """Validate complete COL file using IMG debug system"""
        result = COLValidationResult()
        
        img_debugger.debug(f"Starting COL validation: {col_file_path}")
        
        # Basic file existence and access
        if not os.path.exists(col_file_path):
            result.add_error(f"COL file does not exist: {col_file_path}")
            return result
        
        # File size validation  
        file_size = os.path.getsize(col_file_path)
        if file_size == 0:
            result.add_error("COL file is empty")
            return result
        
        if file_size > COLValidator.MAX_COL_SIZE_MB * 1024 * 1024:
            result.add_warning(f"COL file is very large ({file_size / (1024*1024):.1f} MB)")
        
        # Read and validate file content
        try:
            with open(col_file_path, 'rb') as f:
                data = f.read()
            
            COLValidator._validate_col_format(data, result)
            COLValidator._validate_col_structure(data, result)
            
        except Exception as e:
            result.add_error(f"Failed to read COL file: {str(e)}")
        
        img_debugger.success(f"COL validation completed: {result.get_summary()}")
        return result
    
    def _validate_file_size(self, file_path: str, result: COLValidationResult): #vers 2
        """Validate COL file size - FIXED for multi-model archives"""
        try:
            actual_size = os.path.getsize(file_path)

            with open(file_path, 'rb') as f:
                # Read first model header
                signature = f.read(4)
                if len(signature) < 4:
                    result.add_error("File too small for COL header")
                    return

                declared_size = struct.unpack('<I', f.read(4))[0]

            # FIXED: Handle multi-model COL files
            if signature in [b'COLL', b'COL\x02', b'COL\x03', b'COL\x04']:
                # For single model: declared_size + 8 should equal actual_size
                expected_single = declared_size + 8

                if actual_size == expected_single:
                    result.add_info(f"File size matches single model: {actual_size} bytes")
                elif actual_size > expected_single:
                    # This is likely a multi-model COL archive
                    model_count = self._count_col_models(file_path)
                    if model_count > 1:
                        result.add_info(f"Multi-model COL archive detected: {model_count} models, {actual_size} bytes")
                    else:
                        result.add_warning(f"Size mismatch: first model declares {declared_size}, actual file {actual_size}")
                else:
                    result.add_error(f"File truncated: expected {expected_single}, actual {actual_size}")
            else:
                result.add_error(f"Invalid COL signature: {signature}")

        except Exception as e:
            result.add_error(f"Error validating file size: {str(e)}")

    def _count_col_models(self, file_path: str) -> int: #vers 1
        """Count number of COL models in file"""
        try:
            model_count = 0
            with open(file_path, 'rb') as f:
                data = f.read()
                offset = 0

                while offset < len(data) - 8:
                    signature = data[offset:offset+4]
                    if signature in [b'COLL', b'COL\x02', b'COL\x03', b'COL\x04']:
                        model_count += 1
                        try:
                            # Read model size and skip to next model
                            model_size = struct.unpack('<I', data[offset+4:offset+8])[0]
                            offset += model_size + 8
                        except:
                            break
                    else:
                        offset += 1

            return model_count

        except Exception:
            return 1  # Default to single model

    @staticmethod
    def _validate_col_format(data: bytes, result: COLValidationResult): #vers 1
        """Validate COL file format and signature"""
        if len(data) < 8:
            result.add_error("COL file too small to contain valid header")
            return
        
        # Check COL signature
        signature = data[:4]
        
        if signature in COLValidator.KNOWN_SIGNATURES:
            version_name = COLValidator.KNOWN_SIGNATURES[signature]
            result.add_info(f"Valid COL signature detected: {version_name}")
            
            # Version-specific validation
            if signature == b'COL\x01':
                COLValidator._validate_col1_format(data, result)
            elif signature == b'COL\x02':
                COLValidator._validate_col2_format(data, result)
            elif signature == b'COL\x03':
                COLValidator._validate_col3_format(data, result)
            elif signature == b'COL\x04':
                COLValidator._validate_col4_format(data, result)
            elif signature == b'COLL':
                COLValidator._validate_coll_format(data, result)
        else:
            result.add_warning(f"Unknown COL signature: {signature.hex()}")
    
    @staticmethod
    def _validate_col_structure(data: bytes, result: COLValidationResult): #vers 1
        """Validate COL file internal structure"""
        if len(data) < 8:
            return
        
        try:
            # Read declared file size
            declared_size = struct.unpack('<I', data[4:8])[0]
            actual_size = len(data) - 8  # Subtract header size
            
            if declared_size != actual_size:
                result.add_warning(f"Size mismatch: declared {declared_size}, actual {actual_size}")
            
            # Check if file size is reasonable
            if declared_size > 0 and declared_size < 32:
                result.add_warning("COL data section very small")
            
        except struct.error:
            result.add_error("COL file has malformed size header")
    
    @staticmethod
    def _validate_col1_format(data: bytes, result: COLValidationResult): #vers 1
        """Validate COL Version 1 specific format"""
        result.add_info("Validating COL Version 1 format")
        # Add COL1-specific validation logic here
    
    @staticmethod
    def _validate_col2_format(data: bytes, result: COLValidationResult): #vers 1
        """Validate COL Version 2 specific format"""
        result.add_info("Validating COL Version 2 format")
        # Add COL2-specific validation logic here
    
    @staticmethod
    def _validate_col3_format(data: bytes, result: COLValidationResult): #vers 1
        """Validate COL Version 3 specific format"""
        result.add_info("Validating COL Version 3 format")
        # Add COL3-specific validation logic here
    
    @staticmethod
    def _validate_col4_format(data: bytes, result: COLValidationResult): #vers 1
        """Validate COL Version 4 specific format"""
        result.add_info("Validating COL Version 4 format")
        # Add COL4-specific validation logic here
    
    @staticmethod
    def _validate_coll_format(data: bytes, result: COLValidationResult): #vers 1
        """Validate COLL format specific structure"""
        result.add_info("Validating COLL format")
        # Add COLL-specific validation logic here


# Legacy compatibility functions (keeping existing interface)
def validate_col_file(main_window, file_path: str) -> bool: #vers 1
    """Legacy compatibility function using new validator"""
    try:
        result = COLValidator.validate_col_file(file_path)
        
        # Log results using main window
        if hasattr(main_window, 'log_message'):
            if result.errors:
                for error in result.errors:
                    main_window.log_message(f"❌ {error}")
            if result.warnings:
                for warning in result.warnings:
                    main_window.log_message(f"⚠️ {warning}")
            if result.info:
                for info in result.info:
                    main_window.log_message(f"ℹ️ {info}")
        
        return result.is_valid
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ COL validation error: {str(e)}")
        return False


def validate_col_file_structure(file_path: str) -> tuple[bool, str]: #vers 1
    """Legacy compatibility for structure validation"""
    try:
        result = COLValidator.validate_col_file(file_path)
        
        if result.is_valid:
            summary = result.get_summary()
            return True, summary if summary != "No issues" else "Valid COL file"
        else:
            return False, "; ".join(result.errors)
            
    except Exception as e:
        return False, f"Validation error: {str(e)}"


def validate_col_file_complete(main_window, file_path: str) -> bool: #vers 1
    """Complete COL validation with detailed logging"""
    return validate_col_file(main_window, file_path)


def validate_col_for_import(file_path: str) -> COLValidationResult: #vers 1
    """Validate COL file for import operations"""
    result = COLValidator.validate_col_file(file_path)
    
    # Additional import-specific checks
    if result.is_valid:
        file_size = os.path.getsize(file_path)
        if file_size > 10 * 1024 * 1024:  # 10MB
            result.add_warning("Large COL file may take time to import")
    
    return result


def validate_col_creation_settings(settings: Dict) -> COLValidationResult: #vers 1
    """Validate settings for COL creation"""
    result = COLValidationResult()
    
    # Required fields
    required_fields = ['output_path', 'col_version']
    for field in required_fields:
        if field not in settings:
            result.add_error(f"Missing required setting: {field}")
    
    if not result.is_valid:
        return result
    
    # Validate output path
    output_path = settings['output_path']
    output_dir = os.path.dirname(output_path)
    
    if not os.path.exists(output_dir):
        result.add_error(f"Output directory does not exist: {output_dir}")
    
    if os.path.exists(output_path):
        result.add_warning("Output file already exists and will be overwritten")
    
    # Validate filename
    filename = os.path.basename(output_path)
    if not filename.lower().endswith('.col'):
        result.add_warning("Output filename should have .col extension")
    
    return result


# Export functions
__all__ = [
    'COLValidator',
    'COLValidationResult',
    'validate_col_file',
    'validate_col_file_structure', 
    'validate_col_file_complete',
    'validate_col_for_import',
    'validate_col_creation_settings'
]
