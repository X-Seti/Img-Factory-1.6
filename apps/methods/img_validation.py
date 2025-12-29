#this belongs in methods/ img_validation.py - Version: 1
# X-Seti - September04 2025 - IMG Factory 1.5 - IMG Validation

"""
IMG Validation - File and Structure Validation
Validation functions extracted from img_core_classes.py
Used for IMG file integrity and structure validation
"""

import os
import struct
from typing import Optional, Any, List, Dict, Tuple
from pathlib import Path
from enum import Enum
from apps.methods.img_core_classes import IMGFile, IMGEntry, IMGVersion
from apps.methods.rw_versions import is_valid_rw_version

##Methods list -
# validate_img_file_structure
# validate_img_entries
# validate_entry_integrity
# check_file_corruption
# validate_rw_data
# repair_img_structure
# get_validation_report
# ValidationResult
# integrate_validation_functions

##Classes -
# ValidationResult
# ValidationLevel

class ValidationLevel(Enum):
    """Validation severity levels"""
    INFO = "info"
    WARNING = "warning" 
    ERROR = "error"
    CRITICAL = "critical"


class ValidationResult:
    """Validation result container"""
    def __init__(self, level: ValidationLevel, message: str, details: str = ""): #vers 1
        self.level = level
        self.message = message
        self.details = details
        self.timestamp = None
        self.is_valid = is_valid
        self.warnings = warnings or []
        self.errors = errors or []
        self.info = []

        # Set timestamp
        try:
            from datetime import datetime
            self.timestamp = datetime.now()
        except ImportError:
            pass
    
    def __str__(self):
        return f"[{self.level.value.upper()}] {self.message}"
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization"""
        return {
            'level': self.level.value,
            'message': self.message,
            'details': self.details,
            'timestamp': str(self.timestamp) if self.timestamp else None
        }

    def add_warning(self, message: str):
        """Add a warning message"""
        self.warnings.append(message)

    def add_error(self, message: str):
        """Add an error message"""
        self.errors.append(message)
        self.is_valid = False

    def add_info(self, message: str):
        """Add an info message"""
        self.info.append(message)

    def get_summary(self) -> str:
        """Get validation summary"""
        if self.is_valid:
            if self.warnings:
                return f"Valid with {len(self.warnings)} warnings"
            else:
                return "Valid"
        else:
            return f"Invalid - {len(self.errors)} errors, {len(self.warnings)} warnings"

    def get_details(self) -> str:
        """Get detailed validation report"""
        details = []

        if self.errors:
            details.append("ERRORS:")
            for error in self.errors:
                details.append(f"  • {error}")

        if self.warnings:
            details.append("WARNINGS:")
            for warning in self.warnings:
                details.append(f"  • {warning}")

        if self.info:
            details.append("INFO:")
            for info in self.info:
                details.append(f"  • {info}")

        return "\n".join(details) if details else "No issues found."


class IMGValidator:
    """Comprehensive IMG file validator"""

    # Known file signatures for validation
    KNOWN_SIGNATURES = {
        'DFF': [b'\x10\x00\x00\x00', b'\x0E\x00\x00\x00'],  # RenderWare DFF
        'TXD': [b'\x16\x00\x00\x00'],  # RenderWare TXD
        'COL': [b'COL\x01', b'COL\x02', b'COL\x03', b'COL\x04', b'COLL'],  # Collision
        'IFP': [b'ANPK'],  # Animation package
        'SCM': [b'\x03\x00', b'\x04\x00'],  # Script
    }

    # Maximum reasonable file sizes (in MB)
    MAX_FILE_SIZES = {
        'DFF': 50,   # Model files
        'TXD': 100,  # Texture files
        'COL': 10,   # Collision files
        'IFP': 20,   # Animation files
        'SCM': 5,    # Script files
        'IPL': 1,    # Item placement
        'IDE': 1,    # Item definition
        'DAT': 1,    # Data files
    }

    @staticmethod
    def validate_img_file(img_file: IMGFile) -> ValidationResult:
        """Validate an entire IMG file"""
        result = ValidationResult()

        if not img_file:
            result.add_error("IMG file object is None")
            return result

        # Basic file validation
        if not os.path.exists(img_file.file_path):
            result.add_error(f"IMG file does not exist: {img_file.file_path}")
            return result

        # File size validation
        file_size = os.path.getsize(img_file.file_path)
        if file_size == 0:
            result.add_error("IMG file is empty")
            return result

        if file_size > 2 * 1024 * 1024 * 1024:  # 2GB
            result.add_warning("IMG file is very large (>2GB)")

        # Version-specific validation
        IMGValidator._validate_img_version(img_file, result)

        # Entry validation
        IMGValidator._validate_img_entries(img_file, result)

        # Structure validation
        IMGValidator._validate_img_structure(img_file, result)

        return result

    @staticmethod
    def _validate_img_version(img_file: IMGFile, result: ValidationResult):
        """Validate IMG version-specific aspects"""
        version = img_file.version

        if version == IMGVersion.UNKNOWN:
            result.add_error("Unknown IMG version")
            return

        # Version 1 specific validation TODO, need to seprate SOL imgs version 1,5
        if version == IMGVersion.VERSION_1:
            dir_path = img_file.file_path.replace('.img', '.dir')
            if not os.path.exists(dir_path):
                result.add_error(f"DIR file missing for IMG Version 1: {dir_path}")
            else:
                # Validate DIR file size
                dir_size = os.path.getsize(dir_path)
                expected_size = len(img_file.entries) * 32
                if dir_size != expected_size:
                    result.add_warning(f"DIR file size mismatch. Expected: {expected_size}, Actual: {dir_size}")

        # Version 3 specific validation
        elif version == IMGVersion.VERSION_3:
            if img_file.is_encrypted and img_file.encryption_type == 0:
                result.add_warning("IMG Version 3 marked as encrypted but encryption type is unknown")

        # Fastman92 specific validation
        elif version == IMGVersion.FASTMAN92:
            if img_file.is_encrypted:
                result.add_warning("Fastman92 format encryption is not fully supported")

            if img_file.game_type != 0:
                result.add_warning(f"Fastman92 format with non-standard game type: {img_file.game_type}")

    @staticmethod
    def _validate_img_entries(img_file: IMGFile, result: ValidationResult):
        """Validate all entries in the IMG file"""
        entries = img_file.entries

        if not entries:
            result.add_warning("IMG file contains no entries")
            return

        # Check for duplicate names
        names = [entry.name.upper() for entry in entries]
        duplicates = set([name for name in names if names.count(name) > 1])
        if duplicates:
            result.add_error(f"Duplicate entry names found: {', '.join(duplicates)}")

        # Validate individual entries
        total_size = 0
        offset_ranges = []

        for i, entry in enumerate(entries):
            entry_result = IMGValidator.validate_img_entry(entry, img_file)

            # Merge entry validation results
            result.warnings.extend([f"Entry '{entry.name}': {w}" for w in entry_result.warnings])
            result.errors.extend([f"Entry '{entry.name}': {e}" for e in entry_result.errors])
            if not entry_result.is_valid:
                result.is_valid = False

            # Track size and offsets
            total_size += entry.size
            offset_ranges.append((entry.offset, entry.offset + entry.size))

        # Check for overlapping entries
        sorted_ranges = sorted(offset_ranges)
        for i in range(len(sorted_ranges) - 1):
            current_end = sorted_ranges[i][1]
            next_start = sorted_ranges[i + 1][0]
            if current_end > next_start:
                result.add_error(f"Overlapping entries detected at offset {next_start}")

        # File size validation
        file_size = os.path.getsize(img_file.file_path)
        if total_size > file_size:
            result.add_error(f"Total entry size ({total_size}) exceeds file size ({file_size})")

        result.add_info(f"Validated {len(entries)} entries")

    @staticmethod
    def _validate_img_structure(img_file: IMGFile, result: ValidationResult):
        """Validate IMG file structure and layout"""
        version = img_file.version

        # Check sector alignment for appropriate versions
        if version in [IMGVersion.VERSION1_5, IMGVersion.VERSION_2, IMGVersion.VERSION_3]:
            for entry in img_file.entries:
                if entry.offset % 2048 != 0:
                    result.add_warning(f"Entry '{entry.name}' not aligned to 2048-byte boundary")

        # Check for gaps between entries
        sorted_entries = sorted(img_file.entries, key=lambda e: e.offset)
        for i in range(len(sorted_entries) - 1):
            current_end = sorted_entries[i].offset + sorted_entries[i].size
            next_start = sorted_entries[i + 1].offset
            gap = next_start - current_end

            if gap > 0:
                if version in [IMGVersion.VERSION1_5, IMGVersion.VERSION_2, IMGVersion.VERSION_3]:
                    # Large gaps might indicate corruption
                    if gap > 4096:  # 4KB gap
                        result.add_warning(f"Large gap ({gap} bytes) between entries")
                else:
                    # Version 1 should have no gaps
                    result.add_warning(f"Gap ({gap} bytes) between entries in Version 1 IMG")

    @staticmethod
    def validate_img_entry(entry: IMGEntry, img_file: IMGFile = None) -> ValidationResult:
        """Validate an individual IMG entry"""
        result = ValidationResult()

        # Basic entry validation
        if not entry:
            result.add_error("Entry object is None")
            return result

        # Name validation
        if not entry.name:
            result.add_error("Entry has no name")
        else:
            # Check for invalid characters
            invalid_chars = ['/', '\\', ':', '*', '?', '"', '<', '>', '|']
            if any(char in entry.name for char in invalid_chars):
                result.add_error(f"Entry name contains invalid characters: {entry.name}")

            # Check name length
            if len(entry.name) > 24:
                result.add_warning(f"Entry name exceeds 24 characters: {entry.name}")
            elif len(entry.name) == 0:
                result.add_error("Entry name is empty")

        # Size validation
        if entry.size < 0:
            result.add_error(f"Entry has negative size: {entry.size}")
        elif entry.size == 0:
            result.add_warning(f"Entry is empty: {entry.name}")
        elif entry.size > 100 * 1024 * 1024:  # 100MB
            result.add_warning(f"Entry is very large: {entry.name} ({entry.size} bytes)")

        # Offset validation
        if entry.offset < 0:
            result.add_error(f"Entry has negative offset: {entry.offset}")

        # Extension-based validation
        if hasattr(entry, 'extension') and entry.extension:
            ext = entry.extension.upper()

            # Check against known extensions
            if ext in IMGValidator.MAX_FILE_SIZES:
                max_size_mb = IMGValidator.MAX_FILE_SIZES[ext]
                max_size_bytes = max_size_mb * 1024 * 1024
                if entry.size > max_size_bytes:
                    result.add_warning(f"{ext} file is unusually large: {entry.name} ({entry.size} bytes)")

            # Validate file format if IMG file is available
            if img_file and img_file.is_open:
                try:
                    data = img_file.extract_entry_data(entry, max_bytes=1024)
                    format_result = IMGValidator._validate_entry_format(entry, data)
                    result.warnings.extend(format_result.warnings)
                    result.errors.extend(format_result.errors)
                    if not format_result.is_valid:
                        result.is_valid = False
                except Exception as e:
                    result.add_warning(f"Could not validate entry format: {str(e)}")

        return result

    @staticmethod
    def _validate_entry_format(entry: IMGEntry, data: bytes) -> ValidationResult:
        """Validate entry data format based on file extension"""
        result = ValidationResult()

        if not hasattr(entry, 'extension') or not entry.extension:
            return result

        ext = entry.extension.upper()

        if ext == 'DFF':
            IMGValidator._validate_dff_format(data, result)
        elif ext == 'TXD':
            IMGValidator._validate_txd_format(data, result)
        elif ext == 'COL':
            IMGValidator._validate_col_format(data, result)
        elif ext == 'IFP':
            IMGValidator._validate_ifp_format(data, result)
        elif ext == 'SCM':
            IMGValidator._validate_scm_format(data, result)
        elif ext in ['IPL', 'IDE', 'DAT']:
            IMGValidator._validate_text_format(data, result, ext)

        return result

    @staticmethod
    def _validate_dff_format(data: bytes, result: ValidationResult):
        """Validate DFF (model) file format"""
        if len(data) < 12:
            result.add_error("DFF file too small to contain valid header")
            return

        try:
            # Check RenderWare binary stream format
            section_type = struct.unpack('<I', data[0:4])[0]
            section_size = struct.unpack('<I', data[4:8])[0]
            version = struct.unpack('<I', data[8:12])[0]

            # Common RenderWare section types for DFF files
            valid_sections = [
                0x0001,  # Struct
                0x0002,  # String
                0x0003,  # Extension
                0x0006,  # Texture
                0x0007,  # Material
                0x0008,  # Material List
                0x000E,  # Atomic
                0x000F,  # Plane Section
                0x0010,  # World
                0x0014,  # Frame List
                0x0015,  # Geometry
                0x001A,  # Clump
            ]

            if section_type not in valid_sections:
                result.add_warning(f"Unusual DFF section type: 0x{section_type:08X}")

            # Version validation
            if version < 0x30000 or version > 0x3FFFF:
                result.add_warning(f"Unusual RenderWare version: 0x{version:08X}")

            # Size validation
            if section_size > len(data) - 12:
                result.add_warning("DFF section size exceeds available data")

        except struct.error:
            result.add_error("DFF file has malformed header")

    @staticmethod
    def _validate_txd_format(data: bytes, result: ValidationResult):
        """Validate TXD (texture) file format"""
        if len(data) < 12:
            result.add_error("TXD file too small to contain valid header")
            return

        try:
            section_type = struct.unpack('<I', data[0:4])[0]
            section_size = struct.unpack('<I', data[4:8])[0]
            version = struct.unpack('<I', data[8:12])[0]

            # TXD should start with texture dictionary section (0x16)
            if section_type != 0x16:
                result.add_warning(f"TXD doesn't start with texture dictionary section (found 0x{section_type:08X})")

            # Version validation
            if version < 0x30000 or version > 0x3FFFF:
                result.add_warning(f"Unusual RenderWare version: 0x{version:08X}")

        except struct.error:
            result.add_error("TXD file has malformed header")

    @staticmethod
    def _validate_col_format(data: bytes, result: ValidationResult):
        """Validate COL (collision) file format"""
        if len(data) < 4:
            result.add_error("COL file too small to contain valid header")
            return

        # Check COL signature
        signature = data[:4]
        valid_signatures = [b'COL\x01', b'COL\x02', b'COL\x03', b'COL\x04', b'COLL']

        if signature not in valid_signatures:
            result.add_warning("COL file doesn't have recognized signature")

        # Version-specific validation
        if signature.startswith(b'COL') and len(signature) == 4:
            version = signature[3]
            if version < 1 or version > 4:
                result.add_warning(f"Unusual COL version: {version}")

    @staticmethod
    def _validate_ifp_format(data: bytes, result: ValidationResult):
        """Validate IFP (animation) file format"""
        if len(data) < 4:
            result.add_error("IFP file too small to contain valid header")
            return

        # Check IFP signature
        if not data.startswith(b'ANPK'):
            result.add_warning("IFP file doesn't start with ANPK signature")

    @staticmethod
    def _validate_scm_format(data: bytes, result: ValidationResult):
        """Validate SCM (script) file format"""
        if len(data) < 4:
            result.add_error("SCM file too small to contain valid header")
            return

        # Check for common SCM signatures
        if data.startswith(b'\x03\x00') or data.startswith(b'\x04\x00'):
            result.add_info("Valid SCM script file detected")
        else:
            result.add_warning("SCM file doesn't start with expected signature")

    @staticmethod
    def _validate_text_format(data: bytes, result: ValidationResult, ext: str):
        """Validate text-based file formats (IPL, IDE, DAT)"""
        try:
            # Try to decode as text
            text = data.decode('utf-8', errors='ignore')

            # Check for binary data in text files
            null_count = text.count('\x00')
            if null_count > len(text) * 0.1:  # More than 10% null bytes
                result.add_warning(f"{ext} file appears to contain binary data")

            # Basic format checks
            lines = text.split('\n')
            if len(lines) < 2:
                result.add_warning(f"{ext} file has very few lines")

        except Exception:
            result.add_warning(f"Could not validate {ext} file as text")

    @staticmethod
    def validate_file_for_import(file_path: str) -> ValidationResult:
        """Validate a file before importing into IMG"""
        result = ValidationResult()

        if not os.path.exists(file_path):
            result.add_error(f"File does not exist: {file_path}")
            return result

        # Basic file checks
        file_size = os.path.getsize(file_path)
        if file_size == 0:
            result.add_error("File is empty")
            return result

        if file_size > 500 * 1024 * 1024:  # 500MB
            result.add_warning("File is very large (>500MB)")

        # Name validation
        filename = os.path.basename(file_path)
        if len(filename) > 24:
            result.add_warning(f"Filename is long ({len(filename)} chars) and may be truncated")

        # Check for invalid characters
        invalid_chars = ['/', '\\', ':', '*', '?', '"', '<', '>', '|']
        if any(char in filename for char in invalid_chars):
            result.add_error(f"Filename contains invalid characters: {filename}")

        # Extension validation
        extension = Path(file_path).suffix.upper().lstrip('.')
        if extension:
            # Create dummy entry for format validation
            dummy_entry = IMGEntry(filename, 0, file_size)
            dummy_entry.extension = extension

            try:
                with open(file_path, 'rb') as f:
                    data = f.read(min(1024, file_size))  # Read first 1KB for validation

                format_result = IMGValidator._validate_entry_format(dummy_entry, data)
                result.warnings.extend(format_result.warnings)
                result.errors.extend(format_result.errors)
                if not format_result.is_valid:
                    result.is_valid = False

            except Exception as e:
                result.add_warning(f"Could not read file for format validation: {str(e)}")

        return result

    @staticmethod
    def validate_img_creation_settings(settings: Dict) -> ValidationResult:
        """Validate settings for IMG creation"""
        result = ValidationResult()

        # Required fields
        required_fields = ['output_path', 'img_version', 'initial_size_mb']
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
        if not filename.lower().endswith('.img'):
            result.add_warning("Output filename should have .img extension")

        # Validate size
        size_mb = settings.get('initial_size_mb', 0)
        if size_mb <= 0:
            result.add_error("Initial size must be greater than 0")
        elif size_mb > 2048:  # 2GB
            result.add_warning("Very large initial size (>2GB)")

        # Validate version
        version = settings.get('img_version')
        if isinstance(version, str):
            try:
                version = IMGVersion[version]
            except KeyError:
                result.add_error(f"Unknown IMG version: {version}")

        return result

    @staticmethod
    def get_img_statistics(img_file: IMGFile) -> Dict:
        """Get detailed statistics about an IMG file"""
        stats = {
            'total_entries': len(img_file.entries),
            'total_size': 0,
            'file_types': {},
            'largest_file': None,
            'smallest_file': None,
            'average_size': 0,
            'version': img_file.version.name if img_file.version else 'Unknown',
            'file_size': os.path.getsize(img_file.file_path) if os.path.exists(img_file.file_path) else 0
        }

        if not img_file.entries:
            return stats

        sizes = []
        for entry in img_file.entries:
            stats['total_size'] += entry.size
            sizes.append(entry.size)

            # Track file types
            if hasattr(entry, 'extension') and entry.extension:
                ext = entry.extension.upper()
                stats['file_types'][ext] = stats['file_types'].get(ext, 0) + 1
            else:
                # Try to guess extension from name
                if '.' in entry.name:
                    ext = entry.name.split('.')[-1].upper()
                    stats['file_types'][ext] = stats['file_types'].get(ext, 0) + 1
                else:
                    stats['file_types']['Unknown'] = stats['file_types'].get('Unknown', 0) + 1

            # Track largest/smallest
            if stats['largest_file'] is None or entry.size > stats['largest_file']['size']:
                stats['largest_file'] = {'name': entry.name, 'size': entry.size}

            if stats['smallest_file'] is None or entry.size < stats['smallest_file']['size']:
                stats['smallest_file'] = {'name': entry.name, 'size': entry.size}

        stats['average_size'] = sum(sizes) // len(sizes) if sizes else 0

        return stats

    @staticmethod
    def suggest_optimizations(img_file: IMGFile) -> List[str]:
        """Suggest optimizations for an IMG file"""
        suggestions = []
        stats = IMGValidator.get_img_statistics(img_file)

        # Size-based suggestions
        if stats['total_entries'] > 1000:
            suggestions.append("Consider splitting into multiple IMG files for better performance")

        if stats['average_size'] < 1024:  # Less than 1KB average
            suggestions.append("Many small files detected - consider bundling related files")

        # Format suggestions
        file_types = stats['file_types']
        if 'TXD' in file_types and file_types['TXD'] > 50:
            suggestions.append("Many texture files - consider texture optimization")

        if 'DFF' in file_types and file_types['DFF'] > 100:
            suggestions.append("Many model files - verify all are necessary")

        # Version suggestions
        if img_file.version == IMGVersion.VERSION_1:
            suggestions.append("Consider upgrading to IMG Version 1.5 for better performance")

        return suggestions

    @staticmethod
    def recommend_fixes(validation: ValidationResult, stats: Dict = None) -> List[str]:
        """Recommend fixes for validation issues"""
        recommendations = []

        if validation.errors:
            recommendations.append("Fix critical errors before using this IMG file")

        if validation.warnings:
            recommendations.append("Review validation warnings for potential issues")

        # File type recommendations
        if stats and 'file_types' in stats:
            file_types = stats['file_types']
            if 'Unknown' in file_types and file_types['Unknown'] > 5:
                recommendations.append("Many files with unknown extensions - verify file types")

        return recommendations


def validate_img_file_structure(img_file) -> List[ValidationResult]: #vers 1
    """Validate IMG file structure and return issues"""
    results = []
    
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        
        # Check basic attributes
        if not hasattr(img_file, 'file_path'):
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                "IMG file missing file_path attribute"
            ))
            return results
        
        file_path = getattr(img_file, 'file_path', '')
        if not file_path:
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                "IMG file has empty file_path"
            ))
            return results
        
        # Check file existence
        if not os.path.exists(file_path):
            results.append(ValidationResult(
                ValidationLevel.CRITICAL,
                f"IMG file not found: {file_path}"
            ))
            return results
        
        # Check version attribute
        if not hasattr(img_file, 'version'):
            results.append(ValidationResult(
                ValidationLevel.WARNING,
                "IMG file missing version attribute"
            ))
        else:
            version = getattr(img_file, 'version', None)
            if version is None:
                results.append(ValidationResult(
                    ValidationLevel.WARNING,
                    "IMG file has null version"
                ))
        
        # Check entries attribute
        if not hasattr(img_file, 'entries'):
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                "IMG file missing entries attribute"
            ))
        else:
            entries = getattr(img_file, 'entries', [])
            if not isinstance(entries, list):
                results.append(ValidationResult(
                    ValidationLevel.ERROR,
                    "IMG entries is not a list"
                ))
            else:
                results.append(ValidationResult(
                    ValidationLevel.INFO,
                    f"IMG file has {len(entries)} entries"
                ))
        
        # Check platform attribute
        if hasattr(img_file, 'platform'):
            platform = getattr(img_file, 'platform', None)
            if platform:
                results.append(ValidationResult(
                    ValidationLevel.INFO,
                    f"IMG platform: {platform}"
                ))
        
        # File size check
        try:
            file_size = os.path.getsize(file_path)
            if file_size == 0:
                results.append(ValidationResult(
                    ValidationLevel.ERROR,
                    "IMG file is empty (0 bytes)"
                ))
            elif file_size < 2048:  # Minimum reasonable size
                results.append(ValidationResult(
                    ValidationLevel.WARNING,
                    f"IMG file very small ({file_size} bytes)"
                ))
            else:
                results.append(ValidationResult(
                    ValidationLevel.INFO,
                    f"IMG file size: {file_size} bytes"
                ))
        except OSError as e:
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                f"Cannot read IMG file size: {e}"
            ))
        
        if img_debugger:
            img_debugger.debug(f"IMG structure validation completed: {len(results)} issues found")
        
    except Exception as e:
        results.append(ValidationResult(
            ValidationLevel.CRITICAL,
            f"IMG structure validation failed: {str(e)}"
        ))
    
    return results


def validate_img_entries(img_file) -> List[ValidationResult]: #vers 1
    """Validate all entries in IMG file"""
    results = []
    
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        
        if not hasattr(img_file, 'entries'):
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                "No entries to validate"
            ))
            return results
        
        entries = getattr(img_file, 'entries', [])
        if not entries:
            results.append(ValidationResult(
                ValidationLevel.INFO,
                "IMG file has no entries"
            ))
            return results
        
        # Validate each entry
        duplicate_names = set()
        seen_names = set()
        
        for i, entry in enumerate(entries):
            # Validate individual entry
            entry_results = validate_entry_integrity(entry, i)
            results.extend(entry_results)
            
            # Check for duplicate names
            entry_name = getattr(entry, 'name', '')
            if entry_name:
                if entry_name.lower() in seen_names:
                    duplicate_names.add(entry_name)
                else:
                    seen_names.add(entry_name.lower())
        
        # Report duplicates
        for dup_name in duplicate_names:
            results.append(ValidationResult(
                ValidationLevel.WARNING,
                f"Duplicate entry name found: {dup_name}"
            ))
        
        # Summary
        results.append(ValidationResult(
            ValidationLevel.INFO,
            f"Validated {len(entries)} entries, {len(duplicate_names)} duplicates found"
        ))
        
        if img_debugger:
            img_debugger.debug(f"Entry validation completed for {len(entries)} entries")
        
    except Exception as e:
        results.append(ValidationResult(
            ValidationLevel.CRITICAL,
            f"Entry validation failed: {str(e)}"
        ))
    
    return results


def validate_entry_integrity(entry, index: int = -1) -> List[ValidationResult]: #vers 1
    """Validate single entry integrity"""
    results = []
    
    try:
        entry_id = f"Entry {index}" if index >= 0 else "Entry"
        
        # Check name
        if not hasattr(entry, 'name'):
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                f"{entry_id} missing name attribute"
            ))
        else:
            name = getattr(entry, 'name', '')
            if not name:
                results.append(ValidationResult(
                    ValidationLevel.ERROR,
                    f"{entry_id} has empty name"
                ))
            elif len(name) > 24:  # IMG filename limit
                results.append(ValidationResult(
                    ValidationLevel.WARNING,
                    f"{entry_id} name too long ({len(name)} chars): {name}"
                ))
        
        # Check size
        if not hasattr(entry, 'size'):
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                f"{entry_id} missing size attribute"
            ))
        else:
            size = getattr(entry, 'size', 0)
            if size < 0:
                results.append(ValidationResult(
                    ValidationLevel.ERROR,
                    f"{entry_id} has negative size: {size}"
                ))
            elif size == 0:
                results.append(ValidationResult(
                    ValidationLevel.WARNING,
                    f"{entry_id} has zero size"
                ))
        
        # Check offset
        if not hasattr(entry, 'offset'):
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                f"{entry_id} missing offset attribute"
            ))
        else:
            offset = getattr(entry, 'offset', 0)
            if offset < 0:
                results.append(ValidationResult(
                    ValidationLevel.ERROR,
                    f"{entry_id} has negative offset: {offset}"
                ))
        
        # Check for data availability
        has_data = False
        if hasattr(entry, 'data') and getattr(entry, 'data'):
            has_data = True
        elif hasattr(entry, '_cached_data') and getattr(entry, '_cached_data'):
            has_data = True
        elif hasattr(entry, 'get_data'):
            has_data = True
        
        if not has_data:
            results.append(ValidationResult(
                ValidationLevel.INFO,
                f"{entry_id} has no cached data (will read from file)"
            ))
        
    except Exception as e:
        results.append(ValidationResult(
            ValidationLevel.CRITICAL,
            f"Entry integrity check failed: {str(e)}"
        ))
    
    return results


def check_file_corruption(img_file) -> List[ValidationResult]: #vers 1
    """Check for signs of file corruption"""
    results = []
    
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        
        file_path = getattr(img_file, 'file_path', '')
        if not file_path or not os.path.exists(file_path):
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                "Cannot check corruption - file not accessible"
            ))
            return results
        
        # Check file header for Version 2 IMG
        try:
            with open(file_path, 'rb') as f:
                header = f.read(8)
                if len(header) >= 8:
                    # Check for VER2 signature
                    if header[:4] == b'VER2':
                        # Read entry count
                        entry_count = struct.unpack('<I', header[4:8])[0]
                        
                        # Sanity check entry count
                        if entry_count > 100000:  # Unreasonably high
                            results.append(ValidationResult(
                                ValidationLevel.ERROR,
                                f"Suspicious entry count: {entry_count} (possible corruption)"
                            ))
                        elif entry_count == 0:
                            results.append(ValidationResult(
                                ValidationLevel.INFO,
                                "IMG file has 0 entries in header"
                            ))
                        else:
                            results.append(ValidationResult(
                                ValidationLevel.INFO,
                                f"Header entry count: {entry_count}"
                            ))
                    else:
                        # No VER2 signature - might be Version 1 or corrupted
                        results.append(ValidationResult(
                            ValidationLevel.WARNING,
                            "No VER2 signature found (Version 1 or corrupted)"
                        ))
        except Exception as e:
            results.append(ValidationResult(
                ValidationLevel.ERROR,
                f"Header corruption check failed: {e}"
            ))
        
        # Check entry offset consistency
        if hasattr(img_file, 'entries'):
            entries = getattr(img_file, 'entries', [])
            file_size = os.path.getsize(file_path)
            
            for i, entry in enumerate(entries):
                offset = getattr(entry, 'offset', 0)
                size = getattr(entry, 'size', 0)
                
                # Check if entry extends beyond file
                if offset + size > file_size:
                    results.append(ValidationResult(
                        ValidationLevel.ERROR,
                        f"Entry {i} extends beyond file end (offset: {offset}, size: {size}, file: {file_size})"
                    ))
        
        if img_debugger:
            img_debugger.debug("File corruption check completed")
        
    except Exception as e:
        results.append(ValidationResult(
            ValidationLevel.CRITICAL,
            f"Corruption check failed: {str(e)}"
        ))
    
    return results


def validate_rw_data(entry) -> List[ValidationResult]: #vers 1
    """Validate RenderWare data structure"""
    results = []
    
    try:
        # Check if entry has RW data
        if not hasattr(entry, 'rw_version'):
            results.append(ValidationResult(
                ValidationLevel.INFO,
                f"{entry.name} has no RW version info"
            ))
            return results
        
        rw_version = getattr(entry, 'rw_version', 0)
        if rw_version == 0:
            results.append(ValidationResult(
                ValidationLevel.WARNING,
                f"{entry.name} has invalid RW version (0)"
            ))
            return results
        
        # Check RW version string
        if hasattr(entry, 'rw_version_string'):
            rw_version_string = getattr(entry, 'rw_version_string', '')
            results.append(ValidationResult(
                ValidationLevel.INFO,
                f"{entry.name} RW version: {rw_version_string}"
            ))
        
        # Check section type
        if hasattr(entry, 'rw_section_type'):
            section_type = getattr(entry, 'rw_section_type', 0)
            results.append(ValidationResult(
                ValidationLevel.INFO,
                f"{entry.name} RW section type: 0x{section_type:X}"
            ))
        
        # Check section size
        if hasattr(entry, 'rw_section_size'):
            section_size = getattr(entry, 'rw_section_size', 0)
            entry_size = getattr(entry, 'size', 0)
            
            if section_size > entry_size:
                results.append(ValidationResult(
                    ValidationLevel.WARNING,
                    f"{entry.name} RW section size ({section_size}) larger than entry size ({entry_size})"
                ))
        
    except Exception as e:
        results.append(ValidationResult(
            ValidationLevel.ERROR,
            f"RW data validation failed for {getattr(entry, 'name', 'unknown')}: {str(e)}"
        ))
    
    return results


def repair_img_structure(img_file) -> List[ValidationResult]: #vers 1
    """Attempt to repair IMG structure issues"""
    results = []
    
    try:
        # Import debug system
        try:
            from apps.debug.debug_functions import img_debugger
        except ImportError:
            img_debugger = None
        
        # Initialize missing attributes
        if not hasattr(img_file, 'entries'):
            img_file.entries = []
            results.append(ValidationResult(
                ValidationLevel.INFO,
                "Initialized missing entries list"
            ))
        
        if not hasattr(img_file, 'is_open'):
            img_file.is_open = False
            results.append(ValidationResult(
                ValidationLevel.INFO,
                "Initialized missing is_open attribute"
            ))
        
        if not hasattr(img_file, 'modified'):
            img_file.modified = False
            results.append(ValidationResult(
                ValidationLevel.INFO,
                "Initialized missing modified attribute"
            ))
        
        # Fix entry issues
        entries = getattr(img_file, 'entries', [])
        for i, entry in enumerate(entries):
            # Fix missing name
            if not hasattr(entry, 'name') or not getattr(entry, 'name', ''):
                entry.name = f"unknown_{i}"
                results.append(ValidationResult(
                    ValidationLevel.INFO,
                    f"Fixed missing name for entry {i}"
                ))
            
            # Fix missing size
            if not hasattr(entry, 'size'):
                entry.size = 0
                results.append(ValidationResult(
                    ValidationLevel.INFO,
                    f"Fixed missing size for entry {i}"
                ))
            
            # Fix missing offset
            if not hasattr(entry, 'offset'):
                entry.offset = 0
                results.append(ValidationResult(
                    ValidationLevel.INFO,
                    f"Fixed missing offset for entry {i}"
                ))
        
        if img_debugger:
            img_debugger.success(f"IMG structure repair completed: {len(results)} fixes applied")
        
    except Exception as e:
        results.append(ValidationResult(
            ValidationLevel.ERROR,
            f"Structure repair failed: {str(e)}"
        ))
    
    return results


def get_validation_report(img_file) -> Dict[str, Any]: #vers 1
    """Get comprehensive validation report"""
    try:
        # Run all validations
        structure_results = validate_img_file_structure(img_file)
        entry_results = validate_img_entries(img_file)
        corruption_results = check_file_corruption(img_file)
        
        # Combine results
        all_results = structure_results + entry_results + corruption_results
        
        # Count by severity
        counts = {
            'info': 0,
            'warning': 0,
            'error': 0,
            'critical': 0
        }
        
        for result in all_results:
            counts[result.level.value] += 1
        
        # Determine overall status
        if counts['critical'] > 0:
            status = 'critical'
        elif counts['error'] > 0:
            status = 'error'
        elif counts['warning'] > 0:
            status = 'warning'
        else:
            status = 'valid'
        
        return {
            'status': status,
            'total_issues': len(all_results),
            'counts': counts,
            'results': [result.to_dict() for result in all_results],
            'file_path': getattr(img_file, 'file_path', 'Unknown'),
            'entry_count': len(getattr(img_file, 'entries', []))
        }
        
    except Exception as e:
        return {
            'status': 'error',
            'error': f"Validation report generation failed: {str(e)}",
            'total_issues': 0,
            'counts': {},
            'results': [],
            'file_path': getattr(img_file, 'file_path', 'Unknown'),
            'entry_count': 0
        }


def integrate_validation_functions(main_window) -> bool: #vers 1
    """Integrate validation functions into main window"""
    try:
        # Add validation methods
        main_window.validate_img_file_structure = validate_img_file_structure
        main_window.validate_img_entries = validate_img_entries
        main_window.validate_entry_integrity = validate_entry_integrity
        main_window.check_file_corruption = check_file_corruption
        main_window.validate_rw_data = validate_rw_data
        main_window.repair_img_structure = repair_img_structure
        main_window.get_validation_report = get_validation_report
        
        # Add classes
        main_window.ValidationResult = ValidationResult
        main_window.ValidationLevel = ValidationLevel
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("✅ IMG validation functions integrated")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Validation integration failed: {e}")
        return False

    def detect_img_version(self, file_path: str) -> IMGVersion:
        with open(file_path, 'rb') as f:
            header = f.read(8)

            if header.startswith(b'VER2'):
                # It's a VERSION_2 IMG (VC or SOL)
                f.seek(0)
                # Read first few directory entries to check for SA-like sizes
                dir_start = 8
                f.seek(dir_start)
                entry_data = f.read(32)  # First entry

                if len(entry_data) == 32:
                    # Parse offset/size (VC uses 32-byte entries)
                    offset = struct.unpack('<I', entry_data[16:20])[0]
                    size = struct.unpack('<I', entry_data[20:24])[0]

                    # SOL IMGs have VC structure but SA-sized files (often >10MB)
                    if size > 10 * 1024 * 1024:  # >10MB suggests SOL
                        return IMGVersion.VERSION_SOL

                return IMGVersion.VERSION_2

            elif header.startswith(b'\x00\x00\x00\x01'):
                return IMGVersion.VERSION_1
            elif header.startswith(b'VER3'):
                return IMGVersion.VERSION_3
            else:
                # Fallback: check file size heuristics
                file_size = os.path.getsize(file_path)
                if file_size > 500 * 1024 * 1024:  # >500MB = likely SA/SOL
                    return IMGVersion.VERSION_3
                return IMGVersion.VERSION_2

# Utility functions for integration
def quick_validate_img(file_path: str) -> ValidationResult:
    """Quick validation of IMG file without full loading"""
    result = ValidationResult()

    if not os.path.exists(file_path):
        result.add_error(f"File does not exist: {file_path}")
        return result

    try:
        # Try to detect version
        version = detect_img_version(file_path)
        if version == IMGVersion.UNKNOWN:
            result.add_error("Unknown or invalid IMG file format")
        else:
            result.add_info(f"Detected IMG version: {version.name}")

    except Exception as e:
        result.add_error(f"Failed to analyze file: {str(e)}")

    return result


def validate_before_import(file_paths: List[str]) -> Dict[str, ValidationResult]:
    """Validate multiple files before import"""
    results = {}

    for file_path in file_paths:
        results[file_path] = IMGValidator.validate_file_for_import(file_path)

    return results


def batch_validate_directory(directory_path: str, extensions: List[str] = None) -> Dict[str, ValidationResult]:
    """Validate all files in a directory for IMG import"""
    results = {}

    if not os.path.exists(directory_path):
        return results

    if extensions is None:
        extensions = ['dff', 'txd', 'col', 'ifp', 'scm', 'ipl', 'ide', 'dat']

    for root, dirs, files in os.walk(directory_path):
        for file in files:
            file_path = os.path.join(root, file)
            ext = Path(file_path).suffix.lower().lstrip('.')

            if ext in [e.lower() for e in extensions]:
                results[file_path] = IMGValidator.validate_file_for_import(file_path)

    return results


# Export functions and classes
__all__ = [
    'ValidationLevel',
    'ValidationResult',
    'validate_img_file_structure',
    'validate_img_entries',
    'validate_entry_integrity',
    'check_file_corruption',
    'validate_rw_data',
    'repair_img_structure',
    'get_validation_report',
    'integrate_validation_functions'
]

if __name__ == "__main__":
    # Test validation system
    print("IMG Validator Test")

    # Test file validation
    test_files = ["test.dff", "test.txd", "nonexistent.img"]

    for test_file in test_files:
        print(f"\nValidating: {test_file}")
        result = IMGValidator.validate_file_for_import(test_file)
        print(f"Result: {result.get_summary()}")
        if result.warnings or result.errors:
            print(result.get_details())

    print("\nValidation tests completed!")
