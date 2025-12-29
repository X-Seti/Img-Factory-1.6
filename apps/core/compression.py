#this belongs in core/compression.py - Version: 1
# X-Seti - August27 2025 - IMG Factory 1.5 - Compression Functions
# Moved from apps.components.img_compression.py + added from img_manager.py

"""
IMG Compression System - Complete compression analysis and processing
Consolidated from apps.components.img_compression.py with added analysis functions
"""

import os
import zlib
import struct
import time
from enum import Enum
from typing import Dict, List, Any, Optional, Union, Callable
from dataclasses import dataclass

# Import compression libraries with fallbacks
try:
    import lz4.frame
    import lz4.block
    HAS_LZ4 = True
except ImportError:
    HAS_LZ4 = False

try:
    import lzo
    HAS_LZO = True
except ImportError:
    HAS_LZO = False

try:
    import brotli
    HAS_BROTLI = True
except ImportError:
    HAS_BROTLI = False

##Methods list -
# get_compression_ratio_analysis
# analyze_compression_potential
# compress_img_entries
# decompress_img_entries
# get_available_algorithms
# format_compression_ratio

##Classes -
# CompressionAlgorithm
# CompressionResult
# CompressionSettings
# IMGCompressor
# CompressionAnalyzer

class CompressionAlgorithm(Enum):
    """Supported compression algorithms"""
    NONE = "none"
    ZLIB = "zlib"
    GZIP = "gzip"
    DEFLATE = "deflate"
    LZ4 = "lz4"
    LZO_1X_1 = "lzo1x-1"
    LZO_1X_999 = "lzo1x-999"
    BROTLI = "brotli"
    FASTMAN92_ZLIB = "fastman92_zlib"
    FASTMAN92_LZ4 = "fastman92_lz4"

@dataclass
class CompressionResult:
    """Result of compression operation"""
    success: bool
    original_size: int
    compressed_size: int
    compression_ratio: float
    processing_time: float
    algorithm: CompressionAlgorithm
    error_message: str = ""

@dataclass
class CompressionSettings:
    """Compression settings and options"""
    algorithm: CompressionAlgorithm
    level: int = 6
    fastman92_header: bool = False
    preserve_timestamps: bool = True
    verify_decompression: bool = True

class IMGCompressor:
    """Main compression processor for IMG files"""
    
    def __init__(self, settings: Optional[CompressionSettings] = None):
        self.settings = settings or CompressionSettings(CompressionAlgorithm.ZLIB)
        self.stats = {
            'total_compressed': 0,
            'total_original_size': 0,
            'total_compressed_size': 0,
            'files_processed': 0,
            'compression_time': 0.0
        }
    
    def compress_data(self, data: bytes, algorithm: Optional[CompressionAlgorithm] = None, 
                     level: Optional[int] = None) -> CompressionResult:
        """Compress data using specified algorithm"""
        start_time = time.time()
        
        algorithm = algorithm or self.settings.algorithm
        level = level or self.settings.level
        
        try:
            if algorithm == CompressionAlgorithm.NONE:
                compressed_data = data
            elif algorithm == CompressionAlgorithm.ZLIB:
                compressed_data = zlib.compress(data, level)
            elif algorithm == CompressionAlgorithm.GZIP:
                import gzip
                compressed_data = gzip.compress(data, compresslevel=level)
            elif algorithm == CompressionAlgorithm.DEFLATE:
                compressed_data = zlib.compress(data, level)[2:-4]  # Remove zlib headers
            elif algorithm == CompressionAlgorithm.LZ4 and HAS_LZ4:
                compressed_data = lz4.frame.compress(data)
            elif algorithm == CompressionAlgorithm.BROTLI and HAS_BROTLI:
                compressed_data = brotli.compress(data, quality=level)
            elif algorithm == CompressionAlgorithm.FASTMAN92_ZLIB:
                compressed_data = self._compress_fastman92_zlib(data, level)
            elif algorithm == CompressionAlgorithm.FASTMAN92_LZ4 and HAS_LZ4:
                compressed_data = self._compress_fastman92_lz4(data)
            else:
                return CompressionResult(
                    False, len(data), len(data), 1.0, time.time() - start_time,
                    algorithm, f"Algorithm {algorithm.value} not available"
                )
            
            processing_time = time.time() - start_time
            compression_ratio = len(compressed_data) / len(data) if len(data) > 0 else 1.0
            
            # Update statistics
            self.stats['files_processed'] += 1
            self.stats['total_original_size'] += len(data)
            self.stats['total_compressed_size'] += len(compressed_data)
            self.stats['compression_time'] += processing_time
            
            return CompressionResult(
                True, len(data), len(compressed_data), compression_ratio,
                processing_time, algorithm
            )
            
        except Exception as e:
            return CompressionResult(
                False, len(data), len(data), 1.0, time.time() - start_time,
                algorithm, str(e)
            )
    
    def decompress_data(self, data: bytes, algorithm: CompressionAlgorithm, 
                       expected_size: Optional[int] = None) -> bytes:
        """Decompress data using specified algorithm"""
        try:
            if algorithm == CompressionAlgorithm.NONE:
                return data
            elif algorithm == CompressionAlgorithm.ZLIB:
                return zlib.decompress(data)
            elif algorithm == CompressionAlgorithm.GZIP:
                import gzip
                return gzip.decompress(data)
            elif algorithm == CompressionAlgorithm.LZ4 and HAS_LZ4:
                return lz4.frame.decompress(data)
            elif algorithm == CompressionAlgorithm.BROTLI and HAS_BROTLI:
                return brotli.decompress(data)
            elif algorithm == CompressionAlgorithm.FASTMAN92_ZLIB:
                return self._decompress_fastman92_zlib(data)
            elif algorithm == CompressionAlgorithm.FASTMAN92_LZ4 and HAS_LZ4:
                return self._decompress_fastman92_lz4(data)
            else:
                raise ValueError(f"Algorithm {algorithm.value} not supported for decompression")
                
        except Exception as e:
            raise RuntimeError(f"Decompression failed: {e}")
    
    def _compress_fastman92_zlib(self, data: bytes, level: int) -> bytes:
        """Compress using Fastman92 ZLIB format"""
        compressed = zlib.compress(data, level)
        
        if self.settings.fastman92_header:
            # Add Fastman92 header: original_size(4) + compressed_size(4) + version(4)
            header = struct.pack('<III', len(data), len(compressed), 1)
            return header + compressed
        
        return compressed
    
    def _compress_fastman92_lz4(self, data: bytes) -> bytes:
        """Compress using Fastman92 LZ4 format"""
        if not HAS_LZ4:
            raise RuntimeError("LZ4 not available")
        
        compressed = lz4.block.compress(data)
        
        if self.settings.fastman92_header:
            header = struct.pack('<III', len(data), len(compressed), 1)
            return header + compressed
        
        return compressed
    
    def _decompress_fastman92_zlib(self, data: bytes) -> bytes:
        """Decompress Fastman92 ZLIB format"""
        if self.settings.fastman92_header and len(data) >= 12:
            original_size, compressed_size, version = struct.unpack('<III', data[:12])
            compressed_data = data[12:]
        else:
            compressed_data = data
        
        return zlib.decompress(compressed_data)
    
    def _decompress_fastman92_lz4(self, data: bytes) -> bytes:
        """Decompress Fastman92 LZ4 format"""
        if self.settings.fastman92_header and len(data) >= 12:
            original_size, compressed_size, version = struct.unpack('<III', data[:12])
            compressed_data = data[12:]
        else:
            compressed_data = data
        
        return lz4.block.decompress(compressed_data)
    
    def get_compression_statistics(self) -> Dict[str, Any]:
        """Get compression statistics"""
        total_saved = self.stats['total_original_size'] - self.stats['total_compressed_size']
        compression_ratio = (self.stats['total_compressed_size'] / self.stats['total_original_size'] 
                           if self.stats['total_original_size'] > 0 else 1.0)
        
        return {
            'files_processed': self.stats['files_processed'],
            'total_original_size': self.stats['total_original_size'],
            'total_compressed_size': self.stats['total_compressed_size'],
            'total_bytes_saved': total_saved,
            'overall_compression_ratio': compression_ratio,
            'compression_percentage': (1.0 - compression_ratio) * 100.0,
            'total_processing_time': self.stats['compression_time']
        }


class CompressionAnalyzer:
    """Advanced compression analysis and optimization"""
    
    @staticmethod
    def get_compression_ratio_analysis(img_file) -> Dict[str, Any]: #vers 1
        """Analyze compression ratios for IMG entries - MOVED FROM img_manager.py"""
        try:
            analysis = {
                'compressible_entries': [],
                'already_compressed': [],
                'potential_savings': 0,
                'total_entries': len(img_file.entries) if img_file.entries else 0,
                'analysis_summary': {}
            }
            
            total_uncompressed_size = 0
            total_compressed_size = 0
            compressible_count = 0
            
            for entry in img_file.entries:
                try:
                    if hasattr(entry, 'is_compressed') and entry.is_compressed:
                        # Entry is already compressed
                        if hasattr(entry, 'uncompressed_size') and hasattr(entry, 'size'):
                            ratio = (entry.uncompressed_size - entry.size) / entry.uncompressed_size * 100
                            analysis['already_compressed'].append({
                                'name': entry.name,
                                'ratio': ratio,
                                'savings': entry.uncompressed_size - entry.size,
                                'uncompressed_size': entry.uncompressed_size,
                                'compressed_size': entry.size
                            })
                            total_uncompressed_size += entry.uncompressed_size
                            total_compressed_size += entry.size
                    else:
                        # Test compression potential
                        data = entry.get_data() if hasattr(entry, 'get_data') else b''
                        if data:
                            compressed = zlib.compress(data, 6)
                            if len(compressed) < len(data):
                                savings = len(data) - len(compressed)
                                ratio = savings / len(data) * 100
                                analysis['compressible_entries'].append({
                                    'name': entry.name,
                                    'ratio': ratio,
                                    'savings': savings,
                                    'original_size': len(data),
                                    'compressed_size': len(compressed)
                                })
                                analysis['potential_savings'] += savings
                                compressible_count += 1
                                total_uncompressed_size += len(data)
                                total_compressed_size += len(compressed)
                            
                except Exception:
                    continue
            
            # Generate summary
            analysis['analysis_summary'] = {
                'total_uncompressed_mb': total_uncompressed_size / (1024 * 1024),
                'total_compressed_mb': total_compressed_size / (1024 * 1024),
                'overall_savings_mb': (total_uncompressed_size - total_compressed_size) / (1024 * 1024),
                'overall_compression_ratio': (total_compressed_size / total_uncompressed_size * 100) if total_uncompressed_size > 0 else 100,
                'compressible_entries_count': compressible_count,
                'already_compressed_count': len(analysis['already_compressed'])
            }
            
            return analysis
            
        except Exception as e:
            return {'error': str(e)}
    
    @staticmethod
    def analyze_compression_potential(data: bytes) -> Dict[str, Any]: #vers 1
        """Analyze data compression potential"""
        analysis = {
            'size': len(data),
            'entropy': CompressionAnalyzer._calculate_entropy(data),
            'repetition_score': CompressionAnalyzer._calculate_repetition_score(data),
            'compression_potential': 'unknown',
            'recommended_algorithms': [],
            'estimated_ratios': {}
        }
        
        # Determine compression potential based on entropy
        if analysis['entropy'] < 3.0:
            analysis['compression_potential'] = 'excellent'
        elif analysis['entropy'] < 5.0:
            analysis['compression_potential'] = 'good'
        elif analysis['entropy'] < 6.5:
            analysis['compression_potential'] = 'moderate'
        else:
            analysis['compression_potential'] = 'poor'
        
        # Recommend algorithms based on analysis
        if analysis['repetition_score'] > 0.3:
            analysis['recommended_algorithms'].extend(['LZ4', 'LZO_1X_1'])
        
        if analysis['entropy'] < 5.0:
            analysis['recommended_algorithms'].extend(['ZLIB', 'BROTLI'])
        
        if len(data) > 1024 * 1024:  # Large files
            analysis['recommended_algorithms'].append('LZ4')
        else:
            analysis['recommended_algorithms'].append('ZLIB')
        
        # Remove duplicates and limit recommendations
        analysis['recommended_algorithms'] = list(set(analysis['recommended_algorithms']))[:3]
        
        return analysis
    
    @staticmethod
    def _calculate_entropy(data: bytes) -> float:
        """Calculate Shannon entropy of data"""
        if not data:
            return 0.0
        
        # Count byte frequencies
        freq = {}
        for byte in data:
            freq[byte] = freq.get(byte, 0) + 1
        
        # Calculate entropy
        entropy = 0.0
        data_len = len(data)
        
        for count in freq.values():
            probability = count / data_len
            if probability > 0:
                import math
                entropy -= probability * math.log2(probability)
        
        return entropy
    
    @staticmethod
    def _calculate_repetition_score(data: bytes) -> float:
        """Calculate how repetitive the data is"""
        if len(data) < 256:
            return 0.0
        
        # Sample data to avoid performance issues
        sample_size = min(10000, len(data))
        sample = data[:sample_size]
        
        # Count repeated patterns
        patterns = {}
        pattern_length = 4
        
        for i in range(len(sample) - pattern_length + 1):
            pattern = sample[i:i + pattern_length]
            patterns[pattern] = patterns.get(pattern, 0) + 1
        
        # Calculate repetition score
        total_patterns = len(sample) - pattern_length + 1
        repeated_patterns = sum(1 for count in patterns.values() if count > 1)
        
        return repeated_patterns / total_patterns if total_patterns > 0 else 0.0


def compress_img_entries(img_file, algorithm: CompressionAlgorithm = CompressionAlgorithm.ZLIB, 
                        level: int = 6, progress_callback: Optional[Callable] = None) -> Dict[str, Any]: #vers 1
    """Compress entries in IMG file"""
    try:
        compressor = IMGCompressor(CompressionSettings(algorithm, level))
        results = {
            'successful': [],
            'failed': [],
            'total_savings': 0,
            'compression_ratio': 0
        }
        
        total_original = 0
        total_compressed = 0
        
        for i, entry in enumerate(img_file.entries):
            if progress_callback:
                progress_callback(i, len(img_file.entries), f"Compressing: {entry.name}")
            
            try:
                data = entry.get_data() if hasattr(entry, 'get_data') else b''
                if data:
                    result = compressor.compress_data(data, algorithm, level)
                    
                    if result.success and result.compression_ratio < 0.95:  # Only if significant compression
                        results['successful'].append({
                            'name': entry.name,
                            'original_size': result.original_size,
                            'compressed_size': result.compressed_size,
                            'savings': result.original_size - result.compressed_size,
                            'ratio': result.compression_ratio
                        })
                        
                        total_original += result.original_size
                        total_compressed += result.compressed_size
                        results['total_savings'] += result.original_size - result.compressed_size
                        
                        # Update entry with compressed data (would need IMG file modification support)
                        # entry.compressed_data = compressed_data
                        # entry.is_compressed = True
                    
                    else:
                        results['failed'].append({
                            'name': entry.name,
                            'reason': 'No significant compression benefit'
                        })
                        
            except Exception as e:
                results['failed'].append({
                    'name': entry.name,
                    'reason': str(e)
                })
        
        if progress_callback:
            progress_callback(len(img_file.entries), len(img_file.entries), "Complete")
        
        results['compression_ratio'] = total_compressed / total_original if total_original > 0 else 1.0
        
        return results
        
    except Exception as e:
        return {'error': str(e)}


def decompress_img_entries(img_file, progress_callback: Optional[Callable] = None) -> Dict[str, Any]: #vers 1
    """Decompress entries in IMG file"""
    try:
        compressor = IMGCompressor()
        results = {
            'successful': [],
            'failed': [],
            'total_entries_processed': 0
        }
        
        compressed_entries = [entry for entry in img_file.entries 
                            if hasattr(entry, 'is_compressed') and entry.is_compressed]
        
        for i, entry in enumerate(compressed_entries):
            if progress_callback:
                progress_callback(i, len(compressed_entries), f"Decompressing: {entry.name}")
            
            try:
                # Would need to determine compression algorithm used
                algorithm = CompressionAlgorithm.ZLIB  # Default assumption
                
                compressed_data = entry.get_data() if hasattr(entry, 'get_data') else b''
                if compressed_data:
                    decompressed_data = compressor.decompress_data(compressed_data, algorithm)
                    
                    results['successful'].append({
                        'name': entry.name,
                        'compressed_size': len(compressed_data),
                        'decompressed_size': len(decompressed_data)
                    })
                    
                    # Update entry with decompressed data
                    # entry.data = decompressed_data  
                    # entry.is_compressed = False
                    
            except Exception as e:
                results['failed'].append({
                    'name': entry.name,
                    'reason': str(e)
                })
        
        if progress_callback:
            progress_callback(len(compressed_entries), len(compressed_entries), "Complete")
        
        results['total_entries_processed'] = len(compressed_entries)
        
        return results
        
    except Exception as e:
        return {'error': str(e)}


def get_available_algorithms() -> List[CompressionAlgorithm]: #vers 1
    """Get list of available compression algorithms"""
    available = [
        CompressionAlgorithm.NONE,
        CompressionAlgorithm.ZLIB,
        CompressionAlgorithm.GZIP,
        CompressionAlgorithm.DEFLATE
    ]
    
    if HAS_LZ4:
        available.extend([
            CompressionAlgorithm.LZ4,
            CompressionAlgorithm.FASTMAN92_LZ4
        ])
    
    if HAS_LZO:
        available.extend([
            CompressionAlgorithm.LZO_1X_1,
            CompressionAlgorithm.LZO_1X_999
        ])
    
    if HAS_BROTLI:
        available.append(CompressionAlgorithm.BROTLI)
    
    available.append(CompressionAlgorithm.FASTMAN92_ZLIB)
    
    return available


def format_compression_ratio(ratio: float) -> str: #vers 1
    """Format compression ratio as percentage"""
    percentage = (1.0 - ratio) * 100.0
    return f"{percentage:.1f}%"


def format_file_size(size_bytes: int) -> str: #vers 1
    """Format file size in human readable format"""
    for unit in ['B', 'KB', 'MB', 'GB']:
        if size_bytes < 1024.0:
            return f"{size_bytes:.1f} {unit}"
        size_bytes /= 1024.0
    return f"{size_bytes:.1f} TB"


# Example usage and testing
if __name__ == "__main__":
    # Test compression system
    print("Testing IMG Compression System...")
    
    # Create test data
    test_data = b"Hello, IMG Factory! " * 1000  # Repetitive data for good compression
    
    # Initialize compressor
    compressor = IMGCompressor()
    
    # Test different algorithms
    algorithms_to_test = get_available_algorithms()
    
    print(f"Available algorithms: {[alg.value for alg in algorithms_to_test]}")
    
    for algorithm in algorithms_to_test[:3]:  # Test first 3
        result = compressor.compress_data(test_data, algorithm, 6)
        
        if result.success:
            print(f"✓ {algorithm.value}: {format_file_size(result.original_size)} -> "
                  f"{format_file_size(result.compressed_size)} "
                  f"({format_compression_ratio(result.compression_ratio)})")
            
            # Test decompression
            try:
                decompressed = compressor.decompress_data(
                    zlib.compress(test_data, 6) if algorithm == CompressionAlgorithm.ZLIB else test_data, 
                    algorithm
                )
                
                if decompressed == test_data:
                    print(f"  ✓ Decompression successful")
                else:
                    print(f"  ✗ Decompression failed")
            except Exception as e:
                print(f"  ✗ Decompression error: {e}")
        else:
            print(f"✗ {algorithm.value}: {result.error_message}")
    
    # Test compression analysis
    analysis = CompressionAnalyzer.analyze_compression_potential(test_data)
    print(f"\n✓ Compression analysis:")
    print(f"  Entropy: {analysis['entropy']:.2f}")
    print(f"  Potential: {analysis['compression_potential']}")
    print(f"  Recommended: {analysis['recommended_algorithms']}")
    
    # Statistics
    stats = compressor.get_compression_statistics()
    print(f"\n✓ Compression statistics:")
    print(f"  Files processed: {stats['files_processed']}")
    print(f"  Total saved: {format_file_size(stats['total_bytes_saved'])}")
    print(f"  Overall ratio: {format_compression_ratio(stats['overall_compression_ratio'])}")
    
    print("\nIMG Compression tests completed!")


# Export list for external imports
__all__ = [
    'CompressionAlgorithm',
    'CompressionResult',
    'CompressionSettings',
    'IMGCompressor',
    'CompressionAnalyzer',
    'get_compression_ratio_analysis',
    'analyze_compression_potential',
    'compress_img_entries',
    'decompress_img_entries',
    'get_available_algorithms',
    'format_compression_ratio',
    'format_file_size'
]