#!/usr/bin/env python3
#this belongs in methods/ txd_serializer.py - Version: 5
# X-Seti - October11 2025 - Img Factory 1.5 - TXD Serializer

"""
RenderWare TXD Binary Serializer
Writes texture dictionary files in RenderWare binary format
Supports: DXT1/DXT3/DXT5, ARGB8888, RGB888, mipmaps, bumpmaps, reflection maps
REVERTED: Names go INSIDE struct (88-byte header format), not separate STRING sections
"""

import struct
from typing import List, Dict, Optional

##Methods list -
# __init__
# _build_texture_dictionary
# _build_texture_dictionary_from_sections
# _build_texture_native
# _calculate_texture_size
# _compress_to_dxt
# _get_d3d_format
# _get_format_code
# _write_section_header
# serialize_txd
# serialize_txd_file

class TXDSerializer: #vers 1
    """Serialize texture data to RenderWare TXD binary format"""
    
    # RenderWare section types
    SECTION_STRUCT = 0x01
    SECTION_STRING = 0x02
    SECTION_EXTENSION = 0x03
    SECTION_TEXTURE_DICTIONARY = 0x16
    SECTION_TEXTURE_NATIVE = 0x15
    
    # Platform identifiers
    PLATFORM_D3D8 = 0x08  # PC/DirectX 8
    PLATFORM_D3D9 = 0x09  # PC/DirectX 9
    
    # RenderWare version
    RW_VERSION = 0x1803FFFF  # 3.6.0.3
    
    def __init__(self): #vers 1
        self.output = bytearray()
    
    def serialize_txd(self, textures: List[Dict], target_version: int = None, 
                     target_device: int = None) -> bytes: #vers 1
        """Serialize texture list to TXD binary data"""
        if not textures:
            return b''
        
        txd_data = self._build_texture_dictionary(textures)
        return bytes(txd_data)
    

    def _build_texture_native(self, texture: Dict) -> bytearray: #vers 6
        """
        Build texture native section - FIXED: Alpha preservation

        Args:
            texture: Texture dictionary with all properties and data

        Returns:
            bytearray: Complete texture native section ready to write
        """
        result = bytearray()

        # Extract texture properties
        width = texture.get('width', 256)
        height = texture.get('height', 256)
        depth = texture.get('depth', 32)
        format_str = texture.get('format', 'DXT1')
        has_alpha = texture.get('has_alpha', False)
        rgba_data = texture.get('rgba_data', b'')
        mipmap_levels = texture.get('mipmap_levels', [])
        name = texture.get('name', 'texture')
        alpha_name = texture.get('alpha_name', name + 'a') if has_alpha else ''

        # Extract bumpmap and reflection data
        bumpmap_data = texture.get('bumpmap_data', b'')
        has_bumpmap = texture.get('has_bumpmap', False) or bool(bumpmap_data)
        reflection_map = texture.get('reflection_map', b'')
        fresnel_map = texture.get('fresnel_map', b'')
        has_reflection = texture.get('has_reflection', False) or bool(reflection_map)

        # Get format code
        format_code = self._get_format_code(format_str, has_alpha)

        # Calculate mipmap count
        num_mipmaps = max(1, len(mipmap_levels))

        # Build struct data - 88 byte header
        struct_data = bytearray()

        # Platform ID - 4 bytes
        struct_data.extend(struct.pack('<I', self.PLATFORM_D3D8))

        # Filter flags - 4 bytes
        filter_flags = texture.get('filter_flags', 0x1102)
        struct_data.extend(struct.pack('<I', filter_flags))

        # Texture name - 32 bytes null-terminated
        name_bytes = name.encode('ascii')[:31] + b'\x00'
        name_bytes = name_bytes.ljust(32, b'\x00')
        struct_data.extend(name_bytes)

        # Alpha name - 32 bytes null-terminated
        if has_alpha and alpha_name:
            alpha_bytes = alpha_name.encode('ascii')[:31] + b'\x00'
            alpha_bytes = alpha_bytes.ljust(32, b'\x00')
        else:
            alpha_bytes = b'\x00' * 32
        struct_data.extend(alpha_bytes)

        # Raster format - 4 bytes
        raster_format = format_code
        if num_mipmaps > 1:
            raster_format |= 0x0400

        raster_format_flags = texture.get('raster_format_flags', 0)
        if has_bumpmap:
            raster_format_flags |= 0x10

        raster_format |= (raster_format_flags & 0xFF0)
        struct_data.extend(struct.pack('<I', raster_format))

        # D3D format - 4 bytes
        d3d_format = self._get_d3d_format(format_str)
        struct_data.extend(struct.pack('<I', d3d_format))

        # Width and Height - 2 bytes each
        struct_data.extend(struct.pack('<HH', width, height))

        # Depth - 1 byte
        struct_data.extend(struct.pack('<B', depth))

        # Mipmap count - 1 byte
        struct_data.extend(struct.pack('<B', num_mipmaps))

        # Raster type - 1 byte
        raster_type = 0x04
        struct_data.extend(struct.pack('<B', raster_type))

        # Compression flags - 1 byte
        compression = 0x08 if 'DXT' in format_str else 0x00
        struct_data.extend(struct.pack('<B', compression))

        # Calculate total data size
        total_data_size = 0

        if mipmap_levels:
            total_data_size = sum(level.get('compressed_size', 0) for level in mipmap_levels)
        else:
            total_data_size = self._calculate_texture_size(width, height, format_str, num_mipmaps)

        if bumpmap_data:
            total_data_size += 4 + 1 + len(bumpmap_data)

        if reflection_map:
            total_data_size += 4 + len(reflection_map)
        if fresnel_map:
            total_data_size += 4 + len(fresnel_map)

        struct_data.extend(struct.pack('<I', total_data_size))

        # Build texture data section
        texture_data = bytearray()

        # CRITICAL FIX - Use preserved original data FIRST
        if mipmap_levels:
            for level in sorted(mipmap_levels, key=lambda x: x.get('level', 0)):
                # Priority: compressed_data > original_bgra_data > rgba_data
                level_data = (level.get('compressed_data') or
                            level.get('original_bgra_data') or
                            level.get('rgba_data', b''))

                if level_data:
                    texture_data.extend(level_data)
        else:
            if 'DXT' in format_str:
                # DXT compressed textures - USE ORIGINAL COMPRESSED DATA
                compressed = texture.get('compressed_data', b'')
                if compressed:
                    # ✅ Use original - preserves alpha perfectly
                    texture_data.extend(compressed)
                else:
                    # Only re-compress if no original exists
                    compressed = self._compress_to_dxt(rgba_data, width, height, format_str)
                    if compressed:
                        texture_data.extend(compressed)
            else:
                # Uncompressed textures - USE ORIGINAL BGRA DATA
                original_bgra = texture.get('original_bgra_data', b'')

                if original_bgra:
                    # ✅ Use original - preserves alpha perfectly
                    texture_data.extend(original_bgra)
                else:
                    # Convert RGBA back to BGRA if no original
                    bgra_data = self._rgba_to_bgra(rgba_data)
                    texture_data.extend(bgra_data)

        # Add bumpmap if present
        if has_bumpmap and bumpmap_data:
            struct_data.extend(struct.pack('<I', len(bumpmap_data)))
            struct_data.extend(struct.pack('<B', 0x01))
            texture_data.extend(bumpmap_data)

        # Add reflection map if present
        if has_reflection and reflection_map:
            texture_data.extend(struct.pack('<I', len(reflection_map)))
            texture_data.extend(reflection_map)

        if fresnel_map:
            texture_data.extend(struct.pack('<I', len(fresnel_map)))
            texture_data.extend(fresnel_map)

        # Combine struct + texture data
        combined_data = bytes(struct_data) + bytes(texture_data)

        # Build texture native header
        result.extend(self._write_section_header(
            self.SECTION_TEXTURE_NATIVE,
            len(combined_data) + 12,  # +12 for extension
            self.RW_VERSION
        ))

        # Write struct section
        result.extend(self._write_section_header(
            self.SECTION_STRUCT,
            len(combined_data),
            self.RW_VERSION
        ))
        result.extend(combined_data)

        # Write extension section
        result.extend(self._write_section_header(
            self.SECTION_EXTENSION,
            0,
            self.RW_VERSION
        ))

        return result


    def _rgba_to_bgra(self, rgba_data: bytes) -> bytes: #vers 1
        """Convert RGBA to BGRA for RenderWare - preserves alpha channel"""
        bgra_data = bytearray()

        for i in range(0, len(rgba_data), 4):
            r = rgba_data[i]
            g = rgba_data[i + 1]
            b = rgba_data[i + 2]
            a = rgba_data[i + 3]  # ✅ Keep alpha intact

            # Swap R and B, keep G and A
            bgra_data.extend([b, g, r, a])

        return bytes(bgra_data)


    def _build_texture_dictionary(self, textures: List[Dict]) -> bytearray: #vers 1
        """Build complete texture dictionary"""
        texture_sections = []
        for texture in textures:
            tex_data = self._build_texture_native(texture)
            texture_sections.append(tex_data)
        
        struct_size = 4
        struct_data = struct.pack('<I', len(textures))
        
        result = bytearray()
        
        total_size = 12 + struct_size + 12
        for tex_section in texture_sections:
            total_size += len(tex_section)
        
        result.extend(self._write_section_header(
            self.SECTION_TEXTURE_DICTIONARY,
            total_size - 12,
            self.RW_VERSION
        ))
        
        result.extend(self._write_section_header(
            self.SECTION_STRUCT,
            struct_size,
            self.RW_VERSION
        ))
        result.extend(struct_data)
        
        for tex_section in texture_sections:
            result.extend(tex_section)
        
        result.extend(self._write_section_header(
            self.SECTION_EXTENSION,
            0,
            self.RW_VERSION
        ))
        
        return result
    



    def _write_section_header(self, section_type: int, size: int, version: int) -> bytes: #vers 1
        """Write RenderWare section header"""
        return struct.pack('<III', section_type, size, version)
    
    def _get_format_code(self, format_str: str, has_alpha: bool) -> int: #vers 2
        """Get RenderWare raster_format flags for format string"""
        # Returns the raster_format value (PAL flag | pixel format bits)
        raster_map = {
            'DXT1':     0x31545844,
            'DXT2':     0x32545844,
            'DXT3':     0x33545844,
            'DXT4':     0x34545844,
            'DXT5':     0x35545844,
            'ARGB8888': 0x0500,
            'RGB888':   0x0600,
            'ARGB1555': 0x0100,
            'ARGB4444': 0x0300,
            'RGB565':   0x0200,
            'RGB555':   0x0A00,
            'LUM8':     0x0400,
            'A8L8':     0x0400,  # closest RW type
            'PAL8':     0x0500 | 0x2000,  # C8888 palette entries + FORMAT_EXT_PAL8
            'PAL4':     0x0500 | 0x4000,  # C8888 palette entries + FORMAT_EXT_PAL4
        }
        return raster_map.get(format_str, 0x31545844)

    def _get_d3d_format(self, format_str: str) -> int: #vers 2
        """Get D3D format code for format string"""
        d3d_map = {
            'DXT1':     0x31545844,
            'DXT2':     0x32545844,
            'DXT3':     0x33545844,
            'DXT4':     0x34545844,
            'DXT5':     0x35545844,
            'ARGB8888': 21,   # D3DFMT_A8R8G8B8
            'RGB888':   20,   # D3DFMT_R8G8B8
            'ARGB1555': 25,   # D3DFMT_A1R5G5B5
            'ARGB4444': 26,   # D3DFMT_A4R4G4B4
            'RGB565':   23,   # D3DFMT_R5G6B5
            'RGB555':   24,   # D3DFMT_X1R5G5B5
            'LUM8':     50,   # D3DFMT_L8
            'A8L8':     51,   # D3DFMT_A8L8
            'PAL8':     41,   # D3DFMT_P8
            'PAL4':     41,   # D3DFMT_P8 (closest)
        }
        return d3d_map.get(format_str, 0x31545844)
    
    def _calculate_texture_size(self, width: int, height: int, format_str: str, num_mipmaps: int) -> int: #vers 1
        """Calculate texture data size"""
        total = 0
        w, h = width, height
        
        for i in range(num_mipmaps):
            if 'DXT1' in format_str:
                size = max(1, (w + 3) // 4) * max(1, (h + 3) // 4) * 8
            elif 'DXT' in format_str:
                size = max(1, (w + 3) // 4) * max(1, (h + 3) // 4) * 16
            elif 'ARGB8888' in format_str:
                size = w * h * 4
            elif 'RGB888' in format_str:
                size = w * h * 3
            elif 'PAL8' in format_str:
                size = w * h
            else:
                size = w * h * 2
            
            total += size
            w = max(1, w // 2)
            h = max(1, h // 2)
        
        return total
    
    def _compress_to_dxt(self, rgba_data: bytes, width: int, height: int, format_str: str) -> bytes: #vers 1
        """Compress RGBA data to DXT format (placeholder)"""
        if not rgba_data:
            if 'DXT1' in format_str:
                size = max(1, (width + 3) // 4) * max(1, (height + 3) // 4) * 8
            else:
                size = max(1, (width + 3) // 4) * max(1, (height + 3) // 4) * 16
            return b'\x00' * size
        
        return rgba_data[:self._calculate_texture_size(width, height, format_str, 1)]

    def _build_texture_dictionary_from_sections(self, texture_sections, texture_count): #vers 1
        """Build texture dictionary from pre-built texture sections"""
        struct_size = 4
        struct_data = struct.pack('<I', texture_count)

        result = bytearray()

        total_size = 12 + struct_size + 12
        for tex_section in texture_sections:
            total_size += len(tex_section)

        result.extend(self._write_section_header(
            self.SECTION_TEXTURE_DICTIONARY,
            total_size - 12,
            self.RW_VERSION
        ))

        result.extend(self._write_section_header(
            self.SECTION_STRUCT,
            struct_size,
            self.RW_VERSION
        ))
        result.extend(struct_data)

        for tex_section in texture_sections:
            result.extend(tex_section)

        result.extend(self._write_section_header(
            self.SECTION_EXTENSION,
            0,
            self.RW_VERSION
        ))

        return result


def serialize_txd_file(textures: List[Dict], target_version: int = None, 
                      target_device: int = None) -> Optional[bytes]: #vers 1
    """
    Serialize texture list to TXD binary format
    
    Args:
        textures: List of texture dictionaries
        target_version: Target RenderWare version (optional)
        target_device: Target platform device (optional)
    
    Returns:
        bytes: Serialized TXD data or None on error
    """
    try:
        serializer = TXDSerializer()
        return serializer.serialize_txd(textures, target_version, target_device)
    except Exception as e:
        print(f"TXD serialization error: {e}")
        return None


# DOCUMENTATION
"""
TXD FILE STRUCTURE - VERSION 4 (REVERTED TO WORKING FORMAT):

TextureNative {
    Struct {
        Platform ID (4 bytes)
        Filter flags (4 bytes)
        Texture name (32 bytes)         <- INSIDE struct
        Alpha name (32 bytes)            <- INSIDE struct
        Raster format (4 bytes)
        D3D format (4 bytes)
        Width (2 bytes)
        Height (2 bytes)
        Depth (1 byte)
        Mipmap count (1 byte)
        Raster type (1 byte)
        Compression (1 byte)
        Total data size (4 bytes)
        
        === TEXTURE DATA ===
        Mipmap levels...
        Bumpmap data (if present)
        Reflection data (if present)
    }
    Extension {}
}

CHANGES FROM VERSION 3:
- REVERTED: Names back INSIDE struct as 32-byte fields
- This matches the parser's expectations (_parse_single_texture)
- v3's separate STRING sections caused corruption
- This is the 88-byte header format that IMG Factory uses
"""
