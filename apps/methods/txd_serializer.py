#!/usr/bin/env python3
#this belongs in methods/ txd_serializer.py - Version: 4
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
    


    def _parse_single_texture(self, txd_data, offset, index): #vers 5
        """
        Parse single texture from TXD - FIXED: Preserves original binary data to prevent corruption

        Args:
            txd_data: TXD binary data
            offset: Current offset in data
            index: Texture index

        Returns:
            dict: Texture dictionary with all map data and original binary preservation
        """
        import struct

        tex = {
            'name': f'texture_{index}',
            'width': 0,
            'height': 0,
            'depth': 32,
            'format': 'DXT1',
            'has_alpha': False,
            'mipmaps': 1,
            'rgba_data': b'',              # For display (RGBA format)
            'compressed_data': b'',        # ADDED: Original DXT compressed data
            'original_bgra_data': b'',     # ADDED: Original uncompressed BGRA data
            'mipmap_levels': [],
            'bumpmap_data': b'',
            'bumpmap_type': 0,
            'has_bumpmap': False,
            'reflection_map': b'',
            'fresnel_map': b'',
            'has_reflection': False,
            'raster_format_flags': 0,
            'filter_flags': 0x1102
        }

        try:
            # TextureNative structure (0x15)
            parent_type, parent_size, parent_version = struct.unpack('<III', txd_data[offset:offset+12])

            if parent_type != 0x15:
                return tex

            # Struct section (0x01)
            struct_offset = offset + 12
            struct_type, struct_size, struct_version = struct.unpack('<III', txd_data[struct_offset:struct_offset+12])

            if struct_type != 0x01:
                return tex

            pos = struct_offset + 12

            # === 88-byte header ===
            # Platform ID (4 bytes) + Filter flags (4 bytes)
            platform_id, filter_flags = struct.unpack('<II', txd_data[pos:pos+8])
            tex['filter_flags'] = filter_flags
            pos += 8

            # Texture name (32 bytes, null-terminated)
            name_bytes = txd_data[pos:pos+32]
            tex['name'] = name_bytes.rstrip(b'\x00').decode('ascii', errors='ignore') or f'texture_{index}'
            pos += 32

            # Alpha/mask name (32 bytes, null-terminated)
            mask_bytes = txd_data[pos:pos+32]
            alpha_name = mask_bytes.rstrip(b'\x00').decode('ascii', errors='ignore')
            if alpha_name:
                tex['alpha_name'] = alpha_name
                tex['has_alpha'] = True
            pos += 32

            # Raster format (4 bytes) + D3D format (4 bytes) + Width/Height (2+2) + Depth/Levels/Type (1+1+1)
            raster_format_flags, d3d_format, width, height, depth, num_levels, raster_type = struct.unpack(
                '<IIHHBBB', txd_data[pos:pos+15]
            )

            tex['width'] = width
            tex['height'] = height
            tex['depth'] = depth
            tex['mipmaps'] = num_levels
            tex['raster_format_flags'] = raster_format_flags

            # Check for bumpmap flag (bit 0x10)
            if raster_format_flags & 0x10:
                tex['has_bumpmap'] = True

            pos += 15

            # Determine format from raster_format_flags
            format_code = raster_format_flags & 0xFFFF

            format_map = {
                0x31545844: 'DXT1',  # 'DXT1' fourcc
                0x33545844: 'DXT3',  # 'DXT3' fourcc
                0x35545844: 'DXT5',  # 'DXT5' fourcc
                0x15: 'ARGB8888',
                0x14: 'RGB888',
                0x02: 'ARGB1555',
                0x01: 'RGB565',
                0x05: 'PAL8',
            }

            tex['format'] = format_map.get(format_code, 'DXT1')

            # Check alpha flag in raster format
            if raster_format_flags & 0x10000:
                tex['has_alpha'] = True

            # Compression byte (1 byte) + Data size (4 bytes)
            compression = struct.unpack('<B', txd_data[pos:pos+1])[0]
            pos += 1

            data_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
            pos += 4

            # === TEXTURE DATA ===
            data_offset = pos

            # CRITICAL FIX: Store original binary data before any conversion
            if data_offset + data_size <= len(txd_data):
                original_data = txd_data[data_offset:data_offset+data_size]

                if 'DXT' in tex['format']:
                    # DXT compressed texture
                    tex['compressed_data'] = original_data  # Store original compressed

                    # Decompress to RGBA for display
                    decompressed = self._decompress_texture(original_data, width, height, tex['format'])
                    if decompressed:
                        tex['rgba_data'] = decompressed
                    else:
                        # Decompression failed, create blank
                        tex['rgba_data'] = b'\x00' * (width * height * 4)
                else:
                    # Uncompressed texture (stored as BGRA in RenderWare)
                    tex['original_bgra_data'] = original_data  # Store original BGRA

                    # Convert BGRA to RGBA for display
                    converted = self._decompress_uncompressed(original_data, width, height, tex['format'])
                    if converted:
                        tex['rgba_data'] = converted
                    else:
                        # Conversion failed, create blank
                        tex['rgba_data'] = b'\x00' * (width * height * 4)

            # === MIPMAPS ===
            if num_levels > 1:
                mipmap_offset = data_offset

                for level in range(num_levels):
                    level_width = max(1, width >> level)
                    level_height = max(1, height >> level)

                    # Calculate level size
                    if 'DXT1' in tex['format']:
                        level_size = max(1, (level_width + 3) // 4) * max(1, (level_height + 3) // 4) * 8
                    elif 'DXT' in tex['format']:
                        level_size = max(1, (level_width + 3) // 4) * max(1, (level_height + 3) // 4) * 16
                    elif 'ARGB8888' in tex['format']:
                        level_size = level_width * level_height * 4
                    elif 'RGB888' in tex['format']:
                        level_size = level_width * level_height * 3
                    else:
                        level_size = level_width * level_height * 4

                    if mipmap_offset + level_size <= data_offset + data_size:
                        level_original_data = txd_data[mipmap_offset:mipmap_offset+level_size]

                        mipmap = {
                            'level': level,
                            'width': level_width,
                            'height': level_height,
                            'compressed_size': level_size,
                            'compressed_data': level_original_data,  # Store original
                            'original_bgra_data': level_original_data if 'DXT' not in tex['format'] else b'',
                            'rgba_data': b''  # Decompress on demand
                        }

                        tex['mipmap_levels'].append(mipmap)
                        mipmap_offset += level_size

            # === BUMPMAP DATA ===
            pos = data_offset + data_size

            try:
                if pos + 4 <= len(txd_data):
                    bumpmap_size = struct.unpack('<I', txd_data[pos:pos+4])[0]

                    # Validate bumpmap size
                    max_bumpmap_size = width * height * 4

                    if 0 < bumpmap_size <= max_bumpmap_size and pos + 5 + bumpmap_size <= len(txd_data):
                        pos += 4
                        bumpmap_type = struct.unpack('<B', txd_data[pos:pos+1])[0]
                        pos += 1

                        tex['bumpmap_data'] = txd_data[pos:pos+bumpmap_size]
                        tex['bumpmap_type'] = bumpmap_type
                        tex['has_bumpmap'] = True
                        pos += bumpmap_size

                        if self.main_window and hasattr(self.main_window, 'log_message'):
                            bumpmap_types = {0: "Height", 1: "Normal", 2: "Combined"}
                            type_str = bumpmap_types.get(bumpmap_type, f"Type {bumpmap_type}")
                            self.main_window.log_message(
                                f"  🗻 Bumpmap: {type_str} ({bumpmap_size} bytes)"
                            )
            except Exception:
                pass  # Silently ignore bumpmap errors

            # === REFLECTION MAPS ===
            try:
                if pos + 4 <= len(txd_data):
                    reflection_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
                    expected_reflection_size = width * height * 3  # RGB

                    if reflection_size == expected_reflection_size and pos + 4 + reflection_size <= len(txd_data):
                        pos += 4
                        tex['reflection_map'] = txd_data[pos:pos+reflection_size]
                        tex['has_reflection'] = True
                        pos += reflection_size

                        # Fresnel map
                        if pos + 4 <= len(txd_data):
                            fresnel_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
                            pos += 4

                            expected_fresnel_size = width * height  # Grayscale
                            if fresnel_size == expected_fresnel_size and pos + fresnel_size <= len(txd_data):
                                tex['fresnel_map'] = txd_data[pos:pos+fresnel_size]
                                pos += fresnel_size

                                if self.main_window and hasattr(self.main_window, 'log_message'):
                                    self.main_window.log_message(
                                        f"  🌈 Reflection: Vector ({reflection_size}B) + Fresnel ({fresnel_size}B)"
                                    )
            except Exception:
                pass  # Silently ignore reflection errors

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"❌ Texture parse error: {str(e)}")

        return tex


    def _write_section_header(self, section_type: int, size: int, version: int) -> bytes: #vers 1
        """Write RenderWare section header"""
        return struct.pack('<III', section_type, size, version)
    
    def _get_format_code(self, format_str: str, has_alpha: bool) -> int: #vers 1
        """Get RenderWare format code"""
        format_map = {
            'DXT1': 0x31545844,
            'DXT3': 0x33545844,
            'DXT5': 0x35545844,
            'ARGB8888': 0x15,
            'RGB888': 0x14,
            'ARGB1555': 0x02,
            'RGB565': 0x01,
            'PAL8': 0x05,
        }
        return format_map.get(format_str, 0x31545844)
    
    def _get_d3d_format(self, format_str: str) -> int: #vers 1
        """Get D3D format code"""
        d3d_map = {
            'DXT1': 0x31545844,
            'DXT3': 0x33545844,
            'DXT5': 0x35545844,
            'ARGB8888': 21,
            'RGB888': 20,
            'ARGB1555': 25,
            'RGB565': 23,
            'PAL8': 0x14,
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
