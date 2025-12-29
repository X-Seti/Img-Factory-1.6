#this belongs in /components.Txd_Converter.txd_converter.py - version 2
#!/usr/bin/env python3
"""
X-Seti - June26 2025 - TXD Converter - Complete Texture Conversion System
Credit MexUK 2007 IMG Factory 1.2 - Full TXD handling port
"""

import os
import struct
import io
from typing import List, Dict, Optional, Tuple, Union
from enum import Enum
from dataclasses import dataclass
from pathlib import Path
from PIL import Image, ImageOps
import zlib


class RWVersion(Enum):
    """RenderWare version constants"""
    RW_VERSION_3_0_0_0 = 0x30000
    RW_VERSION_3_1_0_1 = 0x31001
    RW_VERSION_3_3_0_2 = 0x33002
    RW_VERSION_3_4_0_3 = 0x34003
    RW_VERSION_3_5_0_0 = 0x35000
    RW_VERSION_3_6_0_3 = 0x36003
    RW_VERSION_3_7_0_2 = 0x37002


class TextureFormat(Enum):
    """Texture format types"""
    FORMAT_DEFAULT = 0x0000
    FORMAT_1555 = 0x0100
    FORMAT_565 = 0x0200
    FORMAT_4444 = 0x0300
    FORMAT_LUM8 = 0x0400
    FORMAT_8888 = 0x0500
    FORMAT_888 = 0x0600
    FORMAT_16 = 0x0700
    FORMAT_24 = 0x0800
    FORMAT_32 = 0x0900
    FORMAT_555 = 0x0A00
    FORMAT_AUTO_MIPMAP = 0x1000
    FORMAT_DXT1 = 0x0031545844  # 'DXT1'
    FORMAT_DXT3 = 0x0033545844  # 'DXT3'


class TextureFilter(Enum):
    """Texture filtering modes"""
    FILTER_NONE = 0x00
    FILTER_NEAREST = 0x01
    FILTER_LINEAR = 0x02
    FILTER_MIP_NEAREST = 0x03
    FILTER_MIP_LINEAR = 0x04
    FILTER_LINEAR_MIP_NEAREST = 0x05
    FILTER_LINEAR_MIP_LINEAR = 0x06


class TextureAddress(Enum):
    """Texture addressing modes"""
    ADDRESS_WRAP = 0x01
    ADDRESS_MIRROR = 0x02
    ADDRESS_CLAMP = 0x03
    ADDRESS_BORDER = 0x04


@dataclass
class RWSection:
    """RenderWare section header"""
    section_type: int
    section_size: int
    rw_version: int
    data: bytes = b''
    
    @classmethod
    def from_bytes(cls, data: bytes, offset: int = 0) -> 'RWSection':
        """Create section from bytes"""
        if len(data) < offset + 12:
            raise ValueError("Insufficient data for RW section header")
        
        section_type, section_size, rw_version = struct.unpack('<III', data[offset:offset+12])
        section_data = data[offset+12:offset+12+section_size] if section_size > 0 else b''
        
        return cls(section_type, section_size, rw_version, section_data)
    
    def to_bytes(self) -> bytes:
        """Convert section to bytes"""
        header = struct.pack('<III', self.section_type, len(self.data), self.rw_version)
        return header + self.data


@dataclass
class TextureInfo:
    """Texture information structure"""
    name: str = ""
    mask_name: str = ""
    format: TextureFormat = TextureFormat.FORMAT_8888
    width: int = 0
    height: int = 0
    depth: int = 32
    mipmaps: int = 1
    filter_flags: int = 0
    address_u: TextureAddress = TextureAddress.ADDRESS_WRAP
    address_v: TextureAddress = TextureAddress.ADDRESS_WRAP
    has_alpha: bool = False
    raster_data: bytes = b''
    palette_data: bytes = b''
    
    def get_size_bytes(self) -> int:
        """Get texture size in bytes"""
        return len(self.raster_data) + len(self.palette_data)


class TXDFile:
    """TXD (Texture Dictionary) file handler"""
    
    def __init__(self, file_path: str = ""):
        self.file_path = file_path
        self.rw_version = RWVersion.RW_VERSION_3_6_0_3.value
        self.textures: List[TextureInfo] = []
        self.platform = "PC"
        
        # RenderWare section IDs
        self.RW_SECTION_STRUCT = 0x0001
        self.RW_SECTION_STRING = 0x0002
        self.RW_SECTION_EXTENSION = 0x0003
        self.RW_SECTION_TEXTURE = 0x0006
        self.RW_SECTION_MATERIAL = 0x0007
        self.RW_SECTION_MATLIST = 0x0008
        self.RW_SECTION_GEOMETRY = 0x000F
        self.RW_SECTION_CLUMP = 0x0010
        self.RW_SECTION_ATOMIC = 0x0014
        self.RW_SECTION_TEXDICTIONARY = 0x0016
        self.RW_SECTION_ANIMDICTIONARY = 0x0017
        self.RW_SECTION_RASTER = 0x001E
        self.RW_SECTION_TEXTURENATIVE = 0x0015
    
    def open(self) -> bool:
        """Open and parse TXD file"""
        try:
            if not os.path.exists(self.file_path):
                return False
            
            with open(self.file_path, 'rb') as f:
                data = f.read()
            
            return self._parse_txd_data(data)
            
        except Exception as e:
            print(f"Error opening TXD file: {e}")
            return False
    
    def _parse_txd_data(self, data: bytes) -> bool:
        """Parse TXD data from bytes"""
        try:
            offset = 0
            
            # Parse main TXD header
            if len(data) < 12:
                return False
            
            main_section = RWSection.from_bytes(data, offset)
            
            if main_section.section_type != self.RW_SECTION_TEXDICTIONARY:
                return False
            
            self.rw_version = main_section.rw_version
            offset += 12
            
            # Parse struct section
            if offset + 12 > len(data):
                return False
            
            struct_section = RWSection.from_bytes(data, offset)
            offset += 12 + struct_section.section_size
            
            # Get texture count from struct data
            if len(struct_section.data) < 4:
                return False
            
            texture_count = struct.unpack('<I', struct_section.data[:4])[0]
            
            # Parse each texture
            for i in range(texture_count):
                if offset >= len(data):
                    break
                
                texture_info = self._parse_texture_native(data, offset)
                if texture_info:
                    self.textures.append(texture_info[0])
                    offset = texture_info[1]
                else:
                    break
            
            return True
            
        except Exception as e:
            print(f"Error parsing TXD data: {e}")
            return False
    
    def _parse_texture_native(self, data: bytes, offset: int) -> Optional[Tuple[TextureInfo, int]]:
        """Parse texture native section"""
        try:
            if offset + 12 > len(data):
                return None
            
            # Parse texture native header
            texture_section = RWSection.from_bytes(data, offset)
            if texture_section.section_type != self.RW_SECTION_TEXTURENATIVE:
                return None
            
            start_offset = offset
            offset += 12
            
            # Parse struct section
            if offset + 12 > len(data):
                return None
            
            struct_section = RWSection.from_bytes(data, offset)
            offset += 12 + struct_section.section_size
            
            # Parse texture data from struct
            texture_info = self._parse_texture_struct(struct_section.data)
            
            # Parse string section (texture name)
            if offset + 12 <= len(data):
                string_section = RWSection.from_bytes(data, offset)
                if string_section.section_type == self.RW_SECTION_STRING:
                    texture_info.name = string_section.data.rstrip(b'\x00').decode('ascii', errors='ignore')
                    offset += 12 + string_section.section_size
            
            # Parse mask name if present
            if offset + 12 <= len(data):
                mask_section = RWSection.from_bytes(data, offset)
                if mask_section.section_type == self.RW_SECTION_STRING:
                    texture_info.mask_name = mask_section.data.rstrip(b'\x00').decode('ascii', errors='ignore')
                    offset += 12 + mask_section.section_size
            
            # Parse raster section
            if offset + 12 <= len(data):
                raster_section = RWSection.from_bytes(data, offset)
                if raster_section.section_type == self.RW_SECTION_RASTER:
                    self._parse_raster_data(raster_section.data, texture_info)
                    offset += 12 + raster_section.section_size
            
            # Skip extension section if present
            if offset + 12 <= len(data):
                ext_section = RWSection.from_bytes(data, offset)
                if ext_section.section_type == self.RW_SECTION_EXTENSION:
                    offset += 12 + ext_section.section_size
            
            # Calculate final offset
            final_offset = start_offset + 12 + texture_section.section_size
            
            return texture_info, final_offset
            
        except Exception as e:
            print(f"Error parsing texture native: {e}")
            return None
    
    def _parse_texture_struct(self, data: bytes) -> TextureInfo:
        """Parse texture structure data"""
        texture_info = TextureInfo()
        
        try:
            if len(data) < 8:
                return texture_info
            
            # Parse basic texture info
            platform_id, filter_flags = struct.unpack('<II', data[:8])
            texture_info.filter_flags = filter_flags
            
            # Platform-specific parsing
            if platform_id == 8:  # PC Platform
                self.platform = "PC"
                if len(data) >= 16:
                    texture_info.address_u = TextureAddress(data[8])
                    texture_info.address_v = TextureAddress(data[9])
                    # Additional PC-specific data would be parsed here
            
        except Exception as e:
            print(f"Error parsing texture struct: {e}")
        
        return texture_info
    
    def _parse_raster_data(self, data: bytes, texture_info: TextureInfo):
        """Parse raster data section"""
        try:
            if len(data) < 16:
                return
            
            # Parse raster header
            format_flags, width, height, depth = struct.unpack('<IIII', data[:16])
            
            texture_info.width = width
            texture_info.height = height
            texture_info.depth = depth
            texture_info.format = TextureFormat(format_flags & 0xFFFF)
            texture_info.has_alpha = (format_flags & 0x10000) != 0
            
            # Calculate expected data size
            bytes_per_pixel = depth // 8
            expected_size = width * height * bytes_per_pixel
            
            # Extract raster data
            data_offset = 16
            
            # Check for palette data (for 8-bit textures)
            if depth == 8:
                if len(data) >= data_offset + 1024:  # 256 * 4 bytes palette
                    texture_info.palette_data = data[data_offset:data_offset + 1024]
                    data_offset += 1024
            
            # Extract main raster data
            if len(data) >= data_offset + expected_size:
                texture_info.raster_data = data[data_offset:data_offset + expected_size]
            else:
                # Take whatever data is available
                texture_info.raster_data = data[data_offset:]
            
        except Exception as e:
            print(f"Error parsing raster data: {e}")
    
    def save(self, output_path: str = None) -> bool:
        """Save TXD file"""
        if not output_path:
            output_path = self.file_path
        
        try:
            txd_data = self._build_txd_data()
            
            with open(output_path, 'wb') as f:
                f.write(txd_data)
            
            return True
            
        except Exception as e:
            print(f"Error saving TXD file: {e}")
            return False
    
    def _build_txd_data(self) -> bytes:
        """Build TXD data from textures"""
        # Build texture native sections
        texture_sections = []
        for texture in self.textures:
            texture_section = self._build_texture_native(texture)
            texture_sections.append(texture_section)
        
        # Build main struct section
        struct_data = struct.pack('<I', len(self.textures))  # Texture count
        struct_section = RWSection(self.RW_SECTION_STRUCT, len(struct_data), self.rw_version, struct_data)
        
        # Calculate total size
        total_size = 12 + len(struct_section.data)  # struct section
        for section in texture_sections:
            total_size += len(section)
        
        # Build main TXD section
        main_section = RWSection(self.RW_SECTION_TEXDICTIONARY, total_size, self.rw_version)
        
        # Combine all data
        result = main_section.to_bytes()[:-len(main_section.data)]  # Header only
        result += struct_section.to_bytes()
        
        for section in texture_sections:
            result += section
        
        return result
    
    def _build_texture_native(self, texture: TextureInfo) -> bytes:
        """Build texture native section"""
        # Build raster section
        raster_data = self._build_raster_section(texture)
        raster_section = RWSection(self.RW_SECTION_RASTER, len(raster_data), self.rw_version, raster_data)
        
        # Build texture name string section
        name_data = texture.name.encode('ascii') + b'\x00'
        name_section = RWSection(self.RW_SECTION_STRING, len(name_data), self.rw_version, name_data)
        
        # Build mask name string section
        mask_data = texture.mask_name.encode('ascii') + b'\x00' if texture.mask_name else b'\x00'
        mask_section = RWSection(self.RW_SECTION_STRING, len(mask_data), self.rw_version, mask_data)
        
        # Build struct section
        struct_data = self._build_texture_struct(texture)
        struct_section = RWSection(self.RW_SECTION_STRUCT, len(struct_data), self.rw_version, struct_data)
        
        # Calculate total size
        total_size = (12 + len(struct_section.data) + 
                     12 + len(name_section.data) + 
                     12 + len(mask_section.data) + 
                     12 + len(raster_section.data))
        
        # Build texture native header
        texture_section = RWSection(self.RW_SECTION_TEXTURENATIVE, total_size, self.rw_version)
        
        # Combine all sections
        result = texture_section.to_bytes()[:-len(texture_section.data)]  # Header only
        result += struct_section.to_bytes()
        result += name_section.to_bytes()
        result += mask_section.to_bytes()
        result += raster_section.to_bytes()
        
        return result
    
    def _build_texture_struct(self, texture: TextureInfo) -> bytes:
        """Build texture structure data"""
        # PC platform format
        platform_id = 8  # PC
        filter_flags = texture.filter_flags
        
        struct_data = struct.pack('<II', platform_id, filter_flags)
        struct_data += bytes([texture.address_u.value, texture.address_v.value])
        struct_data += b'\x00\x00'  # Padding
        
        return struct_data
    
    def _build_raster_section(self, texture: TextureInfo) -> bytes:
        """Build raster section data"""
        # Build raster header
        format_flags = texture.format.value
        if texture.has_alpha:
            format_flags |= 0x10000
        
        header = struct.pack('<IIII', format_flags, texture.width, texture.height, texture.depth)
        
        # Combine header, palette (if any), and raster data
        result = header
        if texture.palette_data:
            result += texture.palette_data
        result += texture.raster_data
        
        return result
    
    def add_texture_from_image(self, image_path: str, texture_name: str = None) -> bool:
        """Add texture from image file"""
        try:
            if not texture_name:
                texture_name = os.path.splitext(os.path.basename(image_path))[0]
            
            # Load image
            image = Image.open(image_path)
            
            # Convert to appropriate format
            if image.mode not in ['RGB', 'RGBA']:
                if 'transparency' in image.info:
                    image = image.convert('RGBA')
                else:
                    image = image.convert('RGB')
            
            # Create texture info
            texture_info = TextureInfo()
            texture_info.name = texture_name
            texture_info.width = image.width
            texture_info.height = image.height
            texture_info.has_alpha = image.mode == 'RGBA'
            texture_info.depth = 32 if image.mode == 'RGBA' else 24
            texture_info.format = TextureFormat.FORMAT_8888 if image.mode == 'RGBA' else TextureFormat.FORMAT_888
            
            # Convert image data to raster format
            if image.mode == 'RGBA':
                # Convert RGBA to BGRA (RenderWare format)
                r, g, b, a = image.split()
                image = Image.merge('RGBA', (b, g, r, a))
            else:
                # Convert RGB to BGR (RenderWare format)
                r, g, b = image.split()
                image = Image.merge('RGB', (b, g, r))
            
            texture_info.raster_data = image.tobytes()
            
            # Add to textures list
            self.textures.append(texture_info)
            
            return True
            
        except Exception as e:
            print(f"Error adding texture from image: {e}")
            return False
    
    def extract_texture_to_image(self, texture_index: int, output_path: str) -> bool:
        """Extract texture to image file"""
        try:
            if texture_index >= len(self.textures):
                return False
            
            texture = self.textures[texture_index]
            
            if not texture.raster_data:
                return False
            
            # Create image from raster data
            if texture.depth == 32:
                # RGBA format
                image = Image.frombytes('RGBA', (texture.width, texture.height), texture.raster_data)
                # Convert BGRA to RGBA
                b, g, r, a = image.split()
                image = Image.merge('RGBA', (r, g, b, a))
            elif texture.depth == 24:
                # RGB format
                image = Image.frombytes('RGB', (texture.width, texture.height), texture.raster_data)
                # Convert BGR to RGB
                b, g, r = image.split()
                image = Image.merge('RGB', (r, g, b))
            elif texture.depth == 8:
                # Paletted format
                if texture.palette_data:
                    # Create palette
                    palette = []
                    for i in range(0, len(texture.palette_data), 4):
                        b, g, r, a = texture.palette_data[i:i+4]
                        palette.extend([r, g, b])
                    
                    image = Image.frombytes('P', (texture.width, texture.height), texture.raster_data)
                    image.putpalette(palette)
                    image = image.convert('RGB')
                else:
                    # Grayscale
                    image = Image.frombytes('L', (texture.width, texture.height), texture.raster_data)
            else:
                print(f"Unsupported texture depth: {texture.depth}")
                return False
            
            # Save image
            image.save(output_path)
            return True
            
        except Exception as e:
            print(f"Error extracting texture to image: {e}")
            return False
    
    def extract_all_textures(self, output_dir: str) -> int:
        """Extract all textures to directory"""
        os.makedirs(output_dir, exist_ok=True)
        extracted_count = 0
        
        for i, texture in enumerate(self.textures):
            try:
                # Determine output filename
                if texture.name:
                    filename = f"{texture.name}.png"
                else:
                    filename = f"texture_{i:03d}.png"
                
                output_path = os.path.join(output_dir, filename)
                
                if self.extract_texture_to_image(i, output_path):
                    extracted_count += 1
                    
            except Exception as e:
                print(f"Error extracting texture {i}: {e}")
        
        return extracted_count
    
    def replace_texture(self, texture_index: int, image_path: str) -> bool:
        """Replace existing texture with new image"""
        try:
            if texture_index >= len(self.textures):
                return False
            
            # Load new image
            image = Image.open(image_path)
            
            # Get existing texture info
            texture = self.textures[texture_index]
            original_name = texture.name
            
            # Remove old texture
            del self.textures[texture_index]
            
            # Add new texture
            if self.add_texture_from_image(image_path, original_name):
                # Move new texture to correct position
                new_texture = self.textures.pop()
                self.textures.insert(texture_index, new_texture)
                return True
            
            return False
            
        except Exception as e:
            print(f"Error replacing texture: {e}")
            return False
    
    def remove_texture(self, texture_index: int) -> bool:
        """Remove texture from TXD"""
        try:
            if texture_index >= len(self.textures):
                return False
            
            del self.textures[texture_index]
            return True
            
        except Exception as e:
            print(f"Error removing texture: {e}")
            return False
    
    def get_texture_info(self, texture_index: int) -> Optional[Dict]:
        """Get detailed texture information"""
        try:
            if texture_index >= len(self.textures):
                return None
            
            texture = self.textures[texture_index]
            
            return {
                'name': texture.name,
                'mask_name': texture.mask_name,
                'width': texture.width,
                'height': texture.height,
                'depth': texture.depth,
                'format': texture.format.name,
                'has_alpha': texture.has_alpha,
                'size_bytes': texture.get_size_bytes(),
                'mipmaps': texture.mipmaps,
                'filter_flags': texture.filter_flags,
                'address_u': texture.address_u.name,
                'address_v': texture.address_v.name
            }
            
        except Exception as e:
            print(f"Error getting texture info: {e}")
            return None
    
    def optimize_textures(self, max_size: int = 1024, quality: int = 85) -> int:
        """Optimize textures by resizing and compressing"""
        optimized_count = 0
        
        for texture in self.textures:
            try:
                if texture.width > max_size or texture.height > max_size:
                    # Need to resize
                    if texture.depth == 32:
                        image = Image.frombytes('RGBA', (texture.width, texture.height), texture.raster_data)
                        b, g, r, a = image.split()
                        image = Image.merge('RGBA', (r, g, b, a))
                    elif texture.depth == 24:
                        image = Image.frombytes('RGB', (texture.width, texture.height), texture.raster_data)
                        b, g, r = image.split()
                        image = Image.merge('RGB', (r, g, b))
                    else:
                        continue  # Skip unsupported formats
                    
                    # Calculate new size maintaining aspect ratio
                    ratio = min(max_size / texture.width, max_size / texture.height)
                    new_width = int(texture.width * ratio)
                    new_height = int(texture.height * ratio)
                    
                    # Resize image
                    image = image.resize((new_width, new_height), Image.Resampling.LANCZOS)
                    
                    # Update texture info
                    texture.width = new_width
                    texture.height = new_height
                    
                    # Convert back to raster data
                    if texture.depth == 32:
                        r, g, b, a = image.split()
                        image = Image.merge('RGBA', (b, g, r, a))
                    else:
                        r, g, b = image.split()
                        image = Image.merge('RGB', (b, g, r))
                    
                    texture.raster_data = image.tobytes()
                    optimized_count += 1
                    
            except Exception as e:
                print(f"Error optimizing texture {texture.name}: {e}")
        
        return optimized_count
    
    def create_new(self, file_path: str) -> bool:
        """Create new empty TXD file"""
        self.file_path = file_path
        self.textures = []
        self.rw_version = RWVersion.RW_VERSION_3_6_0_3.value
        return True


class TXDConverter:
    """TXD conversion utilities"""
    
    @staticmethod
    def convert_images_to_txd(image_paths: List[str], output_path: str) -> bool:
        """Convert multiple images to TXD file"""
        try:
            txd = TXDFile()
            txd.create_new(output_path)
            
            for image_path in image_paths:
                if not txd.add_texture_from_image(image_path):
                    print(f"Failed to add texture: {image_path}")
                    return False
            
            return txd.save()
            
        except Exception as e:
            print(f"Error converting images to TXD: {e}")
            return False
    
    @staticmethod
    def convert_txd_to_images(txd_path: str, output_dir: str) -> int:
        """Convert TXD file to images"""
        try:
            txd = TXDFile(txd_path)
            if not txd.open():
                return 0
            
            return txd.extract_all_textures(output_dir)
            
        except Exception as e:
            print(f"Error converting TXD to images: {e}")
            return 0
    
    @staticmethod
    def merge_txd_files(txd_paths: List[str], output_path: str) -> bool:
        """Merge multiple TXD files into one"""
        try:
            merged_txd = TXDFile()
            merged_txd.create_new(output_path)
            
            for txd_path in txd_paths:
                source_txd = TXDFile(txd_path)
                if source_txd.open():
                    # Add all textures from source TXD
                    for texture in source_txd.textures:
                        merged_txd.textures.append(texture)
                else:
                    print(f"Failed to open TXD: {txd_path}")
            
            return merged_txd.save()
            
        except Exception as e:
            print(f"Error merging TXD files: {e}")
            return False
    
    @staticmethod
    def split_txd_file(txd_path: str, output_dir: str) -> int:
        """Split TXD file into individual TXD files per texture"""
        try:
            source_txd = TXDFile(txd_path)
            if not source_txd.open():
                return 0
            
            os.makedirs(output_dir, exist_ok=True)
            created_count = 0
            
            for i, texture in enumerate(source_txd.textures):
                # Create new TXD with single texture
                new_txd = TXDFile()
                filename = texture.name if texture.name else f"texture_{i:03d}"
                output_path = os.path.join(output_dir, f"{filename}.txd")
                
                new_txd.create_new(output_path)
                new_txd.textures.append(texture)
                
                if new_txd.save():
                    created_count += 1
            
            return created_count
            
        except Exception as e:
            print(f"Error splitting TXD file: {e}")
            return 0
    
    @staticmethod
    def optimize_txd_file(txd_path: str, output_path: str = None, max_size: int = 1024) -> bool:
        """Optimize TXD file by resizing textures"""
        try:
            txd = TXDFile(txd_path)
            if not txd.open():
                return False
            
            optimized_count = txd.optimize_textures(max_size)
            print(f"Optimized {optimized_count} textures")
            
            if not output_path:
                output_path = txd_path
            
            return txd.save(output_path)
            
        except Exception as e:
            print(f"Error optimizing TXD file: {e}")
            return False


# TXD Analysis Tools
class TXDAnalyzer:
    """TXD file analysis utilities"""
    
    @staticmethod
    def analyze_txd_file(txd_path: str) -> Dict:
        """Analyze TXD file and return detailed information"""
        analysis = {
            'file_path': txd_path,
            'file_size': 0,
            'texture_count': 0,
            'total_texture_memory': 0,
            'textures': [],
            'format_distribution': {},
            'size_distribution': {},
            'has_alpha_textures': 0,
            'average_texture_size': 0,
            'largest_texture': None,
            'smallest_texture': None,
            'rw_version': 0
        }
        
        try:
            analysis['file_size'] = os.path.getsize(txd_path)
            
            txd = TXDFile(txd_path)
            if not txd.open():
                return analysis
            
            analysis['texture_count'] = len(txd.textures)
            analysis['rw_version'] = txd.rw_version
            
            texture_sizes = []
            
            for i, texture in enumerate(txd.textures):
                texture_info = {
                    'index': i,
                    'name': texture.name,
                    'width': texture.width,
                    'height': texture.height,
                    'depth': texture.depth,
                    'format': texture.format.name,
                    'has_alpha': texture.has_alpha,
                    'memory_size': texture.get_size_bytes()
                }
                
                analysis['textures'].append(texture_info)
                analysis['total_texture_memory'] += texture_info['memory_size']
                texture_sizes.append(texture_info['memory_size'])
                
                # Count alpha textures
                if texture.has_alpha:
                    analysis['has_alpha_textures'] += 1
                
                # Format distribution
                format_name = texture.format.name
                analysis['format_distribution'][format_name] = analysis['format_distribution'].get(format_name, 0) + 1
                
                # Size distribution
                size_key = f"{texture.width}x{texture.height}"
                analysis['size_distribution'][size_key] = analysis['size_distribution'].get(size_key, 0) + 1
            
            # Calculate statistics
            if texture_sizes:
                analysis['average_texture_size'] = sum(texture_sizes) / len(texture_sizes)
                
                largest_idx = texture_sizes.index(max(texture_sizes))
                smallest_idx = texture_sizes.index(min(texture_sizes))
                
                analysis['largest_texture'] = analysis['textures'][largest_idx]
                analysis['smallest_texture'] = analysis['textures'][smallest_idx]
            
        except Exception as e:
            print(f"Error analyzing TXD file: {e}")
        
        return analysis
    
    @staticmethod
    def find_duplicate_textures(txd_path: str) -> List[List[int]]:
        """Find duplicate textures in TXD file"""
        duplicates = []
        
        try:
            txd = TXDFile(txd_path)
            if not txd.open():
                return duplicates
            
            # Calculate hashes for each texture
            texture_hashes = {}
            
            for i, texture in enumerate(txd.textures):
                if texture.raster_data:
                    texture_hash = hashlib.md5(texture.raster_data).hexdigest()
                    
                    if texture_hash in texture_hashes:
                        texture_hashes[texture_hash].append(i)
                    else:
                        texture_hashes[texture_hash] = [i]
            
            # Find groups with multiple textures
            for texture_hash, indices in texture_hashes.items():
                if len(indices) > 1:
                    duplicates.append(indices)
            
        except Exception as e:
            print(f"Error finding duplicate textures: {e}")
        
        return duplicates


# Example usage and testing
if __name__ == "__main__":
    # Test TXD functionality
    print("Testing TXD Converter...")
    
    # Test loading TXD file
    test_txd_path = "test.txd"
    if os.path.exists(test_txd_path):
        txd = TXDFile(test_txd_path)
        if txd.open():
            print(f"✓ Loaded TXD: {len(txd.textures)} textures")
            
            # Extract textures
            extracted = txd.extract_all_textures("extracted_textures")
            print(f"✓ Extracted {extracted} textures")
            
            # Analyze TXD
            analysis = TXDAnalyzer.analyze_txd_file(test_txd_path)
            print(f"✓ Analysis: {analysis['texture_count']} textures, {analysis['total_texture_memory']} bytes")
            
            # Test optimization
            if TXDConverter.optimize_txd_file(test_txd_path, "optimized.txd", 512):
                print("✓ TXD optimized")
        else:
            print("✗ Failed to load TXD file")
    else:
        print("No test TXD file found")
    
    # Test creating TXD from images
    image_files = ["test1.png", "test2.png"]
    existing_images = [img for img in image_files if os.path.exists(img)]
    
    if existing_images:
        if TXDConverter.convert_images_to_txd(existing_images, "created.txd"):
            print(f"✓ Created TXD from {len(existing_images)} images")
        else:
            print("✗ Failed to create TXD from images")
    
    print("TXD Converter tests completed!")
