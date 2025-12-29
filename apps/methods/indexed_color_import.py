# this belongs in methods/ indexed_color_import.py - Version: 1
# X-Seti - October09 2025 - IMG Factory 1.5 - 8-bit Indexed Import
"""
Comprehensive 8-bit indexed color format import for retro/legacy formats:
- BMP (8-bit indexed)
- PCX (ZSoft Paintbrush)
- GIF (with transparency)
- PNG (8-bit indexed mode)
- TGA (8-bit indexed)
- LBM (Deluxe Paint)
"""

##Methods list -
# load_indexed_image
# load_bmp_8bit
# load_pcx_8bit
# load_gif_8bit
# load_png_8bit
# load_tga_8bit
# _expand_palette_to_rgba
# is_indexed_format

import struct
from PIL import Image


def load_indexed_image(file_path): #vers 1
    """
    Auto-detect and load 8-bit indexed color image
    
    Returns:
        dict: {
            'width': int,
            'height': int,
            'rgba_data': bytes,
            'has_alpha': bool,
            'format': str,
            'original_format': str
        }
    """
    ext = file_path.lower().split('.')[-1]
    
    # Route to appropriate loader
    if ext == 'bmp':
        return load_bmp_8bit(file_path)
    elif ext == 'pcx':
        return load_pcx_8bit(file_path)
    elif ext == 'gif':
        return load_gif_8bit(file_path)
    elif ext in ('png',):
        return load_png_8bit(file_path)
    elif ext in ('tga', 'targa'):
        return load_tga_8bit(file_path)
    elif ext in ('lbm', 'iff', 'ilbm'):
        # Use IFF loader from iff_import.py
        from apps.methods.iff_import import load_iff_image
        return load_iff_image(file_path)
    else:
        # Try PIL as fallback
        return _load_with_pil(file_path)


def load_bmp_8bit(file_path): #vers 1
    """Load 8-bit indexed BMP file"""
    try:
        with open(file_path, 'rb') as f:
            # Read BMP header
            header = f.read(54)
            
            if header[:2] != b'BM':
                return None
            
            # Parse header
            offset = struct.unpack('<I', header[10:14])[0]
            width = struct.unpack('<I', header[18:22])[0]
            height = struct.unpack('<I', header[22:26])[0]
            bit_depth = struct.unpack('<H', header[28:30])[0]
            
            if bit_depth != 8:
                return _load_with_pil(file_path)
            
            # Read palette (256 colors × 4 bytes BGRA)
            f.seek(54)
            palette_data = f.read(1024)
            palette = []
            
            for i in range(256):
                b = palette_data[i * 4]
                g = palette_data[i * 4 + 1]
                r = palette_data[i * 4 + 2]
                palette.append((r, g, b))
            
            # Read pixel data
            f.seek(offset)
            
            # BMP rows are padded to 4-byte boundary
            row_size = ((width + 3) // 4) * 4
            rgba_data = bytearray(width * height * 4)
            
            # BMP is stored bottom-up
            for y in range(height - 1, -1, -1):
                row_data = f.read(row_size)
                for x in range(width):
                    if x < len(row_data):
                        palette_idx = row_data[x]
                        r, g, b = palette[palette_idx]
                        
                        pixel_pos = (y * width + x) * 4
                        rgba_data[pixel_pos] = r
                        rgba_data[pixel_pos + 1] = g
                        rgba_data[pixel_pos + 2] = b
                        rgba_data[pixel_pos + 3] = 255
            
            return {
                'width': width,
                'height': height,
                'rgba_data': bytes(rgba_data),
                'has_alpha': False,
                'format': 'RGB888',
                'original_format': 'BMP-8bit'
            }
            
    except Exception as e:
        print(f"BMP 8-bit import error: {e}")
        return _load_with_pil(file_path)


def load_pcx_8bit(file_path): #vers 1
    """Load 8-bit PCX file (ZSoft Paintbrush)"""
    try:
        with open(file_path, 'rb') as f:
            data = f.read()
        
        # Verify PCX header
        if data[0] != 0x0A:  # PCX signature
            return None
        
        version = data[1]
        encoding = data[2]
        bits_per_pixel = data[3]
        
        # Only handle 8-bit
        if bits_per_pixel != 8:
            return _load_with_pil(file_path)
        
        # Get dimensions
        x_min = struct.unpack('<H', data[4:6])[0]
        y_min = struct.unpack('<H', data[6:8])[0]
        x_max = struct.unpack('<H', data[8:10])[0]
        y_max = struct.unpack('<H', data[10:12])[0]
        
        width = x_max - x_min + 1
        height = y_max - y_min + 1
        
        num_planes = data[65]
        bytes_per_line = struct.unpack('<H', data[66:68])[0]
        
        # Find palette (last 768 bytes if version 5)
        palette = []
        if version == 5 and data[-769] == 0x0C:
            palette_data = data[-768:]
            for i in range(256):
                r = palette_data[i * 3]
                g = palette_data[i * 3 + 1]
                b = palette_data[i * 3 + 2]
                palette.append((r, g, b))
        else:
            # Default grayscale palette
            palette = [(i, i, i) for i in range(256)]
        
        # Decode RLE compressed data
        pixel_data = bytearray()
        offset = 128  # Header size
        
        while offset < len(data) - 768:
            byte = data[offset]
            offset += 1
            
            if (byte & 0xC0) == 0xC0:  # RLE marker
                count = byte & 0x3F
                if offset < len(data) - 768:
                    value = data[offset]
                    offset += 1
                    pixel_data.extend([value] * count)
            else:
                pixel_data.append(byte)
        
        # Convert to RGBA
        rgba_data = bytearray(width * height * 4)
        
        for y in range(height):
            for x in range(width):
                idx = y * bytes_per_line + x
                if idx < len(pixel_data):
                    palette_idx = pixel_data[idx]
                    r, g, b = palette[palette_idx]
                    
                    pixel_pos = (y * width + x) * 4
                    rgba_data[pixel_pos] = r
                    rgba_data[pixel_pos + 1] = g
                    rgba_data[pixel_pos + 2] = b
                    rgba_data[pixel_pos + 3] = 255
        
        return {
            'width': width,
            'height': height,
            'rgba_data': bytes(rgba_data),
            'has_alpha': False,
            'format': 'RGB888',
            'original_format': 'PCX-8bit'
        }
        
    except Exception as e:
        print(f"PCX import error: {e}")
        return _load_with_pil(file_path)


def load_gif_8bit(file_path): #vers 1
    """Load GIF with transparency support"""
    try:
        img = Image.open(file_path)
        
        # Convert to RGBA if has transparency
        if img.mode == 'P':
            # Check for transparency
            if 'transparency' in img.info:
                img = img.convert('RGBA')
                has_alpha = True
            else:
                img = img.convert('RGB')
                has_alpha = False
        elif img.mode == 'RGBA':
            has_alpha = True
        else:
            img = img.convert('RGB')
            has_alpha = False
        
        width, height = img.size
        
        if has_alpha:
            rgba_data = img.tobytes('raw', 'RGBA')
        else:
            rgb_data = img.tobytes('raw', 'RGB')
            # Add alpha channel
            rgba_data = bytearray()
            for i in range(0, len(rgb_data), 3):
                rgba_data.extend(rgb_data[i:i+3])
                rgba_data.append(255)
            rgba_data = bytes(rgba_data)
        
        return {
            'width': width,
            'height': height,
            'rgba_data': rgba_data,
            'has_alpha': has_alpha,
            'format': 'ARGB8888' if has_alpha else 'RGB888',
            'original_format': 'GIF-8bit'
        }
        
    except Exception as e:
        print(f"GIF import error: {e}")
        return None


def load_png_8bit(file_path): #vers 1
    """Load PNG in 8-bit indexed mode"""
    try:
        img = Image.open(file_path)
        
        # Check if it's indexed
        if img.mode == 'P':
            # Check for transparency
            if 'transparency' in img.info:
                img = img.convert('RGBA')
                has_alpha = True
            else:
                img = img.convert('RGB')
                has_alpha = False
        elif img.mode == 'RGBA':
            has_alpha = True
        else:
            img = img.convert('RGB')
            has_alpha = False
        
        width, height = img.size
        
        if has_alpha:
            rgba_data = img.tobytes('raw', 'RGBA')
        else:
            rgb_data = img.tobytes('raw', 'RGB')
            rgba_data = bytearray()
            for i in range(0, len(rgb_data), 3):
                rgba_data.extend(rgb_data[i:i+3])
                rgba_data.append(255)
            rgba_data = bytes(rgba_data)
        
        return {
            'width': width,
            'height': height,
            'rgba_data': rgba_data,
            'has_alpha': has_alpha,
            'format': 'ARGB8888' if has_alpha else 'RGB888',
            'original_format': 'PNG-8bit'
        }
        
    except Exception as e:
        print(f"PNG import error: {e}")
        return None


def load_tga_8bit(file_path): #vers 1
    """Load 8-bit indexed TGA (Targa) file"""
    try:
        with open(file_path, 'rb') as f:
            # Read TGA header (18 bytes)
            header = f.read(18)
            
            image_type = header[2]
            color_map_type = header[1]
            
            # Type 1 = indexed color
            if image_type != 1 or color_map_type != 1:
                return _load_with_pil(file_path)
            
            # Get color map info
            color_map_start = struct.unpack('<H', header[3:5])[0]
            color_map_length = struct.unpack('<H', header[5:7])[0]
            color_map_depth = header[7]
            
            # Get image dimensions
            width = struct.unpack('<H', header[12:14])[0]
            height = struct.unpack('<H', header[14:16])[0]
            bit_depth = header[16]
            
            if bit_depth != 8:
                return _load_with_pil(file_path)
            
            # Read color map
            palette = []
            bytes_per_entry = color_map_depth // 8
            
            for i in range(color_map_length):
                entry = f.read(bytes_per_entry)
                if bytes_per_entry == 3:
                    b, g, r = entry
                    palette.append((r, g, b))
                elif bytes_per_entry == 4:
                    b, g, r, a = entry
                    palette.append((r, g, b, a))
            
            # Read pixel data
            pixel_data = f.read(width * height)
            
            # Convert to RGBA
            rgba_data = bytearray(width * height * 4)
            
            for y in range(height):
                for x in range(width):
                    idx = y * width + x
                    palette_idx = pixel_data[idx]
                    
                    if palette_idx < len(palette):
                        if len(palette[palette_idx]) == 4:
                            r, g, b, a = palette[palette_idx]
                        else:
                            r, g, b = palette[palette_idx]
                            a = 255
                        
                        pixel_pos = idx * 4
                        rgba_data[pixel_pos] = r
                        rgba_data[pixel_pos + 1] = g
                        rgba_data[pixel_pos + 2] = b
                        rgba_data[pixel_pos + 3] = a
            
            return {
                'width': width,
                'height': height,
                'rgba_data': bytes(rgba_data),
                'has_alpha': bytes_per_entry == 4,
                'format': 'ARGB8888' if bytes_per_entry == 4 else 'RGB888',
                'original_format': 'TGA-8bit'
            }
            
    except Exception as e:
        print(f"TGA import error: {e}")
        return _load_with_pil(file_path)


def _load_with_pil(file_path): #vers 1
    """Fallback loader using PIL for any format"""
    try:
        img = Image.open(file_path)
        
        # Convert to appropriate mode
        if img.mode in ('RGBA', 'LA'):
            has_alpha = True
            img = img.convert('RGBA')
        else:
            has_alpha = False
            img = img.convert('RGB')
        
        width, height = img.size
        
        if has_alpha:
            rgba_data = img.tobytes('raw', 'RGBA')
        else:
            rgb_data = img.tobytes('raw', 'RGB')
            rgba_data = bytearray()
            for i in range(0, len(rgb_data), 3):
                rgba_data.extend(rgb_data[i:i+3])
                rgba_data.append(255)
            rgba_data = bytes(rgba_data)
        
        return {
            'width': width,
            'height': height,
            'rgba_data': rgba_data,
            'has_alpha': has_alpha,
            'format': 'ARGB8888' if has_alpha else 'RGB888',
            'original_format': 'PIL-Generic'
        }
        
    except Exception as e:
        print(f"PIL fallback import error: {e}")
        return None


def is_indexed_format(file_path): #vers 1
    """Quick check if file is a supported indexed format"""
    ext = file_path.lower().split('.')[-1]
    return ext in ('bmp', 'pcx', 'gif', 'png', 'tga', 'targa', 'lbm', 'iff', 'ilbm')
