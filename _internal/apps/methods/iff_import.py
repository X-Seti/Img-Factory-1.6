# this belongs in methods/ iff_import.py - Version: 1
# X-Seti - October09 2025 - IMG Factory 1.5 - IFF Import Handler
"""
Amiga IFF file format import support for 8-bit indexed textures.
Handles ILBM (Interleaved BitMap) format.
"""

##Methods list -
# load_iff_image
# _parse_iff_chunks
# _decode_ilbm_body
# _expand_palette

import struct


def load_iff_image(file_path): #vers 1
    """
    Load Amiga IFF/ILBM image file and convert to RGBA
    
    Returns:
        dict: {
            'width': int,
            'height': int,
            'rgba_data': bytes,  # RGBA format
            'has_alpha': bool,
            'format': str,
            'original_format': 'IFF'
        }
    """
    try:
        with open(file_path, 'rb') as f:
            data = f.read()
        
        # Verify IFF header
        if data[:4] != b'FORM':
            return None
        
        # Get form type
        form_type = data[8:12]
        if form_type not in (b'ILBM', b'PBM ', b'ACBM'):
            return None
        
        # Parse IFF chunks
        chunks = _parse_iff_chunks(data[12:])
        
        if 'BMHD' not in chunks:
            return None
        
        # Parse bitmap header
        bmhd = chunks['BMHD']
        width = struct.unpack('>H', bmhd[0:2])[0]
        height = struct.unpack('>H', bmhd[2:4])[0]
        depth = bmhd[8]  # Bit planes
        compression = bmhd[10]
        
        # Get palette if exists
        palette = None
        if 'CMAP' in chunks:
            palette = _expand_palette(chunks['CMAP'], depth)
        
        # Decode body data
        if 'BODY' not in chunks:
            return None
        
        body_data = chunks['BODY']
        
        # Decompress if needed
        if compression == 1:  # ByteRun1 compression
            body_data = _decode_byterun1(body_data)
        
        # Convert planar to chunky format
        rgba_data = _convert_ilbm_to_rgba(
            body_data, width, height, depth, palette
        )
        
        return {
            'width': width,
            'height': height,
            'rgba_data': rgba_data,
            'has_alpha': False,  # IFF typically doesn't have alpha
            'format': 'RGB888',
            'original_format': 'IFF',
            'bit_depth': depth
        }
        
    except Exception as e:
        print(f"IFF import error: {e}")
        return None


def _parse_iff_chunks(data): #vers 1
    """Parse IFF file chunks into dictionary"""
    chunks = {}
    offset = 0
    
    while offset < len(data) - 8:
        # Read chunk header
        chunk_id = data[offset:offset+4]
        chunk_size = struct.unpack('>I', data[offset+4:offset+8])[0]
        
        # Read chunk data
        chunk_data = data[offset+8:offset+8+chunk_size]
        chunks[chunk_id.decode('ascii', errors='ignore')] = chunk_data
        
        # Move to next chunk (align to word boundary)
        offset += 8 + chunk_size
        if chunk_size % 2:
            offset += 1
    
    return chunks


def _decode_byterun1(data): #vers 1
    """Decode ByteRun1 RLE compression used in IFF files"""
    result = bytearray()
    i = 0
    
    while i < len(data):
        n = data[i]
        i += 1
        
        if n < 128:
            # Copy next n+1 bytes literally
            count = n + 1
            result.extend(data[i:i+count])
            i += count
        elif n > 128:
            # Repeat next byte (257-n) times
            count = 257 - n
            if i < len(data):
                result.extend([data[i]] * count)
                i += 1
        # n == 128 is a no-op
    
    return bytes(result)


def _expand_palette(cmap_data, depth): #vers 1
    """Expand IFF palette to full RGB values"""
    num_colors = 2 ** depth
    palette = []
    
    for i in range(min(num_colors, len(cmap_data) // 3)):
        r = cmap_data[i * 3]
        g = cmap_data[i * 3 + 1]
        b = cmap_data[i * 3 + 2]
        palette.append((r, g, b))
    
    # Fill remaining colors if needed
    while len(palette) < num_colors:
        palette.append((0, 0, 0))
    
    return palette


def _convert_ilbm_to_rgba(body_data, width, height, depth, palette): #vers 1
    """Convert ILBM planar format to RGBA"""
    # Calculate bytes per row (word-aligned)
    bytes_per_row = ((width + 15) // 16) * 2
    
    # Create output buffer
    rgba_data = bytearray(width * height * 4)
    
    # If no palette, create grayscale
    if not palette:
        palette = [(i * 255 // (2**depth - 1),) * 3 for i in range(2**depth)]
    
    # Decode planar data
    for y in range(height):
        for x in range(width):
            # Get pixel index from bit planes
            pixel_index = 0
            
            for plane in range(depth):
                # Calculate position in body data
                plane_offset = plane * bytes_per_row
                row_offset = y * bytes_per_row * depth
                byte_offset = row_offset + plane_offset + (x // 8)
                bit_offset = 7 - (x % 8)
                
                if byte_offset < len(body_data):
                    bit = (body_data[byte_offset] >> bit_offset) & 1
                    pixel_index |= (bit << plane)
            
            # Get color from palette
            if pixel_index < len(palette):
                r, g, b = palette[pixel_index]
                
                # Write RGBA
                pixel_offset = (y * width + x) * 4
                rgba_data[pixel_offset] = r
                rgba_data[pixel_offset + 1] = g
                rgba_data[pixel_offset + 2] = b
                rgba_data[pixel_offset + 3] = 255  # Full alpha
    
    return bytes(rgba_data)


def is_iff_file(file_path): #vers 1
    """Quick check if file is IFF format"""
    try:
        with open(file_path, 'rb') as f:
            header = f.read(12)
        
        if len(header) < 12:
            return False
        
        # Check for FORM header and valid type
        if header[:4] == b'FORM':
            form_type = header[8:12]
            return form_type in (b'ILBM', b'PBM ', b'ACBM')
        
        return False
    except:
        return False
