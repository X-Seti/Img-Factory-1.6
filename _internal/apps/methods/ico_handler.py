#!/usr/bin/env python3
#this belongs in apps/methods/ico_handler.py - Version: 1
# X-Seti - April26 2026 - IMG Factory 1.6
# Windows ICO / Apple ICNS reader/writer

"""
ICO and ICNS icon format handler for IMG Factory / DP5 Workshop.

ICO read:  all sizes/depths in the file, returns list of (w,h,rgba)
ICO write: single or multi-size ICO from list of (w,h,rgba)
ICNS read: all sizes, returns list of (w,h,rgba)
ICNS write: basic PNG-compressed ICNS
"""

import struct
from typing import List, Tuple, Optional

## Methods list -
# read_ico
# write_ico
# read_icns
# write_icns
# _ico_entry_to_rgba
# _rgba_to_ico_entry
# _icns_type_size

# =============================================================================
# ICO reader/writer
# =============================================================================

def read_ico(data: bytes) -> List[Tuple[int, int, bytes]]: #vers 1
    """
    Parse a Windows .ico file.
    Returns list of (width, height, rgba_bytes) for each image in the file.
    """
    if len(data) < 6:
        return []
    reserved, ico_type, count = struct.unpack_from('<HHH', data, 0)
    if ico_type not in (1, 2):  # 1=ICO, 2=CUR
        return []
    results = []
    for i in range(count):
        entry_off = 6 + i * 16
        if entry_off + 16 > len(data):
            break
        w, h, pal_count, _, planes, bpp, img_size, img_off = \
            struct.unpack_from('<BBBBHHII', data, entry_off)
        w = w or 256
        h = h or 256
        if img_off + img_size > len(data):
            continue
        img_data = data[img_off:img_off + img_size]
        try:
            rgba = _ico_entry_to_rgba(img_data, w, h)
            if rgba:
                results.append((w, h, rgba))
        except Exception:
            pass
    return results


def _ico_entry_to_rgba(data: bytes, w: int, h: int) -> Optional[bytes]: #vers 1
    """Convert a single ICO image entry to RGBA bytes."""
    # PNG-compressed (modern ICO)
    if data[:4] == b'\x89PNG':
        try:
            from PyQt6.QtGui import QImage
            from PyQt6.QtCore import QByteArray
            img = QImage()
            img.loadFromData(QByteArray(data), 'PNG')
            img = img.convertToFormat(QImage.Format.Format_RGBA8888)
            return bytes(img.bits().asarray(img.width() * img.height() * 4))
        except Exception:
            try:
                from PIL import Image
                import io
                img = Image.open(io.BytesIO(data)).convert('RGBA')
                return img.tobytes()
            except Exception:
                return None

    # BMP DIB header
    if len(data) < 40:
        return None
    hdr_size, img_w, img_h_dbl, planes, bpp = struct.unpack_from('<IIIHHH', data, 0)
    # ICO BMP height includes mask, so actual height = img_h_dbl // 2
    img_h = abs(img_h_dbl) // 2
    w = w or img_w
    h = h or img_h

    try:
        from PIL import Image
        import io
        # Reconstruct valid BMP by adding BITMAPFILEHEADER
        bmp_hdr = struct.pack('<HIHHI', 0x4D42, 14 + len(data), 0, 0, 14 + hdr_size)
        bmp_data = bmp_hdr + data
        img = Image.open(io.BytesIO(bmp_data)).convert('RGBA')
        img = img.crop((0, 0, w, h))
        return img.tobytes()
    except Exception:
        pass

    # Fallback: manual 32bpp parse
    if bpp == 32:
        px_off = hdr_size
        rgba = bytearray(w * h * 4)
        for y in range(h):
            src_y = h - 1 - y  # BMP is bottom-up
            for x in range(w):
                i = px_off + (src_y * w + x) * 4
                if i + 4 <= len(data):
                    b, g, r, a = data[i], data[i+1], data[i+2], data[i+3]
                    dst = (y * w + x) * 4
                    rgba[dst:dst+4] = [r, g, b, a]
        return bytes(rgba)
    return None


def _rgba_to_ico_entry(rgba: bytes, w: int, h: int,
                        use_png: bool = True) -> bytes: #vers 1
    """Convert RGBA bytes to a single ICO image entry (PNG or BMP)."""
    if use_png:
        try:
            from PyQt6.QtGui import QImage, QPixmap
            from PyQt6.QtCore import QByteArray, QBuffer, QIODeviceBase
            img = QImage(rgba, w, h, w * 4, QImage.Format.Format_RGBA8888)
            buf = QByteArray()
            qbuf = QBuffer(buf)
            qbuf.open(QIODeviceBase.OpenModeFlag.WriteOnly)
            img.save(qbuf, 'PNG')
            return bytes(buf)
        except Exception:
            pass
        try:
            from PIL import Image
            import io
            img = Image.frombytes('RGBA', (w, h), rgba)
            buf = io.BytesIO()
            img.save(buf, 'PNG')
            return buf.getvalue()
        except Exception:
            pass

    # Fallback: 32bpp BMP DIB
    row_bytes = w * 4
    # BMP is bottom-up
    bmp_pixels = bytearray()
    for y in range(h - 1, -1, -1):
        for x in range(w):
            i = (y * w + x) * 4
            r, g, b, a = rgba[i], rgba[i+1], rgba[i+2], rgba[i+3]
            bmp_pixels.extend([b, g, r, a])
    # AND mask (1bpp, bottom-up, row-padded to DWORD)
    mask_row = ((w + 31) // 32) * 4
    and_mask = bytearray(mask_row * h)
    for y in range(h):
        for x in range(w):
            i = (y * w + x) * 4
            if rgba[i+3] < 128:
                src_y = h - 1 - y
                and_mask[src_y * mask_row + x // 8] |= 0x80 >> (x % 8)
    dib_hdr = struct.pack('<IIIHHIIIIII',
        40, w, h * 2, 1, 32, 0,
        len(bmp_pixels) + len(and_mask), 0, 0, 0, 0)
    return bytes(dib_hdr) + bytes(bmp_pixels) + bytes(and_mask)


def write_ico(images: List[Tuple[int, int, bytes]],
              use_png: bool = True) -> bytes: #vers 1
    """
    Write a Windows .ico file from a list of (width, height, rgba_bytes).
    Sizes > 48px use PNG compression automatically.
    """
    entries = []
    for w, h, rgba in images:
        use = use_png or (w >= 64 or h >= 64)
        entries.append(_rgba_to_ico_entry(rgba, w, h, use_png=use))

    count = len(entries)
    dir_size = 6 + count * 16
    offsets = []
    offset = dir_size
    for e in entries:
        offsets.append(offset)
        offset += len(e)

    header = struct.pack('<HHH', 0, 1, count)
    directory = bytearray()
    for i, (w, h, rgba) in enumerate(images):
        pal_count = 0
        bpp = 32
        directory += struct.pack('<BBBBHHII',
            w if w < 256 else 0,
            h if h < 256 else 0,
            pal_count, 0, 1, bpp,
            len(entries[i]), offsets[i])

    return header + bytes(directory) + b''.join(entries)


# =============================================================================
# ICNS reader/writer
# =============================================================================

_ICNS_TYPES = {
    # type_code: (width, height, is_png_compressed)
    b'ic07': (128, 128, True),
    b'ic08': (256, 256, True),
    b'ic09': (512, 512, True),
    b'ic10': (1024,1024,True),
    b'ic11': (16,  16,  True),
    b'ic12': (32,  32,  True),
    b'ic13': (128, 128, True),
    b'ic14': (256, 256, True),
    b'il32': (32,  32,  False),
    b'is32': (16,  16,  False),
    b'ih32': (48,  48,  False),
    b'it32': (128, 128, False),
}


def _icns_type_size(type_code: bytes) -> Optional[Tuple]: #vers 1
    return _ICNS_TYPES.get(type_code)


def read_icns(data: bytes) -> List[Tuple[int, int, bytes]]: #vers 1
    """
    Parse an Apple .icns file.
    Returns list of (width, height, rgba_bytes).
    """
    if data[:4] != b'icns':
        return []
    results = []
    pos = 8  # skip 'icns' + file size
    while pos < len(data) - 8:
        type_code = data[pos:pos+4]
        chunk_size = struct.unpack_from('>I', data, pos+4)[0]
        chunk_data = data[pos+8:pos+chunk_size]
        info = _icns_type_size(type_code)
        if info:
            w, h, is_png = info
            try:
                if is_png and chunk_data[:4] == b'\x89PNG':
                    rgba = _decode_png_rgba(chunk_data)
                    if rgba:
                        results.append((w, h, rgba))
                elif not is_png:
                    # Raw 32bpp ARGB (big-endian)
                    rgba = _decode_icns_raw(chunk_data, w, h)
                    if rgba:
                        results.append((w, h, rgba))
            except Exception:
                pass
        pos += chunk_size
    return results


def _decode_png_rgba(data: bytes) -> Optional[bytes]: #vers 1
    try:
        from PyQt6.QtGui import QImage
        from PyQt6.QtCore import QByteArray
        img = QImage()
        img.loadFromData(QByteArray(data), 'PNG')
        img = img.convertToFormat(QImage.Format.Format_RGBA8888)
        return bytes(img.bits().asarray(img.width() * img.height() * 4))
    except Exception:
        try:
            from PIL import Image
            import io
            img = Image.open(io.BytesIO(data)).convert('RGBA')
            return img.tobytes()
        except Exception:
            return None


def _decode_icns_raw(data: bytes, w: int, h: int) -> Optional[bytes]: #vers 1
    """Decode raw ICNS ARGB data to RGBA."""
    expected = w * h * 4
    if len(data) < expected:
        return None
    rgba = bytearray(expected)
    for i in range(w * h):
        a = data[i*4]
        r = data[i*4+1]
        g = data[i*4+2]
        b = data[i*4+3]
        rgba[i*4:i*4+4] = [r, g, b, a]
    return bytes(rgba)


def write_icns(images: List[Tuple[int, int, bytes]]) -> bytes: #vers 1
    """
    Write a basic Apple .icns file from a list of (width, height, rgba_bytes).
    Uses PNG compression for all sizes.
    """
    SIZE_TO_TYPE = {
        16:   b'ic11', 32:  b'ic12', 64: b'ic13',
        128:  b'ic07', 256: b'ic08', 512: b'ic09', 1024: b'ic10',
    }
    chunks = bytearray()
    for w, h, rgba in images:
        type_code = SIZE_TO_TYPE.get(max(w, h))
        if not type_code:
            continue
        try:
            png_data = _encode_png(rgba, w, h)
            if not png_data:
                continue
            chunk_size = 8 + len(png_data)
            chunks += type_code + struct.pack('>I', chunk_size) + png_data
        except Exception:
            pass
    file_size = 8 + len(chunks)
    return b'icns' + struct.pack('>I', file_size) + bytes(chunks)


def _encode_png(rgba: bytes, w: int, h: int) -> Optional[bytes]: #vers 1
    try:
        from PyQt6.QtGui import QImage
        from PyQt6.QtCore import QByteArray, QBuffer, QIODeviceBase
        img = QImage(rgba, w, h, w * 4, QImage.Format.Format_RGBA8888)
        buf = QByteArray()
        qbuf = QBuffer(buf)
        qbuf.open(QIODeviceBase.OpenModeFlag.WriteOnly)
        img.save(qbuf, 'PNG')
        return bytes(buf)
    except Exception:
        try:
            from PIL import Image
            import io
            img = Image.frombytes('RGBA', (w, h), rgba)
            buf = io.BytesIO()
            img.save(buf, 'PNG')
            return buf.getvalue()
        except Exception:
            return None


__all__ = [
    'read_ico', 'write_ico',
    'read_icns', 'write_icns',
]
