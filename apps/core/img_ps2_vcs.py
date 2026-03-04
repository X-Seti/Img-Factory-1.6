#this belongs in core/ img_ps2_vcs.py - Version: 1
# X-Seti - March04 2026 - IMG Factory 1.6 - PS2 VCS IMG and LVZ Support
"""
PS2 VCS IMG and LVZ Support - Read-only parser for GTA Vice City Stories PS2 archives.
GTA3PS2.IMG: embedded directory, 32-byte entries, 512-byte sectors, type codes, no filenames.
LVZ: zlib-compressed DLRW streaming archive, 8-byte indexed entries, no filenames.
"""

import os
import struct
import zlib

##Methods list -
# detect_lvz
# detect_ps2_vcs
# open_lvz
# open_ps2_vcs

DLRW_MAGIC = b'DLRW'
PS2_SECTOR  = 512


def detect_ps2_vcs(path: str) -> bool: #vers 1
    """Return True if file looks like a GTA3PS2.IMG (type-code at byte 0, no VER2/GTA4 magic)."""
    try:
        if not path.lower().endswith('.img'):
            return False
        with open(path, 'rb') as f:
            header = f.read(8)
        if len(header) < 8:
            return False
        # Reject known PC/IV formats
        if header[:4] in (b'VER2',):
            return False
        import struct as _s
        if _s.unpack('<I', header[:4])[0] == 0xA94E2A52:
            return False
        # PS2 VCS: bytes 0-3 are a short ASCII type code (printable), bytes 4-7 are zero
        type_bytes = header[0:4]
        pad_bytes  = header[4:8]
        has_ascii  = all(0x20 <= b < 0x7F or b == 0x00 for b in type_bytes)
        has_null   = type_bytes.rstrip(b'\x00') != b'' and pad_bytes == b'\x00\x00\x00\x00'
        return has_ascii and has_null
    except Exception:
        return False


def detect_lvz(path: str) -> bool: #vers 1
    """Return True if file is a zlib-compressed DLRW LVZ archive."""
    try:
        with open(path, 'rb') as f:
            magic = f.read(2)
        return magic == b'\x78\xda' or magic == b'\x78\x9c' or magic == b'\x78\x01'
    except Exception:
        return False


def open_ps2_vcs(file_path: str) -> dict: #vers 1
    """
    Parse a GTA3PS2.IMG file.
    Returns dict: { 'version': 'PS2_VCS', 'entries': [...], 'error': None }
    Each entry: { 'name': str, 'type_code': str, 'offset': int, 'size': int, 'index': int }
    """
    result = {'version': 'PS2_VCS', 'entries': [], 'error': None}
    try:
        file_size = os.path.getsize(file_path)
        with open(file_path, 'rb') as f:
            # Read first 64 bytes - scan for valid entries before padding (0x88 fill)
            header_block = f.read(256)

        entries = []
        idx = 0
        offset = 0
        while offset + 32 <= len(header_block):
            chunk = header_block[offset:offset + 32]
            # Stop at padding (0x88 or 0x00 fill with no valid type)
            if chunk[0] == 0x88 or (chunk[0] == 0x00 and chunk[8] == 0x00 and chunk[12] == 0x00):
                break
            type_bytes = chunk[0:8].rstrip(b'\x00')
            # Type must be printable ASCII
            if not type_bytes or not all(0x20 <= b < 0x7F for b in type_bytes):
                break
            type_code  = type_bytes.decode('ascii', errors='replace')
            entry_off  = struct.unpack('<I', chunk[8:12])[0] * PS2_SECTOR
            entry_size = struct.unpack('<I', chunk[12:16])[0] * PS2_SECTOR
            if entry_off >= file_size:
                break
            entries.append({
                'name':      f'{type_code}_{idx}',
                'type_code': type_code,
                'offset':    entry_off,
                'size':      entry_size,
                'index':     idx,
            })
            idx    += 1
            offset += 32

        result['entries'] = entries
    except Exception as e:
        result['error'] = str(e)
    return result


def open_lvz(file_path: str) -> dict: #vers 1
    """
    Decompress and parse a VCS .LVZ archive.
    Returns dict: { 'version': 'PS2_LVZ', 'entries': [...], 'error': None, 'data': bytes }
    Each entry: { 'name': str, 'offset': int, 'size': int, 'index': int }
    offset/size are byte offsets into the decompressed data blob.
    """
    result = {'version': 'PS2_LVZ', 'entries': [], 'error': None, 'data': None}
    try:
        with open(file_path, 'rb') as f:
            raw = f.read()
        data = zlib.decompress(raw)
        result['data'] = data

        if len(data) < 32:
            result['error'] = 'LVZ too small after decompress'
            return result

        magic = data[0:4]
        if magic != DLRW_MAGIC:
            result['error'] = f'Bad DLRW magic: {magic!r}'
            return result

        entry_count = struct.unpack('<I', data[0x14:0x18])[0]

        # Entries start at 0x28: byte_offset[4LE] size_sectors[4LE]
        entries = []
        entry_base = 0x28
        for i in range(entry_count):
            pos = entry_base + i * 8
            if pos + 8 > len(data):
                break
            off = struct.unpack('<I', data[pos:pos + 4])[0]
            sz  = struct.unpack('<I', data[pos + 4:pos + 8])[0] * PS2_SECTOR
            entries.append({
                'name':   f'cell_{i}',
                'offset': off,
                'size':   sz,
                'index':  i,
            })

        result['entries'] = entries
    except zlib.error as e:
        result['error'] = f'zlib decompress failed: {e}'
    except Exception as e:
        result['error'] = str(e)
    return result
