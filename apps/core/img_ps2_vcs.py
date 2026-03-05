#this belongs in core/ img_ps2_vcs.py - Version: 5
# X-Seti - March04 2026 - IMG Factory 1.6 - PS2/PSP/Bully IMG, LVZ, ANPK and HXD Support
"""
PS2/PSP/Bully IMG, LVZ, ANPK, Bully and HXD Support - Read-only parsers.
GTA3PS2.IMG (LCS/VCS): embedded directory, 32-byte entries, 512-byte sectors, type codes.
GTA3_N.IMG (GTA3/VC/Bully PS2, iOS, Android): 12-byte entries, 512-byte sectors, no names.
LVZ: zlib-compressed DLRW streaming archive, 8-byte indexed entries, no filenames.
ANPK: PSP animation package, chunk-based, named animation clips (DGAN blocks).
BULLY: Bully PS2 named-entry archive, 64-byte name-only directory, sequential HXD data.
HXD/MXD/AGR: Bully bone/animation data, float header + internal path, single-entry.
"""

import os
import struct
import zlib

##Methods list -
# detect_anpk
# detect_bully
# detect_hxd
# detect_lvz
# detect_ps2_v1
# detect_ps2_vcs
# open_anpk
# open_bully
# open_hxd
# open_lvz
# open_ps2_v1
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


def detect_ps2_v1(path: str) -> bool: #vers 1
    """
    Return True if file is a GTA3/VC/Bully PS2 (or iOS/Android port) IMG.
    Format: 12-byte entries from byte 0, no magic, no names.
    First u32 LE = entry count (small number, typically < 5000).
    Third u32 LE = first entry size in 512-byte sectors (small, typically < 10000).
    """
    try:
        if not path.lower().endswith('.img'):
            return False
        with open(path, 'rb') as f:
            header = f.read(16)
        if len(header) < 16:
            return False
        # Reject known magic formats
        if header[:4] in (b'VER2',):
            return False
        import struct as _s
        if _s.unpack('<I', header[:4])[0] == 0xA94E2A52:
            return False
        # Reject PS2 VCS type-code format (printable ASCII + null pad)
        type_bytes = header[0:4]
        if all(0x20 <= b < 0x7F or b == 0x00 for b in type_bytes) and \
           type_bytes.rstrip(b'\x00') != b'' and header[4:8] == b'\x00\x00\x00\x00':
            return False
        # PS2 V1: first u32 = entry count (< 5000), third u32 = sector size (< 10000)
        count   = _s.unpack('<I', header[0:4])[0]
        sec_sz  = _s.unpack('<I', header[8:12])[0]
        return 0 < count < 5000 and 0 < sec_sz < 10000
    except Exception:
        return False


def open_ps2_v1(file_path: str) -> dict: #vers 1
    """
    Parse a GTA3/VC/Bully PS2 (or iOS/Android) IMG file.
    Format: 12-byte entries from byte 0 - sector_offset[4] asset_id[4] sector_size[4].
    Returns dict: { 'version': 'PS2_V1', 'entries': [...], 'error': None }
    Each entry: { 'name': str, 'offset': int, 'size': int, 'asset_id': int, 'index': int }
    """
    result = {'version': 'PS2_V1', 'entries': [], 'error': None}
    try:
        file_size = os.path.getsize(file_path)
        with open(file_path, 'rb') as f:
            raw = f.read(12)
        if len(raw) < 12:
            result['error'] = 'File too small'
            return result

        entry_count = struct.unpack('<I', raw[0:4])[0]
        dir_size    = entry_count * 12

        with open(file_path, 'rb') as f:
            dir_data = f.read(dir_size)

        entries = []
        for i in range(entry_count):
            off = i * 12
            if off + 12 > len(dir_data):
                break
            sec_off  = struct.unpack('<I', dir_data[off:off+4])[0]
            asset_id = struct.unpack('<I', dir_data[off+4:off+8])[0]
            sec_sz   = struct.unpack('<I', dir_data[off+8:off+12])[0]
            byte_off = sec_off * PS2_SECTOR
            byte_sz  = sec_sz  * PS2_SECTOR
            if byte_off >= file_size:
                continue
            entries.append({
                'name':     f'entry_{i}',
                'offset':   byte_off,
                'size':     byte_sz,
                'asset_id': asset_id,
                'index':    i,
            })

        result['entries'] = entries
    except Exception as e:
        result['error'] = str(e)
    return result


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


ANPK_MAGIC = b'ANPK'


def detect_anpk(path: str) -> bool: #vers 1
    """Return True if file is a PSP ANPK animation package."""
    try:
        with open(path, 'rb') as f:
            return f.read(4) == ANPK_MAGIC
    except Exception:
        return False


def open_anpk(file_path: str) -> dict: #vers 1
    """
    Parse a PSP ANPK animation package.
    Returns dict: { 'version': 'ANPK', 'entries': [...], 'error': None }
    Each entry: { 'name': str, 'offset': int, 'size': int, 'index': int }
    One entry per DGAN animation block.
    """
    result = {'version': 'ANPK', 'entries': [], 'error': None}
    try:
        with open(file_path, 'rb') as f:
            data = f.read()

        if data[0:4] != ANPK_MAGIC:
            result['error'] = 'Not an ANPK file'
            return result

        # Collect clip names from top-level INFO and NAME chunks
        names = []
        pos = 8
        dgan_offsets = []

        while pos + 8 <= len(data):
            tag  = data[pos:pos+4]
            size = struct.unpack('<I', data[pos+4:pos+8])[0]
            cdata = data[pos+8:pos+8+size] if pos+8+size <= len(data) else b''

            if tag == b'INFO' and len(cdata) >= 4:
                # count(4) + first_name (null-terminated, padded)
                name_bytes = cdata[4:].split(b'\x00')[0]
                if name_bytes:
                    names.append(name_bytes.decode('ascii', errors='replace'))

            elif tag == b'NAME' and cdata:
                name_bytes = cdata.split(b'\x00')[0]
                if name_bytes:
                    names.append(name_bytes.decode('ascii', errors='replace'))

            elif tag == b'DGAN':
                dgan_offsets.append((pos, size))
                # Skip into DGAN without descending (treat as opaque blob)

            pos += 8 + size
            # 4-byte alignment
            remainder = (8 + size) % 4
            if remainder:
                pos += 4 - remainder

        entries = []
        for i, (dgan_pos, dgan_size) in enumerate(dgan_offsets):
            name = names[i] if i < len(names) else f'anim_{i}'
            entries.append({
                'name':   name,
                'offset': dgan_pos,
                'size':   dgan_size + 8,  # include tag+size header
                'index':  i,
            })

        result['entries'] = entries
    except Exception as e:
        result['error'] = str(e)
    return result


BULLY_ENTRY_SIZE = 64


def detect_bully(path: str) -> bool: #vers 1
    """
    Return True if file is a Bully PS2 named-entry IMG (CUTS.IMG style).
    Format: count[4LE] then count*64-byte name-only entries (no offsets stored).
    Detection: small count, first entry bytes are printable ASCII, no known magic.
    """
    try:
        if not path.lower().endswith('.img'):
            return False
        with open(path, 'rb') as f:
            header = f.read(68)
        if len(header) < 68:
            return False
        # Reject known formats
        if header[:4] in (b'VER2', b'ANPK'):
            return False
        import struct as _s
        if _s.unpack('<I', header[:4])[0] == 0xA94E2A52:
            return False
        # Reject LCS/VCS type-code (printable + null pad at [4:8])
        if all(0x20 <= b < 0x7F or b == 0x00 for b in header[0:4]) and \
           header[0:4].rstrip(b'\x00') != b'' and header[4:8] == b'\x00\x00\x00\x00':
            return False
        count = _s.unpack('<I', header[0:4])[0]
        if not (0 < count < 1000):
            return False
        # First entry at offset 4 must start with printable ASCII name
        name_start = header[4:20]
        printable = sum(1 for b in name_start if 0x20 <= b < 0x7F or b == 0x00)
        return printable >= 4 and name_start[0:1] != b'\x00'
    except Exception:
        return False


def open_bully(file_path: str) -> dict: #vers 1
    """
    Parse a Bully PS2 CUTS.IMG style archive.
    Format: count[4] + count*64-byte name entries (name[64], no offsets).
    Data follows directory as sequential HXD blocks (self-describing).
    Returns dict: { 'version': 'BULLY', 'entries': [...], 'error': None }
    Each entry: { 'name': str, 'offset': int, 'size': int, 'index': int }
    offset/size are 0 - sequential HXD blocks require separate scanning.
    """
    result = {'version': 'BULLY', 'entries': [], 'error': None}
    try:
        with open(file_path, 'rb') as f:
            count_data = f.read(4)
            if len(count_data) < 4:
                result['error'] = 'File too small'
                return result
            count = struct.unpack('<I', count_data)[0]
            dir_data = f.read(count * BULLY_ENTRY_SIZE)

        entries = []
        for i in range(count):
            off = i * BULLY_ENTRY_SIZE
            if off + BULLY_ENTRY_SIZE > len(dir_data):
                break
            raw_name = dir_data[off:off + BULLY_ENTRY_SIZE].split(b'\x00')[0]
            name = raw_name.decode('ascii', errors='replace') if raw_name else f'entry_{i}'
            entries.append({
                'name':   name,
                'offset': 0,
                'size':   0,
                'index':  i,
            })

        result['entries'] = entries
    except Exception as e:
        result['error'] = str(e)
    return result


HXD_EXTENSIONS = {'.hxd', '.mxd', '.agr'}
HXD_NAME_OFFSET = 0x0c
HXD_NAME_LENGTH = 32


def detect_hxd(path: str) -> bool: #vers 1
    """
    Return True if file is a Bully HXD/MXD/AGR animation/bone data file.
    Format: float_header[12] + internal_path[32] (null-terminated ASCII).
    Identified by extension only - no magic bytes.
    """
    try:
        ext = os.path.splitext(path)[1].lower()
        if ext not in HXD_EXTENSIONS:
            return False
        if os.path.getsize(path) < HXD_NAME_OFFSET + 4:
            return False
        with open(path, 'rb') as f:
            f.seek(HXD_NAME_OFFSET)
            name_bytes = f.read(HXD_NAME_LENGTH)
        # Must have at least one printable ASCII char at name offset
        printable = sum(1 for b in name_bytes if 0x20 <= b < 0x7F)
        return printable >= 2
    except Exception:
        return False


def open_hxd(file_path: str) -> dict: #vers 1
    """
    Parse a Bully HXD/MXD/AGR animation/bone data file.
    Single-entry format: internal path at offset 0x0c identifies the asset.
    Returns dict: { 'version': 'HXD', 'entries': [...], 'error': None }
    Single entry: { 'name': str, 'offset': int, 'size': int, 'index': 0 }
    """
    result = {'version': 'HXD', 'entries': [], 'error': None}
    try:
        file_size = os.path.getsize(file_path)
        with open(file_path, 'rb') as f:
            f.seek(HXD_NAME_OFFSET)
            name_bytes = f.read(HXD_NAME_LENGTH)
        internal_name = name_bytes.split(b'\x00')[0].decode('ascii', errors='replace')
        if not internal_name:
            internal_name = os.path.splitext(os.path.basename(file_path))[0]
        # Normalise Windows-style path to just the final component
        display_name = internal_name.replace('\\', '/').split('/')[-1]
        result['entries'] = [{
            'name':   display_name,
            'offset': 0,
            'size':   file_size,
            'index':  0,
        }]
    except Exception as e:
        result['error'] = str(e)
    return result
