#this belongs in core/ img_ps2_vcs.py - Version: 10
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


def detect_ps2_v1(path: str) -> bool: #vers 3
    """
    Return True if file is a GTA3/VC PS2 (or iOS/Android port) IMG.
    Format: 12-byte entries from byte 0, no magic, no names, no count header.
      [0:4]  sector_offset  (first entry - a small number)
      [4:8]  asset_id       (type encoded in high 16 bits)
      [8:12] sector_size    (also small)
    The directory size = first_entry.sector_offset * 512 bytes.
    """
    try:
        if not path.lower().endswith('.img'):
            return False
        file_size = os.path.getsize(path)
        if file_size < 36:
            return False
        with open(path, 'rb') as f:
            header = f.read(36)
        # Reject known magic formats
        if header[:4] in (b'VER2', b'DLRW', b'ANPK'):
            return False
        import struct as _s
        # Reject GTA IV
        if _s.unpack('<I', header[:4])[0] == 0xA94E2A52:
            return False
        # Reject PS2 VCS type-code (printable ASCII + null pad at bytes 4-8)
        if all(0x20 <= b < 0x7F or b == 0x00 for b in header[0:4]) and \
           header[0:4].rstrip(b'\x00') != b'' and header[4:8] == b'\x00\x00\x00\x00':
            return False
        # Reject RW chunk types (TXD=0x16, Clump=0x10, etc.)
        first_u32 = _s.unpack('<I', header[:4])[0]
        if first_u32 in (0x1, 0x2, 0x3):  # only reject values that are NEVER valid sector offsets
            return False
        # Entry 0: sec_off[0:4], asset_id[4:8], sec_sz[8:12]
        sec_off0 = _s.unpack('<I', header[0:4])[0]
        asset_id0 = _s.unpack('<I', header[4:8])[0]
        sec_sz0  = _s.unpack('<I', header[8:12])[0]
        # Entry 1
        sec_off1 = _s.unpack('<I', header[12:16])[0]
        sec_sz1  = _s.unpack('<I', header[20:24])[0]
        # sec_off must be small (directory fits in first N sectors)
        if not (1 <= sec_off0 < 5000):
            return False
        # sec_sz must be a plausible sector count
        if not (0 < sec_sz0 < 65536 and 0 < sec_sz1 < 65536):
            return False
        # asset_id high byte must NOT be printable ASCII (that would be Bully name field)
        asset_hi = (asset_id0 >> 24) & 0xFF
        if 0x20 <= asset_hi < 0x7F:
            return False
        # Second entry offset >= first (sequential)
        if sec_off1 < sec_off0:
            return False
        # Directory size = sec_off0 * PS2_SECTOR
        # entry_count = dir_size // 12 (12-byte entries, sector-padded)
        dir_bytes = sec_off0 * PS2_SECTOR
        entry_count = dir_bytes // 12
        if not (1 <= entry_count < 50000):
            return False
        # Spot-check: offset must be inside file
        if sec_off0 * PS2_SECTOR >= file_size:
            return False
        return True
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

        # No count header - header[0:4] is first entry's sector_offset
        # Directory size = first_entry.sec_off * 512 bytes
        first_sec_off = struct.unpack('<I', raw[0:4])[0]
        dir_size = first_sec_off * PS2_SECTOR
        entry_count = dir_size // 12

        with open(file_path, 'rb') as f:
            dir_data = f.read(dir_size)
        # Entries start at byte 0

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
            # Skip null/padding entries
            if sec_off == 0 and sec_sz == 0:
                continue
            if byte_off >= file_size:
                continue
            # Decode asset_id: high 16 bits = type, low 16 bits = sequential index
            _type_map = {
                0x0001: 'txd', 0x0002: 'dff', 0x0003: 'col',
                0x0004: 'ipl', 0x0005: 'dat', 0x0006: 'wtd',
                0x0007: 'scr', 0x0008: 'scm', 0x0009: 'wav',
            }
            _aid_type  = (asset_id >> 16) & 0xFFFF
            _aid_idx   = asset_id & 0xFFFF
            _ext       = _type_map.get(_aid_type, 'bin')
            _name      = f'{_ext}_{_aid_idx:04d}' if _aid_type else f'entry_{i}'
            entries.append({
                'name':     _name,
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
        return magic[0] == 0x78 and magic[1] in (0xDA, 0x9C, 0x01)
    except Exception:
        return False


def open_ps2_vcs(file_path: str) -> dict: #vers 2
    """
    Parse a GTA3PS2.IMG (LCS/VCS PS2) file.
    Format: embedded directory of 32-byte entries, 512-byte sectors.
      [0:8]  type_code  — ASCII, right-padded with 0x00
      [8:12] offset     — sector offset LE
      [12:16]size       — sector count LE
      [16:32]padding    — 0x00 or 0x88 fill
    Directory ends at first fully-zero or 0x88-filled chunk.
    Returns dict: { 'version': 'PS2_VCS', 'entries': [...], 'error': None }
    """
    result = {'version': 'PS2_VCS', 'entries': [], 'error': None}
    try:
        file_size = os.path.getsize(file_path)
        # Find directory end: scan up to the first data sector offset
        # The first entry's offset tells us where data starts, so the
        # directory occupies bytes 0..(first_offset*512 - 1).
        # Read up to 64 KB to cover any realistic directory.
        scan_limit = min(file_size, 65536)
        with open(file_path, 'rb') as f:
            scan_buf = f.read(scan_limit)

        entries = []
        idx = 0
        pos = 0
        while pos + 32 <= len(scan_buf):
            chunk = scan_buf[pos:pos + 32]
            # Stop at padding sentinel (0x88 fill or all-zero with zero offset)
            if chunk[0] == 0x88:
                break
            if chunk[:16] == b'\x00' * 16:
                break
            type_bytes = chunk[0:8].rstrip(b'\x00')
            if not type_bytes or not all(0x20 <= b < 0x7F for b in type_bytes):
                break
            type_code  = type_bytes.decode('ascii', errors='replace')
            entry_off  = struct.unpack('<I', chunk[8:12])[0] * PS2_SECTOR
            entry_size = struct.unpack('<I', chunk[12:16])[0] * PS2_SECTOR
            # Sanity: offset must be inside the file
            if entry_off == 0 and entry_size == 0:
                break
            if entry_off >= file_size:
                break
            # First valid entry tells us where the directory ends
            # once we've passed it, we should stop scanning
            if entries and entry_off < entries[0]['offset']:
                break
            entries.append({
                'name':      f'{type_code}_{idx}',
                'type_code': type_code,
                'offset':    entry_off,
                'size':      min(entry_size, file_size - entry_off),
                'index':     idx,
            })
            idx += 1
            pos += 32

        result['entries'] = entries
    except Exception as e:
        result['error'] = str(e)
    return result


def open_lvz(file_path: str) -> dict: #vers 2
    """
    Decompress and parse a VCS .LVZ (DLRW) streaming archive.

    DLRW binary layout (all LE):
      [0x00]  "DLRW" magic
      [0x04]  0 (reserved)
      [0x08]  total decompressed size
      [0x0C]  index table offset (= end of entry header area)
      [0x10]  index table offset (duplicate)
      [0x14]  entry_count  (number of streaming cells)
      [0x18]  0 (padding)
      [0x1C]  0 (padding)
      [0x20]  header record: data_start_ptr(4) + nested_dlrw_start_ptr(4)
      [0x28 + i*8]  cell i: sub_entry_count(4) + nested_dlrw_offset(4)
      [index_table]  entry_count × 4-byte back-pointers (for lookup)

    Each cell contains a nested DLRW block of audio/visual stream chunks.
    """
    result = {'version': 'PS2_LVZ', 'entries': [], 'error': None, 'data': None}
    try:
        with open(file_path, 'rb') as f:
            raw = f.read()

        # Decompress - try standard zlib, fall back to raw deflate
        try:
            data = zlib.decompress(raw)
        except zlib.error:
            data = zlib.decompress(raw, -15)
        result['data'] = data

        if len(data) < 0x28:
            result['error'] = 'LVZ too small after decompress'
            return result

        magic = data[0:4]
        if magic != DLRW_MAGIC:
            result['error'] = f'Bad DLRW magic: {magic!r}'
            return result

        # Parse header
        total_size   = struct.unpack_from('<I', data, 0x08)[0]
        index_offset = struct.unpack_from('<I', data, 0x0C)[0]
        entry_count  = struct.unpack_from('<I', data, 0x14)[0]

        # Header record at 0x20: pointers to data area and nested DLRW area
        data_area_start  = struct.unpack_from('<I', data, 0x20)[0]
        nested_area_start = struct.unpack_from('<I', data, 0x24)[0]

        # Cell records at 0x28, 8 bytes each:
        #   unknown(4) + nested_dlrw_byte_offset(4)
        # Each nested DLRW at nested_off contains:
        #   [0x04] magic check ('DLRW')
        #   [0x0C] = data size in companion .IMG
        #   [0x18] = byte offset into companion .IMG
        #
        # entry_count is the TOTAL world grid slot count (padded with 0xAAAAAAAA).
        # We scan until we hit a non-DLRW entry and only keep cells with size > 0.
        entries = []
        scan_limit = min(len(data), index_offset)
        cell_idx = 0
        stream_idx = 0
        for i in range(0, (scan_limit - 0x28) // 8):
            pos = 0x28 + i * 8
            if pos + 8 > scan_limit:
                break
            nested_off = struct.unpack_from('<I', data, pos + 4)[0]

            # Stop at uninitialized sentinel (0xAAAAAAAA or clearly out of range)
            if nested_off > len(data) - 4:
                break
            if nested_off == 0:
                continue

            # Validate nested DLRW magic
            if data[nested_off:nested_off + 4] != DLRW_MAGIC:
                break

            img_size   = struct.unpack_from('<I', data, nested_off + 0x0C)[0]
            img_offset = struct.unpack_from('<I', data, nested_off + 0x18)[0]

            # Skip empty slots
            if img_size == 0:
                continue

            entries.append({
                'name':       f'stream_{stream_idx:04d}.strm',
                'offset':     img_offset,
                'size':       img_size,
                'lvz_offset': nested_off,
                'index':      stream_idx,
            })
            stream_idx += 1

        result['entries'] = entries
        result['data_area_start']   = data_area_start
        result['nested_area_start'] = nested_area_start

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


def detect_bully(path: str) -> bool: #vers 2
    """
    Return True if file is a Bully PS2 named-entry IMG (CUTS.IMG style).
    Format: count[4LE] + count*64-byte ASCII name entries (no offsets stored).
    Key distinguisher vs PS2_V1: the bytes immediately after the count MUST be
    valid ASCII text (a filename), NOT a sector_offset integer.
    """
    try:
        if not path.lower().endswith('.img'):
            return False
        with open(path, 'rb') as f:
            header = f.read(72)
        if len(header) < 72:
            return False
        # Reject known magic formats
        if header[:4] in (b'VER2', b'ANPK', b'DLRW'):
            return False
        import struct as _s
        if _s.unpack('<I', header[:4])[0] == 0xA94E2A52:
            return False
        # Reject PS2 VCS type-code
        if all(0x20 <= b < 0x7F or b == 0x00 for b in header[0:4]) and \
           header[0:4].rstrip(b'\x00') != b'' and header[4:8] == b'\x00\x00\x00\x00':
            return False
        count = _s.unpack('<I', header[0:4])[0]
        if not (1 <= count < 500):
            return False
        # First name entry starts at byte 4 and must be printable ASCII
        # Require at least 4 consecutive printable chars from the very start
        first_name = header[4:68]
        # First byte must be printable (A-Z a-z 0-9 _ etc.) - NOT a null or control char
        if first_name[0] < 0x20 or first_name[0] >= 0x7F:
            return False
        # Count consecutive printable chars at start of name
        consecutive = 0
        for b in first_name:
            if 0x20 <= b < 0x7F:
                consecutive += 1
            else:
                break
        if consecutive < 4:
            return False
        # The name must end with a null terminator within 64 bytes
        if b'\x00' not in first_name:
            return False
        return True
    except Exception:
        return False


def open_bully(file_path: str) -> dict: #vers 2
    """
    Parse a Bully PS2 CUTS.IMG style archive.

    Format: count[4LE] + count*64-byte name-only entries.
    Data follows the directory as self-describing HXD blocks.
    offsets/sizes are not stored — entries are listed by name only.

    Note: Bully WORLD.IMG / GTA3.IMG are standard V1 DIR+IMG pairs and
    should be opened via _open_version_1, not this function.  This function
    handles CUTS.IMG and similar name-directory archives.
    """
    result = {'version': 'BULLY', 'entries': [], 'error': None}
    try:
        file_size = os.path.getsize(file_path)
        with open(file_path, 'rb') as f:
            count_raw = f.read(4)
            if len(count_raw) < 4:
                result['error'] = 'File too small'
                return result
            count = struct.unpack('<I', count_raw)[0]
            if count == 0 or count > 10000:
                result['error'] = f'Implausible entry count: {count}'
                return result
            dir_data = f.read(count * BULLY_ENTRY_SIZE)

        if len(dir_data) < count * BULLY_ENTRY_SIZE:
            result['error'] = 'Directory truncated'
            return result

        # Data block starts after the directory
        data_start = 4 + count * BULLY_ENTRY_SIZE

        entries = []
        for i in range(count):
            off = i * BULLY_ENTRY_SIZE
            raw_name = dir_data[off:off + BULLY_ENTRY_SIZE].split(b'\x00')[0]
            name = raw_name.decode('ascii', errors='replace') if raw_name else f'entry_{i}'
            entries.append({
                'name':   name,
                'offset': data_start,   # approximate — HXD blocks follow sequentially
                'size':   0,            # unknown until HXD block scanned
                'index':  i,
            })

        result['entries'] = entries
        result['note'] = ('Name-only directory — offsets are approximate. '
                          'Extract individual HXD blocks to get true sizes.')
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


# ---------------------------------------------------------------------------
# LCS Android detection helper
# ---------------------------------------------------------------------------

def detect_lcs_android(path: str) -> bool:  # vers 1
    """Return True if file is an LCS Android IMG (VER2 header, TXD 0x1005FFFF).

    Primary indicator: VER2 magic AND filename contains 'lcs' or 'liberty'.
    Secondary: scan first few TXD entries for RW version 0x1005FFFF.
    """
    try:
        if not path.lower().endswith('.img'):
            return False
        with open(path, 'rb') as f:
            magic = f.read(4)
        if magic != b'VER2':
            return False
        bn = os.path.basename(path).lower()
        # Filename hint — fastest check
        if any(k in bn for k in ('lcs', 'liberty')):
            return True
        # Scan first TXD entry for 0x1005FFFF RW version
        with open(path, 'rb') as f:
            f.seek(4)
            entry_count = struct.unpack('<I', f.read(4))[0]
            if entry_count < 1 or entry_count > 65535:
                return False
            for i in range(min(entry_count, 20)):
                entry_data = f.read(32)
                if len(entry_data) < 32:
                    break
                sec_off, sec_sz = struct.unpack('<II', entry_data[:8])
                name_raw = entry_data[8:32]
                name = name_raw.rstrip(b'\x00').decode('latin-1', errors='replace').lower()
                if not name.endswith('.txd'):
                    continue
                byte_off = sec_off * 2048
                f.seek(byte_off)
                txd_hdr = f.read(8)
                if len(txd_hdr) < 8:
                    break
                rw_ver = struct.unpack('<I', txd_hdr[4:8])[0]
                if rw_ver == 0x1005FFFF:
                    return True
                f.seek(4 + (i + 1) * 32)  # back to directory
    except Exception:
        pass
    return False


#    iOS LVZ parser                                                             

def detect_ios_lvz(path: str) -> bool:  #vers 1
    """Return True if file is an iOS LCS/VCS LVZ (zlib-compressed, DLRW outer magic)."""
    try:
        if not path.lower().endswith('.lvz'):
            return False
        with open(path, 'rb') as f:
            magic = f.read(2)
        return magic[0] == 0x78 and magic[1] in (0xDA, 0x9C, 0x01, 0x5E)
    except Exception:
        return False


def _is_ios_lvz(blob: bytes) -> bool:
    """Return True if this decompressed LVZ blob is iOS format (not PS2).

    Distinguishing features:
    - PS2 LVZ: cell table contains 0xAAAAAAAA sentinels to mark unused slots.
    - iOS LVZ: no 0xAAAAAAAA values; cells are [L1_blob_ptr(4), float_lod(4)];
               L1_ptr values point to DLRW sub-blocks; blob[0x08] == len(blob).
    """
    import struct as _s
    if len(blob) < 0x40:
        return False
    if blob[:4] != b'DLRW':
        return False
    # PS2 check: scan first 200 cell slots for 0xAAAAAAAA sentinel
    entry_count = _s.unpack_from('<I', blob, 0x14)[0]
    scan = min(entry_count, 200)
    for i in range(scan):
        v0 = _s.unpack_from('<I', blob, 0x28 + i * 8)[0]
        v1 = _s.unpack_from('<I', blob, 0x28 + i * 8 + 4)[0]
        if v0 == 0xAAAAAAAA or v1 == 0xAAAAAAAA:
            return False   # PS2 format
    # iOS check: blob[0x08] == total blob size (self-referential size field)
    blob_size_field = _s.unpack_from('<I', blob, 0x08)[0]
    if blob_size_field == len(blob):
        return True
    # Secondary iOS check: at least one cell's L1_ptr points to a DLRW block
    for i in range(min(scan, 20)):
        l1_ptr = _s.unpack_from('<I', blob, 0x28 + i * 8)[0]
        if 0 < l1_ptr < len(blob) - 4:
            if blob[l1_ptr:l1_ptr+4] == b'DLRW':
                return True
    return False


def open_ios_lvz(file_path: str) -> dict:  #vers 2
    """Parse iOS LCS/VCS LVZ streaming archive.

    iOS LVZ structure (decompressed):
      OUTER DLRW header [0x28 bytes]:
        [00] "DLRW" magic
        [08] total blob size
        [0C] abs ptr to relocation table (array of offsets to L1 blocks)
        [14] entry_count (outer world grid cells)

      CELL TABLE at 0x28 (entry_count * 8 bytes):
        [+0] L1_blob_offset — abs offset of Level-1 DLRW block (0=empty)
        [+4] float_value (LOD/position hint)

      LEVEL-1 DLRW block per non-empty grid cell:
        [00] "DLRW"
        [08] block_total_size
        [14] sub_record_count

      L2 RECORDS at L1+0x28 (sub_record_count * 0x20 bytes each):
        [00..0B] geometry data ptrs (abs in blob)
        [0C]     sector_count — number of 2048-byte sectors in companion .img
        [10]     img_byte_offset — absolute byte position in companion .img
        [14]     0
        [18]     0x57524C44 "DLRW" sentinel (validates record)
        [1C]     0

    Returns same dict format as open_lvz().
    """
    import os, struct, zlib as _zlib

    result = {"entries": [], "error": None, "format": "ios_lvz"}

    try:
        with open(file_path, 'rb') as f:
            raw = f.read()
        blob = _zlib.decompress(raw)
    except Exception as e:
        result["error"] = f"Decompress failed: {e}"
        return result

    if blob[:4] != b'DLRW':
        result["error"] = f"Not DLRW: {blob[:4].hex()}"
        return result

    # Reject PS2 LVZ files — they share the DLRW magic but have a different layout
    if not _is_ios_lvz(blob):
        result["error"] = "Not iOS LVZ format (PS2 LVZ detected by 0xAAAAAAAA sentinels)"
        return result

    entry_count = struct.unpack_from('<I', blob, 0x14)[0]
    if entry_count == 0 or entry_count > 100000:
        result["error"] = f"Implausible entry_count {entry_count}"
        return result

    # Find companion .img
    stem = os.path.splitext(file_path)[0]
    companion_img = stem + '.img'
    if not os.path.exists(companion_img):
        companion_img = stem + '.IMG'
    img_size = os.path.getsize(companion_img) if os.path.exists(companion_img) else 0
    img_name = os.path.basename(companion_img)

    SENTINEL = 0x57524C44  # "DLRW"
    entries = []

    for i in range(entry_count):
        cell_off = 0x28 + i * 8
        l1_ptr = struct.unpack_from('<I', blob, cell_off)[0]

        if l1_ptr == 0 or l1_ptr + 0x28 >= len(blob):
            continue
        if blob[l1_ptr:l1_ptr+4] != b'DLRW':
            continue

        sub_count = struct.unpack_from('<I', blob, l1_ptr + 0x14)[0]
        if sub_count == 0 or sub_count > 2000:
            continue

        for j in range(sub_count):
            rec = l1_ptr + 0x28 + j * 0x20
            if rec + 0x20 > len(blob):
                break

            sector_count = struct.unpack_from('<I', blob, rec + 0x0C)[0]
            img_off      = struct.unpack_from('<I', blob, rec + 0x10)[0]
            sentinel     = struct.unpack_from('<I', blob, rec + 0x18)[0]

            if sentinel != SENTINEL:
                continue
            if img_off == 0 or (img_size and img_off >= img_size):
                continue
            if sector_count == 0 or sector_count > 8192:
                continue

            idx = len(entries)
            entries.append({
                "name":       f"stream_{idx:04d}.strm",
                "offset":     img_off,
                "size":       sector_count * 2048,
                "cd_sector":  img_off // 2048,
                "_source_ref": f"{img_name} @ sector {img_off // 2048}",
                "_source_img": companion_img,
                "_cell":      i,
                "_sub":       j,
            })

    result["entries"] = entries
    if not entries:
        result["error"] = "No valid stream entries found"
    return result

