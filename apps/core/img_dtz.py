#this belongs in apps/core/img_dtz.py - Version: 2
# X-Seti - March 2026 - IMG Factory 1.6 - GAME.DTZ support for LCS/VCS PS2/PSP
"""
GAME.DTZ (DaTa Zlib) parser for GTA Liberty City Stories and Vice City Stories.

GAME.DTZ is the main compressed data container on PS2/PSP.
It is zlib-compressed and contains:
  - IDE, IPL, handling, weapons, pedestrian and particle data
  - MOCAPPS2.DIR — cutscene animation directory (147 entries)
  - Binary game data (reloc'd pointers, compiled tables)

It does NOT contain the main streaming assets (GTA3.IMG/BEACH.IMG etc).
Those are in separate files: GTA3.DIR+GTA3.IMG (LCS) or BEACH/MAINLA/MALL+.LVZ (VCS).

Blob structure:
  - 'GATG' magic at byte 0 (both LCS and VCS PS2)
  - sChunkHeader (28 bytes): ident, shrink, fileEnd, dataEnd, relocTab, numRelocs,
    globalTab, numClasses, numFuncs
  - ALL pointer values in the header are VIRTUAL ADDRESSES = file_offset + load_base
  - header[0xCC] → MOCAPPS2.DIR (147 cutscene entries, verified against standalone file)
  - The load_base = header[0xCC] - actual_blob_offset_of_mocapps2_dir

What to show in IMG Factory:
  - Present GAME.DTZ as a read-only archive with the MOCAPPS2 entries
  - Each entry maps to MOCAPPS2.IMG (not to the DTZ itself)

References:
  guard3/g3DTZ — C++ extraction utility (MIT)
  GTAMods wiki  — https://gtamods.com/wiki/Game.dtz
  Sample analysis — March 2026, GAME.DTZ from VCS PS2 disc
"""

import os
import struct
import zlib

# DTZ blob magic
GATG_MAGIC = b'GATG'     # Both LCS and VCS PS2

# sChunkHeader offsets (all LE)
HDR_IDENT    = 0x00   # uint32 'GATG'
HDR_SHRINK   = 0x04   # uint32 1 = reloc applied
HDR_FILE_END = 0x08   # uint32 virtual address of file end
HDR_DATA_END = 0x0C   # uint32 virtual address of data end
HDR_RELOC    = 0x10   # uint32 virtual address of reloc table
HDR_NRELOCS  = 0x14   # uint32 number of relocs
HDR_GLOBAL   = 0x18   # uint32 virtual address of global table
HDR_NCLASSES = 0x1C   # uint16 number of classes
HDR_NFUNCS   = 0x1E   # uint16 number of functions

# Pointer to MOCAPPS2.DIR inside the blob (virtual address, needs base subtraction)
HDR_MOCAP_DIR_PTR = 0xCC   # confirmed from sample analysis

# DIR entry format: same as V1 PC/PS2 (2048-byte sectors)
DIR_ENTRY_SIZE  = 32
DIR_SECTOR_SIZE = 2048   # MOCAPPS2 uses 2048-byte sectors (verified: 15827*2048 = file size)


def detect_dtz(path: str) -> bool:  #vers 2
    """Return True if file is a GAME.DTZ (zlib-compressed LCS/VCS container)."""
    try:
        if not path.lower().endswith('.dtz'):
            return False
        if os.path.getsize(path) < 16:
            return False
        with open(path, 'rb') as f:
            magic2 = f.read(2)
        # zlib deflate magic bytes
        return magic2[0] == 0x78 and magic2[1] in (0xDA, 0x9C, 0x01, 0x5E)
    except Exception:
        return False


def decompress_dtz(path: str) -> bytes | None:  #vers 2
    """Decompress GAME.DTZ and return the raw blob, or None on error."""
    try:
        with open(path, 'rb') as f:
            data = f.read()
        return zlib.decompress(data)
    except zlib.error:
        try:
            with open(path, 'rb') as f:
                data = f.read()
            return zlib.decompress(data, -15)
        except Exception:
            return None
    except Exception:
        return None


def _find_mocap_dir(blob: bytes) -> tuple[int, int] | tuple[None, None]:
    """Locate MOCAPPS2.DIR in the blob.

    The header at offset 0xCC holds the VIRTUAL ADDRESS of the dir data.
    Virtual address = blob_offset + load_base.
    We find load_base by scanning for the actual dir data, then back-compute.

    Returns (blob_offset, load_base) or (None, None).
    """
    if len(blob) < HDR_MOCAP_DIR_PTR + 4:
        return None, None

    vptr = struct.unpack_from('<I', blob, HDR_MOCAP_DIR_PTR)[0]
    if vptr == 0:
        return None, None

    # Scan blob for a contiguous run of valid DIR entries (min 10)
    # DIR entry: sec_off[4] + sec_sz[4] + name[24] (printable ASCII + null-padded)
    best_pos = None
    best_count = 0

    pos = 0
    while pos + 320 <= len(blob):
        count = 0
        p = pos
        while p + DIR_ENTRY_SIZE <= len(blob):
            chunk = blob[p:p + DIR_ENTRY_SIZE]
            sec_off = struct.unpack_from('<I', chunk, 0)[0]
            sec_sz  = struct.unpack_from('<I', chunk, 4)[0]
            name_raw = chunk[8:32].split(b'\x00')[0]
            if (name_raw and len(name_raw) >= 4 and
                    all(0x20 <= b < 0x7F for b in name_raw) and
                    b'.' in name_raw and 0 < sec_sz < 50000):
                count += 1
                p += DIR_ENTRY_SIZE
            else:
                break
        if count > best_count:
            best_count = count
            best_pos = pos
        pos += 4

    if best_pos is None or best_count < 5:
        return None, None

    load_base = vptr - best_pos
    return best_pos, load_base


def _parse_dir(blob: bytes, dir_offset: int, count_hint: int = 0) -> list:
    """Parse DIR entries from the blob at dir_offset."""
    entries = []
    pos = dir_offset
    idx = 0
    while pos + DIR_ENTRY_SIZE <= len(blob):
        chunk = blob[pos:pos + DIR_ENTRY_SIZE]
        if chunk == b'\x00' * DIR_ENTRY_SIZE:
            break
        sec_off = struct.unpack_from('<I', chunk, 0)[0]
        sec_sz  = struct.unpack_from('<I', chunk, 4)[0]
        name_raw = chunk[8:32].split(b'\x00')[0]
        if not name_raw or not all(0x20 <= b < 0x7F for b in name_raw):
            break
        name = name_raw.decode('ascii', errors='replace')
        entries.append({
            'name':   name,
            'offset': sec_off * DIR_SECTOR_SIZE,
            'size':   sec_sz  * DIR_SECTOR_SIZE,
            'index':  idx,
            'source': 'MOCAPPS2.IMG',   # entries reference MOCAPPS2.IMG
        })
        idx += 1
        pos += DIR_ENTRY_SIZE
        if count_hint and idx >= count_hint:
            break
    return entries


def open_dtz(path: str) -> dict:  #vers 2
    """Decompress and parse a GAME.DTZ file.

    Returns entries from MOCAPPS2.DIR (cutscene animations).
    Each entry's offset/size refers to MOCAPPS2.IMG in the same directory.

    Returns:
      {
        'version':   'DTZ_VCS' | 'DTZ_LCS' | 'DTZ_UNKNOWN',
        'game':      'VCS' | 'LCS' | 'UNKNOWN',
        'platform':  'PS2' | 'PSP',
        'entries':   [ {name, offset, size, index, source}, ... ],
        'blob':      bytes | None,
        'load_base': int | None,
        'error':     str | None,
      }
    """
    result = {
        'version':   'DTZ_UNKNOWN',
        'game':      'UNKNOWN',
        'platform':  'PS2',
        'entries':   [],
        'blob':      None,
        'load_base': None,
        'error':     None,
    }

    blob = decompress_dtz(path)
    if blob is None:
        result['error'] = 'Failed to decompress GAME.DTZ'
        return result

    result['blob'] = blob

    # Verify GATG magic
    if blob[:4] != GATG_MAGIC:
        result['error'] = f'Unexpected magic {blob[:4].hex()} (expected GATG/47415447)'
        return result

    # Game detection — both LCS and VCS use GATG, differentiate by ident value
    ident = struct.unpack_from('<I', blob, 0)[0]
    # 0x47544147 = 'GATG' in both; we'll use 'UNKNOWN' for now until we find
    # a reliable LCS vs VCS discriminator in the header
    result['game'] = 'UNKNOWN'
    result['version'] = 'DTZ_UNKNOWN'

    # Platform from filename
    lower = os.path.basename(path).lower()
    result['platform'] = 'PSP' if 'psp' in lower else 'PS2'

    # Locate MOCAPPS2.DIR
    dir_offset, load_base = _find_mocap_dir(blob)
    if dir_offset is None:
        result['error'] = 'Could not locate MOCAPPS2.DIR in blob'
        return result

    result['load_base'] = load_base

    # Parse entries
    entries = _parse_dir(blob, dir_offset)
    if not entries:
        result['error'] = 'No entries found in MOCAPPS2.DIR'
        return result

    result['entries'] = entries
    result['version'] = 'DTZ_VCS'   # default; refine when LCS discriminator found
    result['game']    = 'VCS'

    return result


__all__ = ['detect_dtz', 'decompress_dtz', 'open_dtz']
