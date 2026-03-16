#this belongs in apps/core/img_dtz.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - GAME.DTZ support for LCS/VCS PS2/PSP
"""
GAME.DTZ parser for GTA Liberty City Stories and Vice City Stories (PS2/PSP).

GAME.DTZ (DaTa Zlib) is the main compressed data container for LCS/VCS.
It contains the CDImage directory (GTA3PS2.DIR), textures, models, IDE, IPL etc.

Format:
  - Entire file is zlib-compressed (deflate, max compression)
  - Decompressed blob starts with a Relocatable Chunk header (sChunkHeader, 28 bytes)
  - VCS PS2 header layout documented at:
    https://gtamods.com/wiki/Game.dtz

References:
  guard3/g3DTZ  — extraction utility (C++, MIT)
  GTAMods wiki  — format documentation
  GTAForums thread #908702 — guard3's workshop

Methods:
  detect_dtz(path)        → bool
  open_dtz(path)          → dict  { version, game, platform, entries, raw_dir, error }
  decompress_dtz(path)    → bytes | None   (raw decompressed blob)
"""

import os
import struct
import zlib

DTZ_SECTOR   = 2048   # CDImage sector size (same as PC .img)
PS2_SECTOR   = 2048   # same thing, but DTZ uses 2048-byte sectors for its DIR

# Relocatable Chunk header - same for LCS and VCS
# struct base::sChunkHeader { uint32[9], uint16, uint16 }  = 28 bytes + padding to 32
CHUNK_HEADER_SIZE = 28

# VCS PS2 decompressed header offsets (all LE uint32 unless noted)
VCS_OFF_DIR          = 0x7C   # offset to GTA3PS2.DIR data inside decompressed blob
VCS_OFF_MOCAP_DIR    = 0xCC   # offset to MOCAPPS2.DIR
VCS_OFF_CUTSCENE_DIR = 0x7C   # same offset — guard3 confirms cutscene DIR at 0x7C
VCS_OFF_IDE_COUNT    = 0x38   # number of entries in IDE table
VCS_OFF_IDE_TABLE    = 0x3C   # offset to IDE offset table

# LCS decompressed header offset for DIR (empirical — wiki stub, verified via g3DTZ source)
LCS_OFF_DIR = 0x68            # guard3's g3DTZ uses offset 0x68 for LCS DIR

# GTA3PS2.DIR entry: 4b offset_sectors + 4b size_sectors + 24b name
DIR_ENTRY_SIZE = 32


def detect_dtz(path: str) -> bool:  #vers 1
    """Return True if the file is a GAME.DTZ (zlib-compressed LCS/VCS container)."""
    try:
        if not path.lower().endswith('.dtz'):
            return False
        if os.path.getsize(path) < 8:
            return False
        with open(path, 'rb') as f:
            magic = f.read(2)
        # zlib deflate magic bytes: 0x78 0xDA (best compression, used by DTZ)
        # Also accept 0x78 0x9C (default) and 0x78 0x01 (low compression)
        return magic[0] == 0x78 and magic[1] in (0xDA, 0x9C, 0x01, 0x5E)
    except Exception:
        return False


def decompress_dtz(path: str) -> bytes | None:  #vers 1
    """Decompress GAME.DTZ and return the raw decompressed blob, or None on error."""
    try:
        with open(path, 'rb') as f:
            compressed = f.read()
        return zlib.decompress(compressed)
    except zlib.error:
        # Try with wbits=-15 (raw deflate, no header)
        try:
            with open(path, 'rb') as f:
                compressed = f.read()
            return zlib.decompress(compressed, -15)
        except Exception:
            return None
    except Exception:
        return None


def _parse_dir_entries(blob: bytes, dir_offset: int, file_label: str = '') -> list:
    """Parse a GTA3PS2.DIR block from the decompressed DTZ blob.

    DIR entries are 32 bytes each:
      [0:4]  sector offset (multiply by DTZ_SECTOR for byte offset into DTZ data)
      [4:8]  sector size
      [8:32] null-terminated filename
    """
    entries = []
    pos = dir_offset
    idx = 0
    blob_len = len(blob)

    while pos + DIR_ENTRY_SIZE <= blob_len:
        chunk = blob[pos:pos + DIR_ENTRY_SIZE]

        # Stop at all-zero sentinel
        if chunk == b'\x00' * DIR_ENTRY_SIZE:
            break

        sec_off  = struct.unpack_from('<I', chunk, 0)[0]
        sec_sz   = struct.unpack_from('<I', chunk, 4)[0]

        # Null-terminated name in bytes 8..31
        name_raw = chunk[8:32]
        name = name_raw.split(b'\x00')[0].decode('ascii', errors='replace').strip()

        # Sanity checks
        if not name or sec_off == 0 and sec_sz == 0:
            pos += DIR_ENTRY_SIZE
            idx += 1
            continue
        # Name must be printable ASCII
        if not all(0x20 <= ord(c) < 0x7F for c in name):
            break

        byte_off = sec_off * DTZ_SECTOR
        byte_sz  = sec_sz  * DTZ_SECTOR

        entries.append({
            'name':   name,
            'offset': byte_off,
            'size':   byte_sz,
            'index':  idx,
        })
        idx  += 1
        pos  += DIR_ENTRY_SIZE

    return entries


def _detect_game_from_blob(blob: bytes) -> str:
    """Heuristically identify LCS vs VCS from the decompressed blob."""
    # VCS has font data pointer at 0xF4; LCS does not (stub in wiki)
    # More reliable: check ident field of sChunkHeader
    # ident values are game-specific magic numbers
    if len(blob) < 8:
        return 'UNKNOWN'
    ident = struct.unpack_from('<I', blob, 0)[0]
    # Known ident values from g3DTZ configurations:
    #   LCS PS2:  0x4C435332  ('LCS2' or similar)
    #   VCS PS2:  0x56435332  ('VCS2' or similar)
    # Fallback: use header offsets to probe for valid data
    if len(blob) > VCS_OFF_DIR + 4:
        vcs_dir_off = struct.unpack_from('<I', blob, VCS_OFF_DIR)[0]
        if 0 < vcs_dir_off < len(blob) - DIR_ENTRY_SIZE:
            # Check if there's a valid-looking DIR entry at that offset
            test = blob[vcs_dir_off:vcs_dir_off + DIR_ENTRY_SIZE]
            if len(test) == DIR_ENTRY_SIZE:
                name_raw = test[8:32].split(b'\x00')[0]
                if name_raw and all(0x20 <= b < 0x7F for b in name_raw):
                    return 'VCS'
    # Try LCS offset
    if len(blob) > LCS_OFF_DIR + 4:
        lcs_dir_off = struct.unpack_from('<I', blob, LCS_OFF_DIR)[0]
        if 0 < lcs_dir_off < len(blob) - DIR_ENTRY_SIZE:
            test = blob[lcs_dir_off:lcs_dir_off + DIR_ENTRY_SIZE]
            if len(test) == DIR_ENTRY_SIZE:
                name_raw = test[8:32].split(b'\x00')[0]
                if name_raw and all(0x20 <= b < 0x7F for b in name_raw):
                    return 'LCS'
    return 'UNKNOWN'


def _detect_platform_from_blob(blob: bytes, path: str) -> str:
    """Detect PS2 vs PSP from filename and/or blob content."""
    lower = os.path.basename(path).lower()
    if 'psp' in lower:
        return 'PSP'
    # PSP builds have SFX.SDT data; no reliable header-level distinction
    # Default to PS2 — user can correct in the UI if needed
    return 'PS2'


def open_dtz(path: str) -> dict:  #vers 1
    """Decompress and parse a GAME.DTZ file.

    Returns:
      {
        'version':  'DTZ_VCS' | 'DTZ_LCS' | 'DTZ_UNKNOWN',
        'game':     'VCS' | 'LCS' | 'UNKNOWN',
        'platform': 'PS2' | 'PSP',
        'entries':  [ { name, offset, size, index }, ... ],
        'raw_dir':  bytes | None,   # raw GTA3PS2.DIR bytes from blob
        'blob':     bytes | None,   # full decompressed blob (may be large)
        'error':    str | None,
      }
    """
    result = {
        'version':  'DTZ_UNKNOWN',
        'game':     'UNKNOWN',
        'platform': 'PS2',
        'entries':  [],
        'raw_dir':  None,
        'blob':     None,
        'error':    None,
    }

    blob = decompress_dtz(path)
    if blob is None:
        result['error'] = 'Failed to decompress GAME.DTZ'
        return result

    result['blob'] = blob
    blob_len = len(blob)

    # Detect game (VCS or LCS)
    game = _detect_game_from_blob(blob)
    result['game']     = game
    result['platform'] = _detect_platform_from_blob(blob, path)
    result['version']  = f'DTZ_{game}'

    # Find the CDImage directory offset
    dir_header_off = VCS_OFF_DIR if game == 'VCS' else LCS_OFF_DIR

    if blob_len <= dir_header_off + 4:
        result['error'] = f'Blob too small for {game} header ({blob_len} bytes)'
        return result

    dir_data_off = struct.unpack_from('<I', blob, dir_header_off)[0]

    if dir_data_off == 0 or dir_data_off >= blob_len:
        result['error'] = f'Invalid DIR offset 0x{dir_data_off:X} in {game} header'
        return result

    # Extract raw DIR bytes (up to 64 KB should cover any realistic directory)
    dir_raw_limit = min(blob_len - dir_data_off, 65536)
    result['raw_dir'] = blob[dir_data_off:dir_data_off + dir_raw_limit]

    # Parse entries
    entries = _parse_dir_entries(blob, dir_data_off)
    if not entries:
        result['error'] = f'No DIR entries found at blob offset 0x{dir_data_off:X}'
    result['entries'] = entries

    return result


__all__ = ['detect_dtz', 'decompress_dtz', 'open_dtz']
