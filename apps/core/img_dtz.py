#this belongs in apps/core/img_dtz.py - Version: 3
# X-Seti - March 2026 - IMG Factory 1.6 - GAME.DTZ support for LCS/VCS PS2/PSP
"""
GAME.DTZ (DaTa Zlib) parser for GTA Liberty City Stories and Vice City Stories.

Structure (confirmed from guard3/g3DTZ v2.1 source + sample analysis):
  - zlib-compressed blob
  - Decompressed blob starts with cRelocChunk header (32 bytes):
      ident(4)='GTAG', shrink(4), fileEnd(4), dataEnd(4),
      relocTab(4), numRelocs(4), globalTab(4), numClasses(2), numFuncs(2)
  - All pointer values in the blob are RAW FILE OFFSETS (base=0 Fixup)
  - sResourceImage struct at blob[32]
  - streamingInst at sResourceImage+92 = blob[0x7C]
  - VCS PS2 CStreaming (240 bytes): texOffset, colOffset, anmOffset, numStreamInfos,
    m_aStreamingInfos ptr at CStreaming+200
  - VCS PS2 CStreamingInfo (24 bytes): ...[16]=cdPosn, [20]=cdSize (2048-byte sectors)
  - Model names from modelInfoPtrs (CRC32 hashes) resolved via lcs_vcs_names.py
  - MOCAPPS2.DIR (cutscene directory) at header[0xCC] (also a raw file offset)

VCS PS2 sResourceImage offsets (from blob[32]):
  +24 = numModelInfos (blob[0x38])
  +28 = modelInfoPtrs (blob[0x3C])
  +64 = texListPool   (blob[0x60])
  +68 = storedTexList (blob[0x64])
  +72 = colPool       (blob[0x68])
  +92 = streamingInst (blob[0x7C])
  +96 = animManagerInst (blob[0x80])

VCS PS2 CStreaming offsets:
  +8  = texOffset  (number of mdl entries)
  +12 = colOffset  (mdl + tex count)
  +16 = anmOffset  (mdl + tex + col count)
  +20 = numStreamInfos
  +200 = m_aStreamingInfos (pointer to CStreamingInfo array)
"""

import os
import struct
import zlib

GTAG_MAGIC    = b'GTAG'
CHUNK_SIZE    = 32   # sizeof(cRelocChunk)
DIR_SECTOR_SZ = 2048

# sResourceImage field offsets from blob start (blob[0] = cRelocChunk)
# All = CHUNK_SIZE + field_offset_within_sResourceImage
RI_NUM_MODELS  = 0x38   # numModelInfos (uint32)
RI_MODEL_PTRS  = 0x3C   # modelInfoPtrs (ptr → CBaseModelInfo*[])
RI_TEX_POOL    = 0x60   # texListPool ptr
RI_COL_POOL    = 0x68   # colPool ptr
RI_STREAMING   = 0x7C   # streamingInst ptr
RI_ANIM_MGR    = 0x80   # animManagerInst ptr
RI_MOCAP_DIR   = 0xCC   # cutsceneDir ptr (VCS=MOCAPPS2.DIR, LCS=cuts.dir)

# VCS PS2 CStreamingInfo size and field offsets
VCS_SINFO_SIZE   = 24
VCS_SINFO_CDPOSN = 16
VCS_SINFO_CDSIZE = 20

# CStreaming field offsets (VCS PS2)
CS_TEX_OFFSET   = 8
CS_COL_OFFSET   = 12
CS_ANM_OFFSET   = 16
CS_NUM_INFOS    = 20
CS_STREAM_ARR   = 200   # m_aStreamingInfos ptr

# CBaseModelInfo: unknown[8] + m_hashname(4) at offset 8
MI_HASH_OFFSET  = 8


def detect_dtz(path: str) -> bool:  #vers 3
    """Return True if file is a GAME.DTZ (zlib-compressed LCS/VCS container)."""
    try:
        if not path.lower().endswith('.dtz'):
            return False
        if os.path.getsize(path) < 16:
            return False
        with open(path, 'rb') as f:
            magic2 = f.read(2)
        return magic2[0] == 0x78 and magic2[1] in (0xDA, 0x9C, 0x01, 0x5E)
    except Exception:
        return False


def decompress_dtz(path: str) -> bytes | None:  #vers 3
    """Decompress GAME.DTZ and return raw blob, or None on error."""
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


def _read_ptr(blob: bytes, offset: int) -> int:
    """Read a 4-byte pointer (raw file offset) from blob."""
    if offset + 4 > len(blob):
        return 0
    return struct.unpack_from('<I', blob, offset)[0]


def _resolve_name(crc: int, game: str) -> str:
    """Resolve CRC32 hash to model name using name tables."""
    try:
        from apps.core.lcs_vcs_names import VCS_NAMES, LCS_NAMES
        table = VCS_NAMES if game == 'VCS' else LCS_NAMES
        return table.get(crc) or (VCS_NAMES if game == 'LCS' else LCS_NAMES).get(crc) or f'unk_{crc:08x}'
    except ImportError:
        return f'unk_{crc:08x}'


def _build_streaming_dir(blob: bytes, game: str) -> list:
    """Build GTA3PS2.DIR entries from the streaming instance in the blob.

    Returns list of {name, offset(bytes), size(bytes), index, file_type}.
    """
    entries = []

    streaming_foff = _read_ptr(blob, RI_STREAMING)
    if not streaming_foff or streaming_foff + 240 > len(blob):
        return entries

    tex_off  = struct.unpack_from('<I', blob, streaming_foff + CS_TEX_OFFSET)[0]
    col_off  = struct.unpack_from('<I', blob, streaming_foff + CS_COL_OFFSET)[0]
    anm_off  = struct.unpack_from('<I', blob, streaming_foff + CS_ANM_OFFSET)[0]
    n_infos  = struct.unpack_from('<I', blob, streaming_foff + CS_NUM_INFOS)[0]

    if n_infos == 0 or n_infos > 50000:
        return entries

    sarr_foff = _read_ptr(blob, streaming_foff + CS_STREAM_ARR)
    if not sarr_foff or sarr_foff + n_infos * VCS_SINFO_SIZE > len(blob):
        return entries

    mi_ptr_table = _read_ptr(blob, RI_MODEL_PTRS)
    num_models   = _read_ptr(blob, RI_NUM_MODELS)

    file_idx = 0
    for i in range(n_infos):
        off      = sarr_foff + i * VCS_SINFO_SIZE
        cd_posn  = struct.unpack_from('<I', blob, off + VCS_SINFO_CDPOSN)[0]
        cd_size  = struct.unpack_from('<I', blob, off + VCS_SINFO_CDSIZE)[0]

        if cd_size == 0 or cd_size >= 0xFFFF:
            continue
        if cd_posn == 0xFFFFFFFF:
            continue

        # Determine file type and name
        if i < tex_off:
            file_type = 'mdl'
            ext       = '.mdl'
            name      = f'mdl_{i:04d}'
            if mi_ptr_table and i < num_models:
                mi_ptr = _read_ptr(blob, mi_ptr_table + i * 4)
                if 0 < mi_ptr < len(blob) - 16:
                    crc  = _read_ptr(blob, mi_ptr + MI_HASH_OFFSET)
                    name = _resolve_name(crc, game)
        elif i < col_off:
            file_type = 'xtx' if game == 'VCS' else 'chk'
            ext       = f'.{file_type}'
            name      = f'tex_{i - tex_off:04d}'
        elif i < anm_off:
            file_type = 'col2'
            ext       = '.col2'
            name      = f'col_{i - col_off:04d}'
        else:
            file_type = 'anim'
            ext       = '.anim'
            name      = f'anm_{i - anm_off:04d}'

        entries.append({
            'name':      name + ext,
            'offset':    cd_posn * DIR_SECTOR_SZ,
            'size':      cd_size * DIR_SECTOR_SZ,
            'index':     file_idx,
            'file_type': file_type,
        })
        file_idx += 1

    # Sort by sector offset
    entries.sort(key=lambda e: e['offset'])
    for i, e in enumerate(entries):
        e['index'] = i

    return entries


def _build_mocap_dir(blob: bytes) -> list:
    """Parse MOCAPPS2.DIR / cuts.dir from the blob.

    Returns list of {name, offset(bytes), size(bytes), index}.
    """
    entries = []
    dir_foff = _read_ptr(blob, RI_MOCAP_DIR)
    if not dir_foff or dir_foff >= len(blob):
        return entries

    idx = 0
    pos = dir_foff
    while pos + 32 <= len(blob):
        chunk   = blob[pos:pos + 32]
        sec_off = struct.unpack_from('<I', chunk, 0)[0]
        sec_sz  = struct.unpack_from('<I', chunk, 4)[0]
        name_raw = chunk[8:32].split(b'\x00')[0]

        if not name_raw or not all(0x20 <= b < 0x7F for b in name_raw):
            break
        name = name_raw.decode('ascii', errors='replace')
        if b'.' not in name_raw:
            break

        entries.append({
            'name':      name,
            'offset':    sec_off * DIR_SECTOR_SZ,
            'size':      sec_sz  * DIR_SECTOR_SZ,
            'index':     idx,
            'file_type': name.rsplit('.', 1)[-1].lower() if '.' in name else 'bin',
        })
        idx  += 1
        pos  += 32

    return entries


def open_dtz(path: str) -> dict:  #vers 3
    """Decompress and parse GAME.DTZ.

    Returns two sets of entries:
      - 'entries': streaming dir (mdl/xtx/chk/col2/anim) — main asset list
      - 'mocap_entries': cutscene dir (CAM/CUT/ANIM) → MOCAPPS2.IMG / CUTS.IMG

    'entries' offsets/sizes are in 2048-byte sectors × DIR_SECTOR_SZ.
    They reference the companion streaming IMG (GTA3.IMG for LCS, or the
    area-specific IMG for VCS streaming areas).
    """
    result = {
        'version':      'DTZ_UNKNOWN',
        'game':         'UNKNOWN',
        'platform':     'PS2',
        'entries':      [],
        'mocap_entries': [],
        'blob':         None,
        'error':        None,
    }

    blob = decompress_dtz(path)
    if blob is None:
        result['error'] = 'Failed to decompress GAME.DTZ'
        return result

    result['blob'] = blob

    if blob[:4] != GTAG_MAGIC:
        result['error'] = f'Unexpected magic {blob[:4].hex()} (expected GTAG/47415447)'
        return result

    lower = os.path.basename(path).lower()
    result['platform'] = 'PSP' if 'psp' in lower else 'PS2'

    # Build streaming directory (main asset list)
    game = 'VCS'  # default; LCS would have different offsets (TODO when LCS sample available)
    result['game']    = game
    result['version'] = f'DTZ_{game}'

    entries = _build_streaming_dir(blob, game)
    result['entries'] = entries if entries else []

    # Also parse MOCAPPS2.DIR
    result['mocap_entries'] = _build_mocap_dir(blob)

    if not entries and not result['mocap_entries']:
        result['error'] = 'No entries found in GAME.DTZ'

    return result


__all__ = ['detect_dtz', 'decompress_dtz', 'open_dtz']
