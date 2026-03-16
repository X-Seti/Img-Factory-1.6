#this belongs in apps/core/img_dtz.py - Version: 4
# X-Seti - March 2026 - IMG Factory 1.6 - GAME.DTZ support for LCS/VCS PS2/PSP
"""
GAME.DTZ (DaTa Zlib) parser for GTA Liberty City Stories and Vice City Stories.

HOW g3DTZ GETS NAMES (from guard3/g3DTZ v2.1 source):
  g3DTZ calls LoadResourceImage() which decompresses the blob and calls Fixup()
  (relocation with base=runtime_address). Then it reads four name sources:

  1. MDL names:  CModelInfo::GetModelInfo(i)->GetModelName()
     → CBaseModelInfo.m_hashname (CRC32) resolved via lcsnames.inc / vcsnames.inc

  2. TEX names:  CTexListStore::GetSlot(i)->GetName()
     → CPool<TexListDef>.m_entries[i].m_name (char[20], plain ASCII)
     → Pool at sResourceImage.texListPool (blob[0x60])
     → TexListDef = RslTexList*(4) + refCount(4) + name[20] = 28 bytes

  3. COL names:  CColStore::GetSlot(i)->GetName()
     → CPool<ColDef>.m_entries[i].m_name (char[20], plain ASCII)
     → Pool at sResourceImage.colPool (blob[0x68])
     → ColDef (VCS) = void*(4)+bool(4)+CRect(16)+CRect(16)+name[20]+... = 72 bytes

  4. ANIM names: CAnimManager::GetAnimationBlock(i)->m_name
     → CAnimBlock array found by scanning blob for ASCII name array
     → CAnimBlock = name[20]+bool+pad(4)+numRefs+pad(4)+firstIdx(4)+numAnims(4)+chunkData*(4)+unk0(4)+unk1(4) = 48 bytes

  All four sources reside inside the decompressed GAME.DTZ blob itself.
  Pointer values in the blob are raw file offsets (base=0 Fixup).

VCS PS2 sResourceImage offsets from blob[0] (after 32-byte cRelocChunk):
  blob[0x38] = numModelInfos
  blob[0x3C] = modelInfoPtrs → CBaseModelInfo*[]
  blob[0x60] = texListPool  → CPool<TexListDef>
  blob[0x68] = colPool      → CPool<ColDef>
  blob[0x7C] = streamingInst → CStreaming
  blob[0x80] = animManagerInst → CAnimManager

VCS PS2 CStreaming offsets:
  +8  = texOffset  (number of MDL streaming slots)
  +12 = colOffset  (MDL + TEX count)
  +16 = anmOffset  (MDL + TEX + COL count)
  +20 = numStreamInfos (total)
  +200 = m_aStreamingInfos ptr → CStreamingInfo[numStreamInfos]

VCS PS2 CStreamingInfo = 24 bytes: field_0(4)+next(4)+prev(4)+status(1)+flags(1)+nextModel(2)+cdPosn(4)+cdSize(4)

VCS PS2 CAnimManager layout:
  [0x0000..0x2C40] inline AnimAssocDefinition array (340 entries × 0x34 bytes)
  [0x2C40+] pointer fields then CAnimBlock array starts at discovered offset

Confirmed offsets from VCS PS2 GAME.DTZ sample analysis:
  CAnimBlock array: blob[0x323DD0], 48 bytes/entry, 90 streaming anim blocks
"""

import os
import struct
import zlib

GTAG_MAGIC    = b'GTAG'
CHUNK_SIZE    = 32
DIR_SECTOR_SZ = 2048

# sResourceImage field offsets (from blob start)
RI_NUM_MODELS  = 0x38
RI_MODEL_PTRS  = 0x3C
RI_TEX_POOL    = 0x60
RI_COL_POOL    = 0x68
RI_STREAMING   = 0x7C
RI_ANIM_MGR    = 0x80
RI_MOCAP_DIR   = 0xCC

# CStreaming offsets (VCS PS2)
CS_TEX_OFF   = 8
CS_COL_OFF   = 12
CS_ANM_OFF   = 16
CS_NUM_INFO  = 20
CS_SARR_PTR  = 200

# CStreamingInfo (VCS PS2): 24 bytes
SINFO_SZ     = 24
SINFO_POSN   = 16
SINFO_SIZE   = 20

# Pool structure: m_entries*(4) + m_flags*(4) + m_size(4) + m_allocPtr(4) [+ m_name[16] VCS PS2]
POOL_ENTRIES = 0
POOL_FLAGS   = 4
POOL_SIZE    = 8

# TexListDef: RslTexList*(4) + refCount(4) + name[20]
TEX_DEF_SZ   = 28
TEX_DEF_NAME = 8

# ColDef (VCS PS2): void*(4)+bool(4)+CRect(16)+CRect(16)+name[20]+firstIdx(4)+lastIdx(4)+chk*(4)
COL_DEF_SZ   = 72
COL_DEF_NAME = 40

# CAnimBlock: name[20]+bool+pad(4)+numRefs+pad(4)+firstIdx(4)+numAnims(4)+chunkData*(4)+unk0(4)+unk1(4)
ANIM_BLOCK_SZ   = 48
ANIM_BLOCK_NAME = 0

# CBaseModelInfo: unknown[8] + m_hashname(4)
MI_HASH_OFF  = 8


def detect_dtz(path: str) -> bool:  #vers 4
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


def decompress_dtz(path: str) -> bytes | None:  #vers 4
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


def _rptr(blob: bytes, offset: int) -> int:
    if offset + 4 > len(blob):
        return 0
    return struct.unpack_from('<I', blob, offset)[0]


def _resolve_mdl_name(blob: bytes, mi_ptr_table: int, idx: int, game: str) -> str:
    try:
        from apps.core.lcs_vcs_names import VCS_NAMES, LCS_NAMES
        mi_ptr = _rptr(blob, mi_ptr_table + idx * 4)
        if not (0 < mi_ptr < len(blob) - 16):
            return f'mdl_{idx:04d}'
        crc = _rptr(blob, mi_ptr + MI_HASH_OFF)
        table = VCS_NAMES if game == 'VCS' else LCS_NAMES
        name = table.get(crc) or (LCS_NAMES if game == 'VCS' else VCS_NAMES).get(crc)
        return name or f'unk_{crc:08x}'
    except Exception:
        return f'mdl_{idx:04d}'


def _resolve_tex_name(blob: bytes, tex_entries_ptr: int, idx: int) -> str:
    try:
        off = tex_entries_ptr + idx * TEX_DEF_SZ + TEX_DEF_NAME
        name_raw = blob[off:off+20].split(b'\x00')[0]
        if name_raw and all(0x20 <= b < 0x7F for b in name_raw):
            return name_raw.decode('ascii', errors='replace')
    except Exception:
        pass
    return f'tex_{idx:04d}'


def _resolve_col_name(blob: bytes, col_entries_ptr: int, idx: int) -> str:
    try:
        off = col_entries_ptr + idx * COL_DEF_SZ + COL_DEF_NAME
        name_raw = blob[off:off+20].split(b'\x00')[0]
        if name_raw and all(0x20 <= b < 0x7F for b in name_raw):
            return name_raw.decode('ascii', errors='replace')
    except Exception:
        pass
    return f'col_{idx:04d}'


def _find_anim_block_array(blob: bytes) -> int:
    """Find the CAnimBlock array by scanning for a compact run of 20-char ASCII names.
    Returns the base file offset of the array, or 0 if not found.
    Confirmed at 0x323DD0 for VCS PS2 GAME.DTZ sample.
    """
    # First try the known VCS PS2 offset
    candidate = 0x323DD0
    if candidate + ANIM_BLOCK_SZ * 5 < len(blob):
        names_ok = 0
        for j in range(5):
            nr = blob[candidate + j*ANIM_BLOCK_SZ : candidate + j*ANIM_BLOCK_SZ + 20].split(b'\x00')[0]
            if nr and all(0x20 <= b < 0x7F for b in nr):
                names_ok += 1
        if names_ok >= 4:
            return candidate

    # Fallback: scan blob for 10+ consecutive valid CAnimBlock entries
    for pos in range(0, len(blob) - ANIM_BLOCK_SZ * 10, 4):
        count = 0
        for j in range(10):
            nr = blob[pos + j*ANIM_BLOCK_SZ : pos + j*ANIM_BLOCK_SZ + 20].split(b'\x00')[0]
            if (nr and len(nr) >= 2 and all(0x20 <= b < 0x7F for b in nr) and
                    0 < struct.unpack_from('<i', blob, pos + j*ANIM_BLOCK_SZ + 28)[0] < 5000):
                count += 1
            else:
                break
        if count >= 10:
            return pos

    return 0


def _resolve_anim_name(blob: bytes, anim_block_base: int, idx: int) -> str:
    try:
        if anim_block_base == 0:
            return f'anim_{idx:04d}'
        off = anim_block_base + idx * ANIM_BLOCK_SZ
        name_raw = blob[off:off+20].split(b'\x00')[0]
        if name_raw and all(0x20 <= b < 0x7F for b in name_raw):
            return name_raw.decode('ascii', errors='replace')
    except Exception:
        pass
    return f'anim_{idx:04d}'


def _build_streaming_dir(blob: bytes, game: str) -> list:
    """Build complete streaming directory from GAME.DTZ blob."""
    entries = []

    streaming_foff = _rptr(blob, RI_STREAMING)
    if not streaming_foff or streaming_foff + 240 > len(blob):
        return entries

    tex_off  = struct.unpack_from('<I', blob, streaming_foff + CS_TEX_OFF)[0]
    col_off  = struct.unpack_from('<I', blob, streaming_foff + CS_COL_OFF)[0]
    anm_off  = struct.unpack_from('<I', blob, streaming_foff + CS_ANM_OFF)[0]
    n_infos  = struct.unpack_from('<I', blob, streaming_foff + CS_NUM_INFO)[0]

    if n_infos == 0 or n_infos > 50000:
        return entries

    sarr_foff = _rptr(blob, streaming_foff + CS_SARR_PTR)
    if not sarr_foff or sarr_foff + n_infos * SINFO_SZ > len(blob):
        return entries

    mi_ptr_table  = _rptr(blob, RI_MODEL_PTRS)

    tex_pool_foff = _rptr(blob, RI_TEX_POOL)
    tex_entries   = _rptr(blob, tex_pool_foff + POOL_ENTRIES) if tex_pool_foff else 0

    col_pool_foff = _rptr(blob, RI_COL_POOL)
    col_entries   = _rptr(blob, col_pool_foff + POOL_ENTRIES) if col_pool_foff else 0

    anim_block_base = _find_anim_block_array(blob)

    file_idx = 0
    for i in range(n_infos):
        off     = sarr_foff + i * SINFO_SZ
        cd_posn = struct.unpack_from('<I', blob, off + SINFO_POSN)[0]
        cd_size = struct.unpack_from('<I', blob, off + SINFO_SIZE)[0]

        if cd_size == 0 or cd_size >= 0xFFFF:
            continue
        if cd_posn == 0xFFFFFFFF:
            continue

        if i < tex_off:
            ext  = '.mdl'
            ft   = 'mdl'
            name = _resolve_mdl_name(blob, mi_ptr_table, i, game)
        elif i < col_off:
            ext  = '.xtx' if game == 'VCS' else '.chk'
            ft   = ext[1:]
            name = _resolve_tex_name(blob, tex_entries, i - tex_off)
        elif i < anm_off:
            ext  = '.col2'
            ft   = 'col2'
            name = _resolve_col_name(blob, col_entries, i - col_off)
        else:
            ext  = '.anim'
            ft   = 'anim'
            name = _resolve_anim_name(blob, anim_block_base, i - anm_off)

        entries.append({
            'name':      name + ext,
            'offset':    cd_posn * DIR_SECTOR_SZ,
            'size':      cd_size * DIR_SECTOR_SZ,
            'index':     file_idx,
            'file_type': ft,
            'cd_sector': cd_posn,
        })
        file_idx += 1

    entries.sort(key=lambda e: e['offset'])
    for i, e in enumerate(entries):
        e['index'] = i

    return entries


def _build_mocap_dir(blob: bytes) -> list:
    """Parse MOCAPPS2.DIR / cuts.dir from the blob."""
    entries = []
    dir_foff = _rptr(blob, RI_MOCAP_DIR)
    if not dir_foff or dir_foff >= len(blob):
        return entries

    idx = 0
    pos = dir_foff
    while pos + 32 <= len(blob):
        chunk    = blob[pos:pos+32]
        sec_off  = struct.unpack_from('<I', chunk, 0)[0]
        sec_sz   = struct.unpack_from('<I', chunk, 4)[0]
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
        idx += 1
        pos += 32

    return entries


def open_dtz(path: str) -> dict:  #vers 4
    """Decompress and parse GAME.DTZ, returning fully-named streaming entries.

    Returns:
      {
        'version':       'DTZ_VCS' | 'DTZ_LCS' | 'DTZ_UNKNOWN',
        'game':          'VCS' | 'LCS' | 'UNKNOWN',
        'platform':      'PS2' | 'PSP',
        'entries':       [ {name, offset, size, index, file_type, cd_sector} ],
        'mocap_entries': [ {name, offset, size, index, file_type} ],
        'blob':          bytes | None,
        'error':         str | None,
      }
    """
    result = {
        'version':       'DTZ_UNKNOWN',
        'game':          'UNKNOWN',
        'platform':      'PS2',
        'entries':       [],
        'mocap_entries': [],
        'blob':          None,
        'error':         None,
    }

    blob = decompress_dtz(path)
    if blob is None:
        result['error'] = 'Failed to decompress GAME.DTZ'
        return result

    result['blob'] = blob

    if blob[:4] != GTAG_MAGIC:
        result['error'] = f'Unexpected magic {blob[:4].hex()} (expected GTAG)'
        return result

    lower            = os.path.basename(path).lower()
    result['platform'] = 'PSP' if 'psp' in lower else 'PS2'
    game             = 'VCS'   # TODO: distinguish LCS when sample available
    result['game']   = game
    result['version'] = f'DTZ_{game}'

    result['entries']       = _build_streaming_dir(blob, game)
    result['mocap_entries'] = _build_mocap_dir(blob)

    if not result['entries'] and not result['mocap_entries']:
        result['error'] = 'No entries found in GAME.DTZ'

    return result


__all__ = ['detect_dtz', 'decompress_dtz', 'open_dtz']
