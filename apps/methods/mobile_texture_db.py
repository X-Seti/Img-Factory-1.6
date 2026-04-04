#this belongs in apps/methods/mobile_texture_db.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - Mobile Texture Database Parser
"""
Mobile Texture Database Parser

Supports the SA/VC mobile texture database format used by iOS and Android ports.
This is NOT a RenderWare TXD - it is a separate quad-file format:

  name.txt   - texture list + properties (human-readable)
  name.x.toc - table of contents (offsets into .dat)
  name.x.dat - texture data (listings + raw pixel data)
  name.x.tmb - thumbnails

Where x = pvr (iOS) or etc (Android / some iOS).

Reference: https://gtamods.com/wiki/Mobile_textures_(SA/VC)
"""

import struct
import os
from typing import List, Dict, Optional, Tuple

## Methods list -
# hash_texture_name
# detect_mobile_db
# parse_txt_file
# parse_toc_file
# parse_dat_file
# load_mobile_texture_db
# get_encoding_name
# get_encoding_bpp
# decode_rle
# MobileTexture
# MobileTextureDB


# ── Encoding type constants ────────────────────────────────────────────────────
# Values confirmed from community research of GTA SA/VC mobile dat files.
# GTAMods wiki does not list the encoding_type values explicitly.

ENCODING_RGBA8888   = 0   # 32-bit RGBA uncompressed
ENCODING_PVRTC_4RGB = 1   # PVRTC 4bpp RGB  (iOS PowerVR)
ENCODING_PVRTC_4RGBA= 2   # PVRTC 4bpp RGBA (iOS PowerVR)
ENCODING_PVRTC_2RGB = 3   # PVRTC 2bpp RGB  (iOS PowerVR)
ENCODING_PVRTC_2RGBA= 4   # PVRTC 2bpp RGBA (iOS PowerVR)
ENCODING_ETC1       = 5   # ETC1 (Android / some iOS fallback)
# VC Android pvr.dat encoding IDs (different from SA)
ENCODING_VC_PVRTC2  = 0x8C01   # PVRTC 2bpp (VC Android gta3hi.pvr.dat)
ENCODING_VC_PVRTC2B = 0x8C02   # PVRTC 2bpp variant 2
ENCODING_RGB565     = 6   # 16-bit RGB 5:6:5
ENCODING_RGBA4444   = 7   # 16-bit RGBA 4:4:4:4
ENCODING_RGBA5551   = 8   # 16-bit RGBA 5:5:5:1

ENCODING_NAMES = {
    ENCODING_RGBA8888:    'RGBA8888',
    ENCODING_PVRTC_4RGB:  'PVRTC-4bpp-RGB',
    ENCODING_PVRTC_4RGBA: 'PVRTC-4bpp-RGBA',
    ENCODING_PVRTC_2RGB:  'PVRTC-2bpp-RGB',
    ENCODING_PVRTC_2RGBA: 'PVRTC-2bpp-RGBA',
    ENCODING_ETC1:        'ETC1',
    ENCODING_VC_PVRTC2:  'PVRTC-2bpp-VC',
    ENCODING_VC_PVRTC2B: 'PVRTC-2bpp-VC-B',
    ENCODING_RGB565:      'RGB565',
    ENCODING_RGBA4444:    'RGBA4444',
    ENCODING_RGBA5551:    'RGBA5551',
}

ENCODING_BPP = {
    ENCODING_RGBA8888:    32,
    ENCODING_PVRTC_4RGB:  4,
    ENCODING_PVRTC_4RGBA: 4,
    ENCODING_PVRTC_2RGB:  2,
    ENCODING_PVRTC_2RGBA: 2,
    ENCODING_ETC1:        4,   # ETC1 = 4 bits per pixel (4×4 blocks, 8 bytes each)
    ENCODING_RGB565:      16,
    ENCODING_RGBA4444:    16,
    ENCODING_RGBA5551:    16,
}

ENCODING_IS_PVRTC = {
    ENCODING_PVRTC_4RGB, ENCODING_PVRTC_4RGBA,
    ENCODING_PVRTC_2RGB, ENCODING_PVRTC_2RGBA,
}

# ── Platform detection ─────────────────────────────────────────────────────────
# File extension of the texture data files determines platform:
PLATFORM_IOS     = 'pvr'   # iOS   → PVRTC compression
PLATFORM_ANDROID = 'etc'   # Android → ETC1 compression


def get_encoding_name(encoding_type: int) -> str:
    """Return readable name for encoding_type value."""
    return ENCODING_NAMES.get(encoding_type, f'Unknown (0x{encoding_type:04X})')


def get_encoding_bpp(encoding_type: int) -> int:
    """Return bits per pixel for encoding_type, 0 if unknown."""
    return ENCODING_BPP.get(encoding_type, 0)


# ── Hash algorithm (from GTAMods wiki) ────────────────────────────────────────

def hash_texture_name(name: str) -> int:
    """
    Hash texture name to u16 for .dat verification.

    Algorithm (from GTAMods wiki):
    1. Start with u32 hash = 0
    2. For each byte: hash += (hash << 5) + byte  (all u32 wrapping)
    3. After loop: hash += hash >> 5
    4. Return low 16 bits
    """
    h = 0
    for byte in name.encode('ascii', errors='replace'):
        h = (h + ((h << 5) & 0xFFFFFFFF) + byte) & 0xFFFFFFFF
    h = (h + (h >> 5)) & 0xFFFFFFFF
    return h & 0xFFFF


# ── RLE decompression (from GTAMods wiki) ─────────────────────────────────────

def decode_rle(data: bytes, segment_size: int, indicator: int) -> bytes:
    """
    Decode mobile texture RLE compression.

    Args:
        data:         Compressed bytes.
        segment_size: Size of each segment in bytes (must be >= 1).
        indicator:    If a group starts with this byte, the next byte is a
                      repeat count and the following segment_size bytes are
                      repeated that many times.

    Returns:
        Decompressed bytes.
    """
    # Sanity checks to prevent runaway loops on corrupt/wrong data
    segment_size = max(1, segment_size)
    max_out = max(len(data) * 256, 64 * 1024 * 1024)  # cap at 64 MB

    out = bytearray()
    i = 0
    n = len(data)

    while i < n:
        if len(out) >= max_out:
            break  # safety cap — corrupt data guard
        b = data[i]; i += 1
        if b == indicator:
            if i + 1 + segment_size > n:
                break
            count = data[i]; i += 1
            if count == 0:
                continue  # skip zero-repeat runs
            segment = data[i:i + segment_size]; i += segment_size
            for _ in range(count):
                out.extend(segment)
                if len(out) >= max_out:
                    break
        else:
            if i + segment_size - 1 > n:
                break
            # First byte already read; prepend it to the segment
            segment = bytes([b]) + data[i:i + segment_size - 1]
            i += segment_size - 1
            out.extend(segment)

    return bytes(out)


# ── Data structures ────────────────────────────────────────────────────────────

class MobileTexture:
    """Single texture entry from a mobile texture database."""

    def __init__(self):
        self.name: str = ''
        self.hash: int = 0
        self.encoding_type: int = 0
        self.width: int = 0
        self.height: int = 0
        self.has_mipmaps: bool = False
        self.mip_count: int = 1
        self.compressed_size: int = 0
        self.rle_indicator: int = 0      # 0 = no RLE
        self.data_offset: int = 0        # byte offset into .dat file
        self.raw_data: bytes = b''       # compressed bytes as stored in .dat
        self.pixel_data: bytes = b''     # decoded pixel bytes (decompressed)

        # Properties from .txt file
        self.txt_props: Dict[str, str] = {}
        self.is_affiliate: bool = False  # affiliate=... redirect entry

    @property
    def encoding_name(self) -> str:
        return get_encoding_name(self.encoding_type)

    @property
    def is_pvrtc(self) -> bool:
        return self.encoding_type in ENCODING_IS_PVRTC

    @property
    def is_etc1(self) -> bool:
        return self.encoding_type == ENCODING_ETC1

    @property
    def bpp(self) -> int:
        return get_encoding_bpp(self.encoding_type)

    def __repr__(self):
        return (f'<MobileTexture {self.name!r} '
                f'{self.width}x{self.height} '
                f'{self.encoding_name} '
                f'mipmaps={self.has_mipmaps}>')


class MobileTextureDB:
    """
    Parsed mobile texture database (quad-file set: .txt + .toc + .dat + .tmb).
    """

    def __init__(self):
        self.name: str = ''               # e.g. 'gta3'
        self.platform: str = ''           # 'pvr' or 'etc'
        self.textures: List[MobileTexture] = []
        self.txt_path: str = ''
        self.toc_path: str = ''
        self.dat_path: str = ''
        self.tmb_path: str = ''
        self.dat_size: int = 0            # size of .dat as stored in .toc header
        self.errors: List[str] = []

    @property
    def texture_count(self) -> int:
        return len(self.textures)

    @property
    def is_ios(self) -> bool:
        return self.platform == PLATFORM_IOS

    @property
    def is_android(self) -> bool:
        return self.platform == PLATFORM_ANDROID

    def get_by_name(self, name: str) -> Optional[MobileTexture]:
        for t in self.textures:
            if t.name == name:
                return t
        return None


# ── Parsers ────────────────────────────────────────────────────────────────────

def parse_txt_file(txt_path: str) -> Tuple[Dict, List[Dict]]:
    """
    Parse the .txt property file.

    Returns:
        (category_props, list_of_texture_prop_dicts)
        Each texture dict has at minimum 'name' key.
    """
    category_props: Dict[str, str] = {}
    textures: List[Dict] = []

    if not os.path.isfile(txt_path):
        return category_props, textures

    with open(txt_path, 'r', errors='replace') as f:
        lines = f.readlines()

    for raw_line in lines:
        line = raw_line.strip()
        if not line or line.startswith('#'):
            continue

        # Parse key=value pairs from this line
        props: Dict[str, str] = {}
        name = None
        is_affiliate = False

        # Texture lines begin with a quoted name: "texname"
        if line.startswith('"'):
            end = line.find('"', 1)
            if end != -1:
                name = line[1:end]
                rest = line[end + 1:].strip()
            else:
                continue
        else:
            rest = line

        # Parse remaining key=value pairs
        for token in rest.split():
            if '=' in token:
                k, _, v = token.partition('=')
                # Handle quoted values (e.g. "affiliate=something")
                v = v.strip('"')
                k = k.strip('"')
                props[k] = v
                if k == 'affiliate':
                    is_affiliate = True
            elif token.startswith('"') and token.endswith('"'):
                # bare quoted token is a texture name on a category line
                pass

        if 'cat' in props:
            # Category / database info line
            category_props.update(props)
            if name:
                category_props['_name'] = name
        elif name is not None:
            entry = {'name': name, 'is_affiliate': is_affiliate}
            entry.update(props)
            textures.append(entry)
        elif props:
            # Unnamed property line — treat as category
            category_props.update(props)

    return category_props, textures


def parse_toc_file(toc_path: str, entry_count: int) -> Tuple[int, List[int]]:
    """
    Parse the .toc offset file.

    Supports two TOC layouts:
      With-txt (SA iOS / VC iOS):
        dat_size(4) + N×offset(4) where N == entry_count, offsets -1 = affiliate
      No-txt (SA Android / VC Android):
        dat_size(4) + pad(4) + M×offset(4) where M = (file_size-8)/4
        First texture is implicitly at offset 0 (not in the array).

    Args:
        toc_path:    Path to .toc file.
        entry_count: Number of entries from .txt (0 = no-txt format).

    Returns:
        (stated_dat_size, list_of_offsets)
        Offsets of -1 (0xFFFFFFFF) = affiliate/gap entries.
    """
    offsets: List[int] = []
    dat_size = 0

    if not os.path.isfile(toc_path):
        return dat_size, offsets

    with open(toc_path, 'rb') as f:
        data = f.read()

    if len(data) < 4:
        return dat_size, offsets

    dat_size = struct.unpack_from('<I', data, 0)[0]

    if entry_count == 0:
        # No-txt format: dat_size(4) + pad(4) + flat u32 offsets
        # First texture is always at offset 0 in the DAT (implicit).
        if len(data) < 8:
            return dat_size, offsets
        n = (len(data) - 8) // 4
        raw = struct.unpack_from(f'<{n}I', data, 8)
        # Prepend 0 (implicit first entry) then all TOC entries
        # Sentinel 0xFFFFFFFF = affiliate gap → keep as -1
        offsets = [0] + [int(o) if o != 0xFFFFFFFF else -1 for o in raw]
    else:
        # With-txt format: dat_size(4) + entry_count × u32 offsets
        n = min(entry_count, (len(data) - 4) // 4)
        raw = struct.unpack_from(f'<{n}I', data, 4)
        offsets = [int(o) if o != 0xFFFFFFFF else -1 for o in raw]

    return dat_size, offsets


def parse_dat_file(dat_path: str, txt_entries: List[Dict],
                   offsets: Optional[List[int]] = None,
                   load_pixel_data: bool = True) -> List[MobileTexture]:
    """
    Parse the .dat texture data file.

    Args:
        dat_path:         Path to .dat file.
        txt_entries:      Texture dicts from parse_txt_file.
        offsets:          Offset list from parse_toc_file (optional — will scan
                          sequentially if not provided or mismatched).
        load_pixel_data:  If True, decompress RLE and store in texture.pixel_data.

    Returns:
        List of MobileTexture objects.
    """
    textures: List[MobileTexture] = []

    if not os.path.isfile(dat_path):
        return textures

    with open(dat_path, 'rb') as f:
        dat = f.read()

    # If no txt_entries but offsets exist (VC Android no-txt format),
    # create synthetic entry list so we can iterate over TOC offsets directly.
    if not txt_entries and offsets:
        txt_entries = [{'name': f'texture_{i}', 'is_affiliate': False}
                       for i in range(len(offsets))]

    use_offsets = (offsets is not None and len(offsets) == len(txt_entries))
    pos = 0
    HEADER_SIZE = 16  # u16 hash + u16 enc + u16 w + u16 h_mask + u32 csz + i32 rle

    for idx, entry_props in enumerate(txt_entries):
        tex = MobileTexture()
        tex.name = entry_props.get('name', f'texture_{idx}')
        tex.txt_props = dict(entry_props)
        tex.is_affiliate = entry_props.get('is_affiliate', False)

        if tex.is_affiliate:
            textures.append(tex)
            continue

        # Find position in .dat
        if use_offsets and idx < len(offsets):
            off = offsets[idx]
            if off == -1:
                # Affiliate in .toc - skip
                tex.is_affiliate = True
                textures.append(tex)
                continue
            pos = off
        # else: use sequential pos

        if pos + HEADER_SIZE > len(dat):
            break

        # --- Parse 12-byte listing header ---
        (name_hash, encoding_type,
         width, height_mask,
         comp_size, rle_indicator) = struct.unpack_from('<HHHHIi', dat, pos)

        tex.hash = name_hash
        tex.encoding_type = encoding_type
        tex.width = width
        tex.height = height_mask & 0x7FFF
        tex.has_mipmaps = not bool(height_mask & 0x8000)
        tex.compressed_size = comp_size
        tex.rle_indicator = rle_indicator  # 0 = no RLE; treated as signed but wiki says u32
        tex.data_offset = pos + HEADER_SIZE

        # Count mipmaps
        if tex.has_mipmaps and tex.width > 0 and tex.height > 0:
            w, h = tex.width, tex.height
            count = 0
            while w >= 1 and h >= 1:
                count += 1
                if w == 1 and h == 1:
                    break
                w = max(1, w >> 1)
                h = max(1, h >> 1)
            tex.mip_count = count
        else:
            tex.mip_count = 1

        # When TOC offsets available, derive true comp_size from offset difference.
        # VC pvr.dat stores garbage in the csz header field — use TOC instead.
        # Skip -1 (affiliate/gap) entries to find the real next valid offset.
        if use_offsets:
            for _nxt_i in range(idx + 1, len(offsets)):
                if offsets[_nxt_i] != -1:
                    toc_comp_size = offsets[_nxt_i] - pos - HEADER_SIZE
                    if toc_comp_size > 0:
                        comp_size = toc_comp_size
                        tex.compressed_size = comp_size
                    break

        # Sanity check comp_size before reading — corrupt/wrong-format guard
        MAX_TEX_BYTES = 16 * 1024 * 1024  # 16 MB hard cap per texture
        if comp_size == 0 or comp_size > MAX_TEX_BYTES:
            # comp_size is unrecoverable — append affiliate placeholder and skip
            tex.is_affiliate = True
            textures.append(tex)
            continue

        # Read raw compressed bytes
        data_start = pos + HEADER_SIZE
        data_end = data_start + comp_size
        if data_end <= len(dat):
            tex.raw_data = dat[data_start:data_end]
        else:
            tex.raw_data = dat[data_start:]  # truncated

        # Decode RLE if present
        if load_pixel_data and tex.raw_data:
            if rle_indicator != 0:
                seg_sz = max(1, get_encoding_bpp(encoding_type) // 8)
                if seg_sz < 1:
                    seg_sz = 1
                tex.pixel_data = decode_rle(tex.raw_data, seg_sz,
                                            rle_indicator & 0xFF)
            else:
                tex.pixel_data = tex.raw_data

        textures.append(tex)
        pos = data_end  # advance for sequential scan

    return textures


# ── Top-level loader ───────────────────────────────────────────────────────────

def detect_mobile_db(path: str) -> Optional[Tuple[str, str, str]]:
    """
    Detect if a path belongs to a mobile texture database.

    Accepts (with .txt sidecar — SA iOS / VC iOS format):
      - name.txt, name.pvr.dat, name.pvr.toc, name.pvr.tmb

    Accepts (without .txt — SA Android / VC Android format):
      - name.dxt.toc / name.dxt.dat / name.dxt.tmb
      - name.etc.toc / name.etc.dat / name.etc.tmb
      - name.pvr.toc / name.pvr.dat / name.pvr.tmb  (VC)

    Returns:
        (base_name, platform_ext, folder_path)  or  None if not a mobile DB.
        e.g. ('gta3', 'pvr', '/path/to/texdb/')
    """
    if not path:
        return None

    folder = os.path.dirname(path)
    fname  = os.path.basename(path)
    name_lower = fname.lower()

    # Quick reject: must have a known platform extension somewhere in the name
    KNOWN_PLATFORMS = (PLATFORM_IOS, PLATFORM_ANDROID, 'dxt', 'etc')

    # Case 1: .txt file → look for companion .dat
    if name_lower.endswith('.txt'):
        db_name = os.path.splitext(fname)[0]
        for platform in (PLATFORM_IOS, PLATFORM_ANDROID, 'dxt', 'etc'):
            dat = os.path.join(folder, f'{db_name}.{platform}.dat')
            if os.path.isfile(dat):
                return (db_name, platform, folder)
        return None

    # Case 2: .dat / .toc / .tmb with embedded platform token
    # Filename looks like: gta3hi.pvr.dat  /  txd.dxt.toc  /  hud.etc.tmb
    for platform in (PLATFORM_IOS, PLATFORM_ANDROID, 'dxt', 'etc'):
        token = f'.{platform}.'
        if token in name_lower:
            db_name = fname[:name_lower.index(token)]
            # With .txt sidecar (preferred)
            txt = os.path.join(folder, f'{db_name}.txt')
            if os.path.isfile(txt):
                return (db_name, platform, folder)
            # Without .txt — Android/no-txt format; verify .dat exists
            dat = os.path.join(folder, f'{db_name}.{platform}.dat')
            if os.path.isfile(dat):
                return (db_name, platform, folder)

    return None


def load_mobile_texture_db(path: str,
                           load_pixel_data: bool = True) -> Optional[MobileTextureDB]:
    """
    Load a mobile texture database from any one of its four files.

    Args:
        path:             Path to .txt, .dat, .toc, or .tmb file.
        load_pixel_data:  Whether to decode RLE and populate pixel_data.

    Returns:
        MobileTextureDB on success, None if not recognised.
    """
    detected = detect_mobile_db(path)
    if not detected:
        return None

    db_name, platform, folder = detected

    db = MobileTextureDB()
    db.name = db_name
    db.platform = platform
    db.txt_path = os.path.join(folder, f'{db_name}.txt')
    db.toc_path = os.path.join(folder, f'{db_name}.{platform}.toc')
    db.dat_path = os.path.join(folder, f'{db_name}.{platform}.dat')
    db.tmb_path = os.path.join(folder, f'{db_name}.{platform}.tmb')

    # 1. Parse .txt (optional — not present in Android / no-txt format)
    cat_props, txt_entries = parse_txt_file(db.txt_path)

    has_txt = bool(txt_entries)

    # 2. Parse .toc
    # entry_count=0 triggers no-txt mode in parse_toc_file
    offsets: Optional[List[int]] = None
    if os.path.isfile(db.toc_path):
        dat_size, offsets = parse_toc_file(
            db.toc_path,
            entry_count=len(txt_entries) if has_txt else 0
        )
        db.dat_size = dat_size

        # Validate against actual dat size
        if os.path.isfile(db.dat_path):
            actual_size = os.path.getsize(db.dat_path)
            if dat_size and dat_size != actual_size:
                db.errors.append(
                    f'.toc states dat_size={dat_size} but actual={actual_size}; '
                    f'using sequential scan'
                )
                if has_txt:
                    offsets = None  # only discard offsets if we have .txt fallback

    # For no-txt format: synthesise placeholder txt_entries from offsets
    if not has_txt and offsets:
        txt_entries = [{'name': f'texture_{i}'} for i in range(len(offsets))]

    if not txt_entries:
        db.errors.append(
            f'No texture entries found: missing {db.txt_path} '
            f'and could not derive entries from .toc'
        )
        return db

    # 3. Parse .dat (only if we have entries to match — no-txt formats
    #    use TOC offsets alone; txt_entries may be [] for VC Android)
    if os.path.isfile(db.dat_path):
        if txt_entries or offsets:
            db.textures = parse_dat_file(db.dat_path, txt_entries, offsets,
                                         load_pixel_data)
        else:
            db.errors.append(
                'No texture entries (.txt missing and TOC has no usable offsets) '
                f'for {os.path.basename(db.dat_path)} — cannot parse.'
            )
    else:
        db.errors.append(f'Missing .dat file: {db.dat_path}')

    return db


# ── Summary helpers ────────────────────────────────────────────────────────────

def describe_mobile_db(db: MobileTextureDB) -> str:
    """Return a one-line summary string for the database."""
    platform_str = 'iOS (PVRTC)' if db.is_ios else 'Android (ETC1)' if db.is_android else db.platform
    real = [t for t in db.textures if not t.is_affiliate]
    affiliates = len(db.textures) - len(real)
    enc_counts: Dict[str, int] = {}
    for t in real:
        enc_counts[t.encoding_name] = enc_counts.get(t.encoding_name, 0) + 1
    enc_str = ', '.join(f'{k}:{v}' for k, v in sorted(enc_counts.items()))
    parts = [
        f'{db.name}.{db.platform}',
        platform_str,
        f'{len(real)} textures',
    ]
    if affiliates:
        parts.append(f'{affiliates} affiliates')
    if enc_str:
        parts.append(enc_str)
    return ' | '.join(parts)
