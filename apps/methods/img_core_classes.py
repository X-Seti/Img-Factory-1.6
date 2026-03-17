#this belongs in methods.img_core_classes.py - Version: 11
# X-Seti - November29 2025 - IMG Factory 1.5 - IMG Core Classes with Fixed RW Version Detection

"""
IMG Core Classes
"""

import os
import struct
import json
import shutil
from enum import Enum
from typing import List, Dict, Optional, Any, Union, BinaryIO
from pathlib import Path
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QComboBox, QLineEdit, QGroupBox, QLabel)
from PyQt6.QtCore import pyqtSignal, Qt

# Import existing RW version functions - KEPT ALL ORIGINAL IMPORTS
from apps.methods.rw_versions import get_rw_version_name, parse_rw_version, get_model_format_version
from apps.debug.debug_functions import img_debugger
from apps.methods.populate_img_table import DragSelectTableWidget

def _find_companion(base_path: str, new_ext: str) -> str:
    # Find companion file case-insensitively.
    # Also handles LCS PS2: GTA3PS2.IMG <-> GTA3.DIR cross-stem pairs.
    stem = base_path[:-4]
    for ext in (new_ext.lower(), new_ext.upper()):
        p = stem + ext
        if os.path.exists(p):
            return p
    try:
        parent  = os.path.dirname(base_path)
        target  = (os.path.basename(stem) + new_ext).lower()
        entries = os.listdir(parent)
        for name in entries:
            if name.lower() == target:
                return os.path.join(parent, name)
        # Cross-stem: GTA3PS2.IMG <-> GTA3.DIR etc.
        bn  = os.path.basename(base_path).lower()
        exl = new_ext.lower()
        CROSS = {
            ('gta3ps2.img', '.dir'): 'gta3.dir',
            ('gta3.img',    '.dir'): 'gta3ps2.dir',
            ('gta3ps2.dir', '.img'): 'gta3.img',
            ('gta3.dir',    '.img'): 'gta3ps2.img',
        }
        alt = CROSS.get((bn, exl))
        if alt:
            for name in entries:
                if name.lower() == alt:
                    return os.path.join(parent, name)
    except Exception:
        pass
    return ''



##Methods list -
# create_entries_table_panel
# create_img_file
# detect_img_version
# format_file_size
# integrate_filtering
# populate_table_with_sample_data
# rebuild_img_file

##Classes -
# CompressionType
# FileType
# FilterPanel
# IMGEntriesTable
# IMGEntry
# IMGFile
# IMGFileInfoPanel
# IMGPlatform
# IMGVersion
# Platform
# RecentFilesManager
# TabFilterWidget
# ValidationResult

def _is_v3_encrypted(first16: bytes) -> bool:
    """Return True if first 16 bytes decrypt (AES-256 ECB, 16 rounds) to a valid V3 header start."""
    try:
        from Crypto.Cipher import AES
        import struct as _struct
        GTAIV_KEY = bytes([
            0x1a,0xb5,0x6f,0xed,0x7e,0xc3,0xff,0x01,
            0x22,0x7b,0x69,0x15,0x33,0x97,0x5d,0xce,
            0x47,0xd7,0x69,0x65,0x3f,0xf7,0x75,0x42,
            0x6a,0x96,0xcd,0x6d,0x53,0x07,0x56,0x5d,
        ])
        data = first16
        for _ in range(16):
            data = AES.new(GTAIV_KEY, AES.MODE_ECB).decrypt(data)
        # After decryption, bytes 4-7 should be Version = 3
        ver = _struct.unpack('<I', data[4:8])[0]
        return ver == 3
    except Exception:
        return False


def _detect_v1_or_v1_5(dir_path: str, img_path: str) -> str:
    """Return 'V1_5' if extended (>2GB or long names), else 'V1'."""
    try:
        import os as _os
        if _os.path.getsize(img_path) > 2 * 1024 * 1024 * 1024:
            return 'V1_5'
        with open(dir_path, 'rb') as f:
            while True:
                entry = f.read(32)
                if len(entry) < 32:
                    break
                if b'\x00' not in entry[8:32]:
                    return 'V1_5'
        return 'V1'
    except Exception:
        return 'V1'


class IMGVersion(Enum):
    """IMG Archive Version Types"""
    VERSION_1     = 1   # DIR/IMG pair (GTA3, VC) - 2GB limit, short filenames
    VERSION_1_5   = 15  # DIR/IMG pair extended - up to 4GB, long filenames
    VERSION_SOL   = 25  # DIR/IMG pair (SOL)
    VERSION_2     = 2   # Single IMG file (SA) - magic VER2
    VERSION_3     = 3   # Single IMG file (GTA IV) - magic 0xA94E2A52, unencrypted
    VERSION_3_ENC = 30  # Single IMG file (GTA IV) - AES-256 ECB encrypted header
    VERSION_PS2_VCS = 40  # PS2 VCS embedded-dir IMG, 512-byte sectors, type-code entries
    VERSION_PS2_LVZ = 41  # PS2 VCS zlib DLRW streaming archive (.lvz)
    VERSION_PS2_V1  = 42  # PS2/iOS/Android GTA3/VC/Bully - 12-byte entries, no names
    VERSION_ANPK    = 43  # PSP ANPK animation package - named DGAN clip blocks
    VERSION_BULLY   = 44  # Bully PS2 named-entry archive - 64-byte name-only directory
    VERSION_HXD     = 45  # Bully HXD/MXD/AGR bone+animation data - float header + path
    VERSION_IOS_LVZ = 49  # iOS LCS/VCS LVZ — two-level DLRW tree (no 0xAAAAAAAA sentinels)
    VERSION_DTZ_VCS = 46  # GAME.DTZ VCS PS2/PSP — zlib blob with Relocatable Chunk header
    VERSION_DTZ_LCS = 47  # GAME.DTZ LCS PS2/PSP — zlib blob with Relocatable Chunk header
    VERSION_IRX     = 48  # PS2 IOP MIPS ELF module (.irx) — read-only system file
    VERSION_1_IOS   = 54  # iOS GTA3/VC (*_pvr.img) - 12-byte entries, 512-byte sectors, no names
    VERSION_XBOX        = 50  # Xbox GTA3/VC - DIR+IMG pair, LZO-compressed entries, 2048-byte sectors
    VERSION_SA_ANDROID  = 51  # Android SA - VER2 header, 2048-byte sectors, mobile texture DB
    VERSION_LCS_ANDROID = 52  # Android LCS - VER2 header, TXD version 0x1005FFFF embedded in IMG
    VERSION_LCS_IOS        = 53  # iOS LCS - 12-byte entries, 512-byte sectors, *_pvr.img suffix
    VERSION_STREAMING_SEG  = 60  # Raw streaming segment (LCS/VCS iOS/PSP) - no internal directory
    UNKNOWN       = 0

class IMGPlatform(Enum):
    """Platform types for IMG files - MOVED HERE TO ELIMINATE CIRCULAR IMPORT"""
    PC = "pc"
    PS2 = "ps2" 
    XBOX = "xbox"
    PSP = "psp"
    ANDROID = "android"
    IOS = "ios"
    UNKNOWN = "unknown"

class FileType(Enum):
    """File types found in IMG archives"""
    DFF = "dff"         # 3D Models
    TXD = "txd"         # Texture Dictionary
    COL = "col"         # Collision Data
    IFP = "ifp"         # Animation Data
    IPL = "ipl"         # Item Placement
    DAT = "dat"         # Data files
    WAV = "wav"         # Audio files
    STRM = "strm"       # PS2/PSP LVZ streaming cell (raw geometry)
    CHK  = "chk"        # LCS PS2 texture archive (same format as XTX)
    UNKNOWN = "unknown"
        # Aliases for backwards compatibility
    dff = DFF           # Lowercase alias
    txd = TXD           # Lowercase alias
    col = COL           # Lowercase alias
    ifp = IFP           # Lowercase alias
    ipl = IPL           # Lowercase alias
    dat = DAT           # Lowercase alias
    wav = WAV           # Lowercase alias
    strm = STRM         # Lowercase alias
    chk  = CHK          # Lowercase alias
    unknown = UNKNOWN   # Lowercase alias

    # Legacy alias
    MODEL = DFF         # Old name alias

class Platform(Enum):
    """Platform types for IMG files"""
    PC = 0
    XBOX = 1
    PS2 = 2
    MOBILE = 3

class CompressionType(Enum):
    """Compression types"""
    NONE        = "none"
    ZLIB        = "zlib"
    LZO         = "lzo"
    FASTMAN92   = "fastman92"
    UNKNOWN     = "unknown"

class EncryptionType(Enum):
    """Encryption types"""
    NONE        = "none"
    FASTMAN92   = "fastman92"

class RecentFilesManager:
    """Manage recently opened files"""
    def __init__(self, max_files: int = 10): #vers 1
        self.max_files = max_files
        self.recent_files: List[str] = []
        self.settings_file = "recent_files.json"
        self._load_recent_files()
    
    def _load_recent_files(self): #vers 1
        """Load recent files from settings"""
        try:
            if os.path.exists(self.settings_file):
                with open(self.settings_file, 'r') as f:
                    data = json.load(f)
                    self.recent_files = data.get('recent_files', [])
        except Exception:
            self.recent_files = []
    
    def _save_recent_files(self): #vers 1
        """Save recent files to settings"""
        try:
            data = {'recent_files': self.recent_files}
            with open(self.settings_file, 'w') as f:
                json.dump(data, f, indent=2)
        except Exception:
            pass
    
    def add_file(self, file_path): #vers 2
        """Add file to recent files list"""
        if file_path in self.recent_files:
            self.recent_files.remove(file_path)
        
        self.recent_files.insert(0, file_path)
        
        # Keep only max_files entries
        if len(self.recent_files) > self.max_files:
            self.recent_files = self.recent_files[:self.max_files]
        
        self._save_recent_files()
    
    def get_recent_files(self): #vers 1
        """Get list of recent files"""
        # Filter out files that no longer exist
        existing_files = [f for f in self.recent_files if os.path.exists(f)]
        if len(existing_files) != len(self.recent_files):
            self.recent_files = existing_files
            self._save_recent_files()
        return self.recent_files

class ValidationResult:
    """Results from entry validation"""
    def __init__(self): #vers 1
        self.is_valid: bool = True
        self.errors: List[str] = []
        self.warnings: List[str] = []
    
    def add_error(self, message: str): #vers 1
        self.errors.append(message)
        self.is_valid = False
    
    def add_warning(self, message: str): #vers 1
        self.warnings.append(message)

_KNOWN_GTA_EXTENSIONS = {
    'dff', 'txd', 'col', 'ifp', 'ipl', 'dat', 'wav', 'ide', 'zon',
    'ped', 'grp', 'cut', 'cnf', 'img', 'dir', 'scm', 'mp3', 'ogg',
    'fxp', 'bmp', 'png', 'jpg', 'spl', 'rrr', 'rdb', 'rsc',
}

_XBOX_LZO_MAGIC = 0x67A3A1CE  # little-endian master header magic for Xbox LZO streams

def _lzo1x_decompress(data: bytes, expected_size: int = 0) -> bytes:
    """Pure-Python LZO1X-1 decompressor (lzo1x_decompress_safe compatible).

    Based on the canonical LZO1X algorithm. Handles the compressed block
    data as produced by lzo1x-999 (used in GTA Xbox TXD/DFF entries).
    Returns decompressed bytes. Raises ValueError on malformed input.
    """
    out = bytearray(expected_size) if expected_size else bytearray()
    if expected_size:
        out = bytearray()

    src = 0
    n = len(data)

    def peek():
        return data[src] if src < n else 0

    def get():
        nonlocal src
        if src >= n:
            raise ValueError("LZO: truncated input")
        b = data[src]; src += 1
        return b

    def copy_match(dist, length):
        start = len(out) - dist
        if start < 0:
            raise ValueError(f"LZO: invalid match distance {dist}")
        for _ in range(length):
            out.append(out[start])
            start += 1

    # Decode first instruction
    t = get()
    if t >= 18:
        # Initial literal run
        cnt = t - 17
        for _ in range(cnt):
            out.append(get())
        t = get()
    elif t >= 16:
        pass  # fall through to main loop
    else:
        # t < 16 on first byte: short literal
        pass

    # Main decode loop
    while True:
        if t < 16:
            # Short literal + end-of-block
            if t == 0:
                cnt = 15
                while peek() == 0:
                    cnt += 255; get()
                cnt += get()
            else:
                cnt = t
            for _ in range(cnt + 3):
                out.append(get())
            t = get()
            if t >= 16:
                continue
            # EOS short match
            dist = 0x801 + (get() << 2) + (t >> 2)
            copy_match(dist, 3)
            t &= 3
            if t:
                for _ in range(t):
                    out.append(get())
                t = get()
            continue

        if t >= 64:
            # Short match (2-8 bytes, dist 1-8*256)
            length = (t >> 5) - 1 + 2
            dist = ((t >> 2) & 7) * 256 + get() + 1
            copy_match(dist, length)

        elif t >= 32:
            # Medium match
            length = t & 31
            if length == 0:
                length = 31
                while peek() == 0:
                    length += 255; get()
                length += get()
            length += 2
            b1 = get(); b2 = get()
            dist = (b1 >> 2) + (b2 << 6) + 1
            copy_match(dist, length)

        else:  # 16 <= t < 32
            # Long match
            length = t & 7
            if length == 0:
                length = 7
                while peek() == 0:
                    length += 255; get()
                length += get()
            length += 2
            b1 = get(); b2 = get()
            dist = (b1 >> 2) + (b2 << 6) + 0x4001
            if t & 8:
                dist += 0x4000
            copy_match(dist, length)

        # Trailing literal nibble
        t &= 3
        if t == 0:
            t = get()
            continue
        for _ in range(t):
            out.append(get())
        t = get()
        if src >= n:
            break

    return bytes(out)


def _xbox_lzo_peek_header(data: bytes, want: int = 64) -> bytes:
    """Fast extraction of the RW chunk header from an Xbox LZO stream.

    The RW header (12 bytes: type + size + version) is always encoded as an
    initial literal run in the very first LZO instruction byte.  We decode
    only that literal run — no back-references needed — and return the result.

    This is O(want) instead of O(block_size) and avoids reading megabytes of
    compressed data just to get a 12-byte header.

    Falls back to full decompression if the first byte is not a literal-run
    instruction (i.e. the data is structured differently than expected).
    """
    # Layout: master_header(12) + block_header(12) + lzo_payload(...)
    # lzo_payload starts at offset 24
    PAYLOAD_START = 24
    if len(data) < PAYLOAD_START + 1:
        return b''

    out = bytearray()
    pos = PAYLOAD_START

    first = data[pos]
    if first >= 18:
        # Initial literal run: copy (first - 17) bytes verbatim
        n_lit = first - 17
        pos += 1
        lit_end = pos + n_lit
        if lit_end <= len(data):
            out.extend(data[pos:lit_end])
            pos = lit_end
        else:
            out.extend(data[pos:])
        # If we have enough, return early — no back-reference decoding needed
        if len(out) >= want:
            return bytes(out)
        # Continue decoding the next instruction (one more literal or match)
        if pos < len(data):
            nxt = data[pos]
            if nxt >= 18:
                # Another literal run
                n2 = nxt - 17
                pos += 1
                out.extend(data[pos:pos + n2])
    elif first == 0:
        # Long literal: read zero-run then count
        pos += 1
        n_lit = 15
        while pos < len(data) and data[pos] == 0:
            n_lit += 255; pos += 1
        if pos < len(data):
            n_lit += data[pos]; pos += 1
        n_lit += 3
        out.extend(data[pos:pos + n_lit])
    # else: match instruction on first byte — fall back to full decompress
    # (uncommon for well-formed RW files)

    if len(out) >= want:
        return bytes(out[:want])

    # Fallback: full decompress (slow path, should rarely be needed)
    try:
        full = _xbox_lzo_decompress_entry(data)
        return full[:want]
    except Exception:
        return bytes(out)


def _xbox_lzo_decompress_entry(data: bytes) -> bytes:
    """Decompress an Xbox LZO-compressed entry (GTA III/VC Xbox).

    Stream layout:
      Master header (12 bytes):
        magic     u32  0x67A3A1CE (LE)
        checksum  u32
        total_sz  u32  total size of all block headers+data combined
      Then one or more blocks:
        always    u32  always 4
        decomp_sz u32  decompressed size of this block
        comp_sz   u32  size of the LZO block data following

      NOTE: In GTA Xbox, comp_sz often equals decomp_sz — this does NOT mean
      the block is stored uncompressed. The block data is always LZO1X
      compressed; comp_sz is simply the stored (compressed) byte count.
      comp_sz == 0 signals end of stream.
    """
    if len(data) < 12:
        return data
    magic = struct.unpack_from('<I', data, 0)[0]
    if magic != _XBOX_LZO_MAGIC:
        return data

    try:
        import lzo as _lzo
        _has_lzo = True
    except ImportError:
        _has_lzo = False

    pos = 12  # skip master header
    out = bytearray()
    while pos + 12 <= len(data):
        _always, decomp_sz, comp_sz = struct.unpack_from('<III', data, pos)
        pos += 12
        if comp_sz == 0:
            break
        block = data[pos:pos + comp_sz]
        pos += comp_sz

        # Always LZO compressed (comp_sz == decomp_sz is normal for GTA Xbox)
        if _has_lzo:
            try:
                out.extend(_lzo.decompress(block, False, decomp_sz))
                continue
            except Exception:
                pass
        try:
            out.extend(_lzo1x_decompress(block, decomp_sz))
        except Exception:
            out.extend(block)  # best-effort on decompressor failure

    return bytes(out)


def _is_xbox_lzo(data: bytes) -> bool:
    """Return True if data starts with the Xbox LZO master header magic."""
    return len(data) >= 4 and struct.unpack_from('<I', data, 0)[0] == _XBOX_LZO_MAGIC


def _strip_xbox_lzo_header(data: bytes) -> bytes:
    """If data starts with the Xbox LZO master header (magic 0x67A3A1CE),
    return data with the 12-byte master header stripped so callers see
    the first compressed block header.  Otherwise return data unchanged."""
    if len(data) >= 4 and struct.unpack_from('<I', data, 0)[0] == _XBOX_LZO_MAGIC:
        return data[12:]   # skip magic(4) + checksum(4) + total_size(4)
    return data


def _scan_rw_version(data: bytes):
    """Scan first 64 bytes of a file for a valid RW version.

    RW chunk layout: type(4) + size(4) + version(4).
    Handles plain RW files, Xbox LZO-prefixed files (12-byte master header
    before the first compressed block), and other 4/8-byte prefix variants.
    Returns (version_int, byte_offset) or (None, -1).
    """
    # If this is an Xbox LZO stream, peek at the first block payload.
    # Block header: always(4) + decompressed_size(4) + compressed_size(4) = 12 bytes,
    # then compressed data follows.  We can't decompress here cheaply, but the RW
    # version still lives at type+size+version layout, so try scanning the raw
    # compressed block too — if the version wasn't scrambled by the compressor it
    # may still be visible (Xbox LZO is block-level; small blocks often match).
    # More robustly: mark as LZO and let _read_header_data decompress if needed.
    is_lzo = len(data) >= 4 and struct.unpack_from('<I', data, 0)[0] == _XBOX_LZO_MAGIC
    if is_lzo:
        # Skip past master header (12) + block header (12) to reach payload bytes
        payload_start = 24
        if len(data) >= payload_start + 12:
            data = data[payload_start:]

    # Check standard offset first (bytes 8-11), then prefixed variants
    for base_offset in (0, 4, 8):
        off = base_offset + 8
        if len(data) >= off + 4:
            v = struct.unpack_from('<I', data, off)[0]
            if _is_valid_rw_version(v):
                return v, off
    return None, -1


class IMGEntry:
    """Represents a single file entry within an IMG archive - FIXED WITH RW VERSION DETECTION"""
    
    def __init__(self): #vers 4
        self.name: str = ""
        self.extension: str = ""
        self.offset: int = 0          # Offset in bytes
        self.size: int = 0            # Size in bytes
        self.uncompressed_size: int = 0
        self.file_type: FileType = FileType.UNKNOWN
        self.compression_type: CompressionType = CompressionType.NONE
        self.rw_version: int = 0      # RenderWare version
        self.rw_version_name: str = "" # ADDED: Human readable version name
        self.is_encrypted: bool = False
        self.encryption_type: EncryptionType = EncryptionType.NONE
        self.is_new_entry: bool = False
        self.is_replaced: bool = False
        self.is_readonly: bool = False
        self.flags: int = 0
        self.compression_level = 0

        # Internal data cache
        self._cached_data: Optional[bytes] = None
        self._img_file: Optional['IMGFile'] = None
        self._version_detected: bool = False # ADDED: Track if version was detected
    
    def set_img_file(self, img_file: 'IMGFile'): #vers 1
        """Set reference to parent IMG file"""
        self._img_file = img_file

    def detect_file_type_and_version(self): #vers 4
        """Detect file type and RW version. Robust against garbage bytes after the null
        terminator or after the extension in DIR entry name fields.

        """
        try:
            self.name = _parse_entry_name(self.name.encode('ascii', errors='replace'))

            if '.' in self.name:
                self.extension = self.name.split('.')[-1].upper()
                self.extension = ''.join(c for c in self.extension if c.isalpha())
            else:
                self.extension = "NO_EXT"

            ext_lower = self.extension.lower()
            if ext_lower == 'dff':
                self.file_type = FileType.DFF
            elif ext_lower == 'txd':
                self.file_type = FileType.TXD
            elif ext_lower == 'col':
                self.file_type = FileType.COL
            elif ext_lower == 'ifp':
                self.file_type = FileType.IFP
            elif ext_lower == 'ipl':
                self.file_type = FileType.IPL
            elif ext_lower == 'dat':
                self.file_type = FileType.DAT
            elif ext_lower == 'wav':
                self.file_type = FileType.WAV
            elif ext_lower in ('strm',):
                self.file_type = FileType.STRM
            elif ext_lower in ('chk',):
                self.file_type = FileType.CHK
            else:
                self.file_type = FileType.UNKNOWN

            # RW version detection — skipped during bulk load (expensive: disk I/O + LZO)
            if self.extension in ['DFF', 'TXD'] and not self._version_detected:
                self._detect_rw_version()

        except Exception as e:
            img_debugger.error(f"Error detecting file type for {self.name}: {e}")

    def _detect_rw_version(self): #vers 2
        """Detect RenderWare version from file header - scans for valid version across known offsets"""
        try:
            if not self._img_file or not self._img_file.file_path:
                return

            # Read enough bytes to cover standard + prefixed layouts
            file_data = self._read_header_data(64)
            if not file_data or len(file_data) < 12:
                return

            version_value, found_offset = _scan_rw_version(file_data)
            if version_value:
                version_name = get_rw_version_name(version_value)
                self.rw_version = version_value
                self.rw_version_name = version_name
                self._version_detected = True
                img_debugger.success(f"Detected RW version {version_name} (0x{version_value:X}) for {self.name}")

        except Exception as e:
            img_debugger.error(f"Error detecting RW version for {self.name}: {e}")

    def detect_rw_version(self, data: bytes = None) -> bool: #vers 2
        """Detect RenderWare version from provided data - validates version range"""
        try:
            if data is None:
                if self._img_file:
                    data = self.get_data()
                else:
                    return False

            if not data or len(data) < 12:
                return False

            version_value, _ = _scan_rw_version(data[:64])
            if version_value:
                version_name = get_rw_version_name(version_value)
                self.rw_version = version_value
                self.rw_version_name = version_name
                self._version_detected = True
                if hasattr(img_debugger, 'success'):
                    img_debugger.success(f"Detected RW version {version_name} (0x{version_value:X}) for {self.name}")
                return True

            return False

        except Exception as e:
            if hasattr(img_debugger, 'error'):
                img_debugger.error(f"Error detecting RW version for {self.name}: {e}")
            return False

    def _read_header_data(self, bytes_to_read: int) -> Optional[bytes]: #vers 4
        """Read file header data from the data portion of the IMG archive.

        For Xbox LZO entries only the first ~512 bytes of the compressed stream
        are read.  The RW chunk header (12 bytes) is always encoded as a literal
        run in the first LZO instruction, so we only need to decompress the very
        start of the stream — not a full 64 KB block.
        """
        try:
            if not self._img_file or not self._img_file.file_path:
                return None

            file_path = self._img_file.file_path

            # V1-family opened via .dir: data lives in the companion .img file
            if (self._img_file.version in (IMGVersion.VERSION_1,
                                           IMGVersion.VERSION_1_5,
                                           IMGVersion.VERSION_SOL)
                    and file_path.lower().endswith('.dir')):
                img_path = _find_companion(file_path, '.img')
                if not img_path:
                    return None
                file_path = img_path

            is_xbox = (getattr(self._img_file, 'platform', None) == IMGPlatform.XBOX)

            # For Xbox: only read 512 bytes — the RW header is always in the first
            # LZO literal run (~30-50 compressed bytes).  No need to read 64 KB.
            read_size = min(self.size, 512) if is_xbox else min(self.size, bytes_to_read + 256)

            with open(file_path, 'rb') as f:
                f.seek(self.offset)
                raw = f.read(read_size)

            # Decompress just enough of the LZO stream to extract the RW header
            if is_xbox and _is_xbox_lzo(raw):
                raw = _xbox_lzo_peek_header(raw, bytes_to_read)

            return raw[:bytes_to_read]

        except Exception as e:
            img_debugger.error(f"Error reading header data for {self.name}: {e}")
            return None

    def _get_file_type_from_extension(self) -> FileType: #vers 1
        """Get file type from extension"""
        ext_lower = self.extension.lower()
        try:
            return FileType(ext_lower)
        except ValueError:
            return FileType.UNKNOWN

    def get_version_text(self) -> str: #vers 3
        """Get human-readable version text"""
        try:
            if self.extension in ['DFF', 'TXD']:
                if self.size == 0:
                    return "Empty"
                if self.rw_version > 0 and self.rw_version_name:
                    return f"RW {self.rw_version_name}"
                elif self.rw_version > 0:
                    return f"RW 0x{self.rw_version:X}"
                else:
                    return "RW Unknown"
            elif self.extension == 'COL':
                return "COL"
            elif self.extension == 'IFP':
                return "IFP"
            elif self.extension == 'IPL':
                return "IPL"
            elif self.extension in ['WAV', 'MP3']:
                return "Audio"
            else:
                return "Unknown"
        except:
            return "Unknown"
    
    def get_offset_in_sectors(self) -> int: #vers 1
        """Get offset in 2048-byte sectors"""
        return self.offset // 2048
    
    def get_size_in_sectors(self) -> int: #vers 1
        """Get size in 2048-byte sectors (rounded up)"""
        return (self.size + 2047) // 2048
    
    def get_file_type(self) -> FileType: #vers 1
        """Get file type based on extension"""
        if not self.extension:
            return FileType.UNKNOWN
        
        ext_lower = self.extension.lower().lstrip('.')
        try:
            return FileType(ext_lower)
        except ValueError:
            return FileType.UNKNOWN
    
    def is_renderware_file(self) -> bool: #vers 1
        """Check if file is a RenderWare format"""
        return self.extension.upper() in ['DFF', 'TXD']
    
    def validate(self) -> ValidationResult: #vers 1
        """Validate entry data"""
        result = ValidationResult()
        
        try:
            # Check basic attributes
            if not self.name:
                result.add_error("Entry has no name")
            
            if self.size < 0:
                result.add_error("Entry has negative size")
            
            if self.offset < 0:
                result.add_error("Entry has negative offset")
            
            # Check name validity
            if len(self.name) > 24:
                result.add_warning("Entry name longer than 24 characters")
            
            invalid_chars = set('\x00\xff\xcd')
            if any(char in self.name for char in invalid_chars):
                result.add_error("Entry name contains invalid characters")
            
            # Validate data if available
            if self._img_file:
                try:
                    data = self.get_data()
                    if len(data) != self.size:
                        result.add_warning(f"Entry {self.name} actual size differs from header")
                except Exception as e:
                    result.add_error(f"Cannot read data for {self.name}: {str(e)}")

        except Exception as e:
            result.add_error(f"Validation error for {self.name}: {str(e)}")

        return result
    
    def get_data(self) -> bytes: #vers 1
        """Read entry data from IMG file"""
        if not self._img_file:
            raise ValueError("No IMG file reference set")
        
        return self._img_file.read_entry_data(self)
    
    def set_data(self, data: bytes): #vers 1
        """Write entry data to IMG file"""
        if not self._img_file:
            raise ValueError("No IMG file reference set")
        
        self._img_file.write_entry_data(self, data)

# INLINE PLATFORM DETECTION FUNCTIONS - to replace the circular import
def _detect_xbox_by_content(file_path: str) -> bool:
    """Return True if this IMG/DIR pair contains Xbox LZO-compressed entries.

    Reads the first DIR entry to get the offset of the first data entry,
    then peeks at 4 bytes in the .img file.  If they match the Xbox LZO
    magic (0x67A3A1CE) it is Xbox.
    """
    try:
        path_lower = file_path.lower()

        if path_lower.endswith('.dir'):
            img_path = _find_companion(file_path, '.img')
            if not os.path.exists(img_path):
                return False
            with open(file_path, 'rb') as df:
                entry = df.read(32)
            if len(entry) < 8:
                return False
            offset_bytes = struct.unpack_from('<I', entry, 0)[0] * 2048
            with open(img_path, 'rb') as f:
                f.seek(offset_bytes)
                magic_bytes = f.read(4)

        elif path_lower.endswith('.img'):
            dir_path = _find_companion(file_path, '.dir')
            if os.path.exists(dir_path):
                with open(dir_path, 'rb') as df:
                    entry = df.read(32)
                if len(entry) < 8:
                    return False
                offset_bytes = struct.unpack_from('<I', entry, 0)[0] * 2048
                with open(file_path, 'rb') as f:
                    f.seek(offset_bytes)
                    magic_bytes = f.read(4)
            else:
                # VER2 single file — read first entry offset from directory
                with open(file_path, 'rb') as f:
                    f.seek(8)
                    entry = f.read(8)
                    if len(entry) < 8:
                        return False
                    offset_bytes = struct.unpack_from('<I', entry, 0)[0] * 2048
                    f.seek(offset_bytes)
                    magic_bytes = f.read(4)
        else:
            return False

        if len(magic_bytes) < 4:
            return False
        return struct.unpack_from('<I', magic_bytes, 0)[0] == _XBOX_LZO_MAGIC

    except Exception:
        return False


def detect_img_platform(file_path: str): #vers 3
    """Detect IMG platform — content-based probes first, filename as fallback.

    Priority order:
      1. Xbox LZO magic in first entry data bytes (95% confidence)
      2. Mobile texture DB alongside VER2 file → Android (90% confidence)
      3. iOS *_pvr.img suffix → iOS (85% confidence)
      4. Filename keywords → lower confidence
      5. Default PC
    """
    try:
        # 1. Xbox — probe entry data for LZO magic
        if _detect_xbox_by_content(file_path):
            return IMGPlatform.XBOX, {'confidence': 95, 'indicators': ['lzo_magic']}

        filename  = os.path.basename(file_path).lower()
        directory = os.path.dirname(file_path)

        # 2. Android — mobile texture DB alongside (SA Android, LCS Android)
        _mobile_dbs = ('texdb.dat', 'texdb.toc', 'streaming.dat',
                       'cdimages', 'models.img')
        if any(os.path.exists(os.path.join(directory, db)) for db in _mobile_dbs[:3]):
            return IMGPlatform.ANDROID, {'confidence': 90, 'indicators': ['mobile_db']}

        # 3. iOS — *_pvr.img suffix
        if '_pvr' in filename and filename.endswith('.img'):
            return IMGPlatform.IOS, {'confidence': 85, 'indicators': ['pvr_suffix']}

        # 4. Filename keyword fallbacks
        if any(k in filename for k in ('ps2', 'playstation')):
            return IMGPlatform.PS2,     {'confidence': 70, 'indicators': ['ps2_filename']}
        if any(k in filename for k in ('xbox',)):
            return IMGPlatform.XBOX,    {'confidence': 80, 'indicators': ['xbox_filename']}
        if any(k in filename for k in ('android', 'mobile')):
            return IMGPlatform.ANDROID, {'confidence': 70, 'indicators': ['android_filename']}
        if any(k in filename for k in ('psp', 'stories')):
            return IMGPlatform.PSP,     {'confidence': 70, 'indicators': ['psp_filename']}
        if any(k in filename for k in ('ios', '_pvr')):
            return IMGPlatform.IOS,     {'confidence': 70, 'indicators': ['ios_filename']}

        return IMGPlatform.PC, {'confidence': 50, 'indicators': ['default_pc']}

    except Exception:
        return IMGPlatform.UNKNOWN, {'confidence': 0, 'indicators': ['error']}

def detect_img_platform_inline(file_path: str) -> IMGPlatform: #vers 2
    """Content-based platform detection (inline, no tuple return)."""
    try:
        if _detect_xbox_by_content(file_path):
            return IMGPlatform.XBOX
        filename = os.path.basename(file_path).lower()
        if any(keyword in filename for keyword in ['ps2', 'playstation']):
            return IMGPlatform.PS2
        elif any(keyword in filename for keyword in ['xbox']):
            return IMGPlatform.XBOX
        elif any(keyword in filename for keyword in ['android', 'mobile']):
            return IMGPlatform.ANDROID
        elif any(keyword in filename for keyword in ['psp', 'stories']):
            return IMGPlatform.PSP
        else:
            return IMGPlatform.PC
    except Exception:
        return IMGPlatform.UNKNOWN

def get_platform_specific_specs(platform: IMGPlatform) -> Dict[str, Any]: #vers 1
    """INLINE: Get platform-specific specifications"""
    specs = {
        IMGPlatform.PC: {
            'sector_size': 2048,
            'entry_size': 32,
            'name_length': 24,
            'endianness': 'little',
            'supports_compression': True,
            'max_entries': 65535
        },
        IMGPlatform.PS2: {
            'sector_size': 2048,
            'entry_size': 32,
            'name_length': 24,
            'endianness': 'little',
            'supports_compression': False,
            'max_entries': 16000,
            'special_alignment': True
        },
        IMGPlatform.ANDROID: {
            'sector_size': 2048,
            'entry_size': 32,
            'name_length': 24,
            'endianness': 'little',
            'supports_compression': True,
            'max_entries': 32000,
            'mobile_optimized': True
        },
        IMGPlatform.PSP: {
            'sector_size': 2048,
            'entry_size': 32,
            'name_length': 24,
            'endianness': 'little',
            'supports_compression': False,
            'max_entries': 8000,
            'stories_format': True
        },
        IMGPlatform.XBOX: {
            'sector_size': 2048,
            'entry_size': 32,
            'name_length': 24,
            'endianness': 'little',     # IMG directory is LE, same as PC
            'supports_compression': True,
            'max_entries': 65535,
            'lzo_compressed': True,     # DFF/TXD entries may be LZO-compressed
            'lzo_magic': 0x67A3A1CE,
        }
    }
    return specs.get(platform, specs[IMGPlatform.PC])


def _parse_entry_name(raw_name_bytes: bytes) -> str:
    """Parse a 24-byte IMG directory name field robustly.

    Some third-party tools write garbage bytes after the extension (non-null),
    or embed partial Windows paths in the remaining bytes.  Standard null-split
    handles most cases, but when non-null garbage follows the extension we use a
    known-extension whitelist to truncate at the correct boundary.
    """
    # Stop at first null byte
    s = raw_name_bytes.split(b'\x00')[0].decode('ascii', errors='replace')
    # Find the last dot and check whether the chars that follow are a known extension
    dot_pos = s.rfind('.')
    if dot_pos > 0:
        after_dot = s[dot_pos + 1:dot_pos + 5].lower()
        for ext in sorted(_KNOWN_GTA_EXTENSIONS, key=len, reverse=True):
            if after_dot.startswith(ext):
                return s[:dot_pos + 1 + len(ext)]
    # No known extension: keep only filename-safe characters
    import re as _re
    m = _re.match(r'^[A-Za-z0-9_\-@+.]+', s)
    return m.group(0) if m else s


def _is_valid_rw_version(v: int) -> bool:
    """Delegate to the canonical is_valid_rw_version in rw_versions.py."""
    from apps.methods.rw_versions import is_valid_rw_version as _canonical
    return _canonical(v)

class IMGFile:
    """Main IMG archive file handler - FIXED WITH PLATFORM SUPPORT"""
    
    def __init__(self, file_path: str = ""): #vers 5
        self.file_path: str = file_path
        self.version: IMGVersion = IMGVersion.UNKNOWN
        self.platform: IMGPlatform = IMGPlatform.UNKNOWN  # ADDED: Platform detection
        self.platform_specs: Dict[str, Any] = {}  # ADDED: Platform-specific specs
        self.entries: List[IMGEntry] = []
        self.is_open: bool = False
        self.total_size: int = 0
        self.creation_time: Optional[float] = None
        self.modification_time: Optional[float] = None

        # File handles
        self._img_handle: Optional[BinaryIO] = None
        self._dir_handle: Optional[BinaryIO] = None
    
    def create_new(self, output_path: str, version: IMGVersion, **options) -> bool: #vers 2
        """Create new IMG file with specified parameters"""
        try:
            self.file_path = output_path
            self.version = version
            self.entries = []

            # Extract creation options
            initial_size_mb = options.get('initial_size_mb', 50)
            compression_enabled = options.get('compression_enabled', False)
            game_preset = options.get('game_preset', None)

            if version == IMGVersion.VERSION_1:
                # Use Version 1 creator
                from apps.core.img_version1 import IMGVersion1Creator
                creator = IMGVersion1Creator()
                success = creator.create_version_1(output_path, initial_size_mb)
                if success:
                    self.entries = creator.entries
                    self.file_path = creator.dir_path  # Store DIR file path for Version 1
                return success
                
            elif version == IMGVersion.VERSION_2:
                # Use Version 2 creator
                from apps.core.img_version2 import IMGVersion2Creator
                creator = IMGVersion2Creator()
                success = creator.create_version_2(output_path, initial_size_mb, compression_enabled)
                if success:
                    self.entries = creator.entries
                    self.file_path = creator.file_path
                return success
                
            else:
                print(f"Unsupported IMG version: {version}")
                return False

        except Exception as e:
            print(f"Error creating IMG file: {e}")
            return False

    def save_img_file(self) -> bool: #vers 2
        """Save IMG file with current entries"""
        try:
            if not self.file_path or not self.entries:
                return False

            # Create backup first
            import shutil
            backup_path = self.file_path + '.backup'
            shutil.copy2(self.file_path, backup_path)

            # Rebuild the IMG file
            return self.rebuild_img_file()

        except Exception as e:
            return False

    def save(self, file_path=None): #vers 1
        """Save IMG file - wrapper for save_img_file()"""
        if file_path:
            self.file_path = file_path
        return self.save_img_file()

    def rebuild_img_file(self) -> bool: #vers 1
        """Rebuild IMG file based on version"""
        try:
            if self.version == IMGVersion.VERSION_1:
                return self._rebuild_version1()
            elif self.version == IMGVersion.VERSION_2:
                return self._rebuild_version2()
            else:
                return False

        except Exception as e:
            return False

    def _sanitize_filename(self, filename: str) -> str: #vers 1
        """CRITICAL: Clean corrupted filenames before encoding"""
        try:
            # Remove corrupted bytes that show as garbage in table
            clean_name = filename.replace('\x00', '').replace('\xcd', '').replace('\xff', '')

            # Remove control characters (except null terminator)
            clean_name = ''.join(c for c in clean_name if 32 <= ord(c) <= 126)

            # Limit to IMG field size
            clean_name = clean_name.strip()[:24]

            # Fallback if empty
            if not clean_name:
                clean_name = f"file_{len(self.entries):04d}.dat"

            return clean_name

        except Exception:
            return f"file_{len(self.entries):04d}.dat"


    def _rebuild_version2(self) -> bool: #vers 1
        """Rebuild Version 2 IMG file (SA format)"""
        try:
            import struct
            import os

            # Calculate sizes
            entry_count = len(self.entries)
            directory_size = entry_count * 32  # 32 bytes per entry
            data_start = directory_size

            # Collect entry data
            entry_data_list = []
            current_offset = data_start

            for entry in self.entries:
                # Get entry data
                if hasattr(entry, '_cached_data') and entry._cached_data:
                    data = entry._cached_data
                else:
                    data = self.read_entry_data(entry)

                entry_data_list.append(data)

                # Update entry with new offset/size
                entry.offset = current_offset
                entry.size = len(data)

                # Align to sector boundary (2048 bytes)
                aligned_size = ((len(data) + 2047) // 2048) * 2048
                current_offset += aligned_size

            # Write new IMG file
            with open(self.file_path, 'wb') as f:
                # Write directory
                for i, entry in enumerate(self.entries):
                    # Convert to sectors
                    offset_sectors = entry.offset // 2048
                    size_sectors = ((entry.size + 2047) // 2048)

                    # Pack entry: offset(4), size(4), name(24)
                    entry_data = struct.pack('<II', offset_sectors, size_sectors)

                    #name_bytes = entry.name.encode('ascii')[:24].ljust(24, b'\x00')
                    # CORRUPTION FIX: Sanitize before encoding
                    clean_name = self._sanitize_filename(entry.name)
                    if clean_name != entry.name:
                        print(f"[CORRUPTION FIX] '{entry.name}' Ã¢ÂÂ '{clean_name}'")
                        entry.name = clean_name

                    name_bytes = clean_name.encode('ascii', errors='replace')[:24]
                    name_bytes = name_bytes.ljust(24, b'\x00')

                    entry_data += name_bytes

                    f.write(entry_data)

                # Write file data
                for i, data in enumerate(entry_data_list):
                    f.seek(self.entries[i].offset)
                    f.write(data)

                    # Pad to sector boundary
                    current_pos = f.tell()
                    sector_end = ((current_pos + 2047) // 2048) * 2048
                    if current_pos < sector_end:
                        f.write(b'\x00' * (sector_end - current_pos))

            print(f"Rebuilt IMG file: {entry_count} entries")
            return True

        except Exception as e:
            return False

    def _rebuild_version1(self) -> bool: #vers 1
        """Rebuild Version 1 IMG file (DIR/IMG pair)"""
        try:
            import struct
            import os

            # Get DIR and IMG paths
            dir_path = self.file_path
            img_path = self.file_path.replace('.dir', '.img')

            entry_count = len(self.entries)

            # Collect entry data and calculate offsets
            entry_data_list = []
            current_offset = 0

            for entry in self.entries:
                # Get entry data
                if hasattr(entry, '_cached_data') and entry._cached_data:
                    data = entry._cached_data
                else:
                    data = self.read_entry_data(entry)

                entry_data_list.append(data)

                # Update entry with new offset/size
                entry.offset = current_offset
                entry.size = len(data)

                # Align to sector boundary
                aligned_size = ((len(data) + 2047) // 2048) * 2048
                current_offset += aligned_size

            # Write DIR file
            with open(dir_path, 'wb') as f:
                for entry in self.entries:
                    # Convert to sectors
                    offset_sectors = entry.offset // 2048
                    size_sectors = ((entry.size + 2047) // 2048)

                    # Pack entry: offset(4), size(4), name(24)
                    entry_data = struct.pack('<II', offset_sectors, size_sectors)
                    name_bytes = entry.name.encode('ascii')[:24].ljust(24, b'\x00')
                    entry_data += name_bytes

                    f.write(entry_data)

            # Write IMG file
            with open(img_path, 'wb') as f:
                for i, data in enumerate(entry_data_list):
                    f.seek(self.entries[i].offset)
                    f.write(data)

                    # Pad to sector boundary
                    current_pos = f.tell()
                    sector_end = ((current_pos + 2047) // 2048) * 2048
                    if current_pos < sector_end:
                        f.write(b'\x00' * (sector_end - current_pos))

            print(f"Rebuilt DIR/IMG pair: {entry_count} entries")
            return True

        except Exception as e:
            return False

    def import_file(self, file_path: str) -> bool: #vers 1
        """Import file into IMG"""
        try:
            import os
            filename = os.path.basename(file_path)

            # Read file data
            with open(file_path, 'rb') as f:
                data = f.read()

            # Use add_entry method
            return self.add_entry(filename, data)

        except Exception as e:
            return False


    def add_entry(self, filename: str, data: bytes, auto_save: bool = True) -> bool: #vers 3
        """Add new entry to IMG file - FIXED VERSION with enhanced debugging"""
        try:
            # CRITICAL: Sanitize filename to prevent corruption
            clean_filename = self._sanitize_filename(filename)
            if clean_filename != filename:
                filename = clean_filename


            # Check for duplicate entries (replace if exists)
            existing_entry = None
            for i, entry in enumerate(self.entries):
                if entry.name == filename:
                    existing_entry = entry
                    break

            # Skip if existing entry is pinned
            if existing_entry and getattr(existing_entry, 'is_pinned', False):
                return False

            # Calculate proper offset for new entry
            if self.entries and not existing_entry:
                # Find the end of the last entry
                last_entry = max(self.entries, key=lambda e: e.offset + e.size)
                # Align to sector boundary (2048 bytes for IMG files)
                last_end = last_entry.offset + last_entry.size
                new_offset = ((last_end + 2047) // 2048) * 2048
            else:
                # First entry or replacing existing
                if self.version == IMGVersion.VERSION_1:
                    new_offset = 0  # Version 1 starts at beginning of .img file
                else:
                    # Version 2: Calculate directory size first
                    directory_size = len(self.entries) * 32  # 32 bytes per entry
                    new_offset = directory_size

            # Create new IMGEntry with proper setup
            if existing_entry:
                # Replace existing entry data
                new_entry = existing_entry
                new_entry._cached_data = data
                new_entry.size = len(data)
                # Keep existing offset for replacement
            else:
                # Create brand new entry
                new_entry = IMGEntry()
                new_entry.name = filename
                new_entry.size = len(data)
                new_entry.offset = new_offset
                new_entry.set_img_file(self)
                new_entry._cached_data = data

                # Detect file type and RW version from data
                new_entry.detect_file_type_and_version()

                # Add to entries list
                self.entries.append(new_entry)

            new_entry.is_new_entry = True
            # Stamp creation date
            try:
                from datetime import datetime
                new_entry.date_modified = datetime.now().strftime("%b %d %Y %H:%M:%S")
            except Exception:
                pass

            # Only save if requested (for batch operations, set auto_save=False)
            if auto_save:

                if hasattr(self, 'save_img_file'):
                    success = self.save_img_file()
                else:
                    from apps.core.save_img_entry import save_img_file_with_backup
                    success = save_img_file_with_backup(self)

                if success:
                    pass
                else:
                    pass
                return success

            # Entry added successfully but not saved
            return True

        except Exception as e:
            import traceback
            traceback.print_exc()
            return False


    def calculate_next_offset(self) -> int: #vers 1
        """Calculate the next available offset for a new entry - HELPER METHOD"""
        try:
            if not self.entries:
                # First entry
                if self.version == IMGVersion.VERSION_1:
                    return 0  # Version 1 starts at beginning
                else:
                    return 0  # Version 2 will be recalculated during save

            # Find the entry that ends the latest
            max_end = 0
            for entry in self.entries:
                entry_end = entry.offset + entry.size
                if entry_end > max_end:
                    max_end = entry_end

            # Align to sector boundary (2048 bytes)
            aligned_offset = ((max_end + 2047) // 2048) * 2048
            return aligned_offset

        except Exception as e:
            return 0

    def remove_entry(self, filename: str) -> bool: #vers 1
        """Remove entry by filename - HELPER METHOD"""
        try:
            for i, entry in enumerate(self.entries):
                if entry.name == filename:
                    removed_entry = self.entries.pop(i)
                    return True

            return False

        except Exception as e:
            return False

    def has_entry(self, filename: str) -> bool: #vers 1
        """Check if entry exists by filename - HELPER METHOD"""
        try:
            return any(entry.name == filename for entry in self.entries)
        except Exception:
            return False

    def get_entry(self, filename: str) -> Optional['IMGEntry']: #vers 1
        """Get entry by filename - HELPER METHOD"""
        try:
            for entry in self.entries:
                if entry.name == filename:
                    return entry
            return None
        except Exception:
            return None

    def add_multiple_entries(self, file_data_pairs: List[tuple], auto_save: bool = True) -> int: #vers 1
        """Add multiple entries efficiently - BATCH METHOD"""
        try:
            added_count = 0


            for filename, data in file_data_pairs:
                # Add without auto-save for efficiency
                if self.add_entry(filename, data, auto_save=False):
                    added_count += 1
                else:
                    pass

            # Save once at the end if requested
            if auto_save and added_count > 0:
                if self.save_img_file():
                    pass
                else:
                    return 0

            return added_count

        except Exception as e:
            return 0

    def integrate_fixed_add_entry_methods(img_file_class): #vers 1
        """Integrate all fixed methods into IMGFile class"""
        try:
            # Add the fixed methods to the class
            img_file_class.add_entry = add_entry
            img_file_class.calculate_next_offset = calculate_next_offset
            img_file_class.remove_entry = remove_entry
            img_file_class.has_entry = has_entry
            img_file_class.get_entry = get_entry
            img_file_class.add_multiple_entries = add_multiple_entries

            print("Fixed add_entry methods integrated into IMGFile class")
            return True

        except Exception as e:
            print(f"Failed to integrate fixed add_entry methods: {e}")
            return False


    def detect_version(self) -> IMGVersion: #vers 4
        """Detect IMG version and platform from file"""
        try:
            if not os.path.exists(self.file_path):
                return IMGVersion.UNKNOWN

            # ADDED: Platform detection first
            detected_platform, detection_info = detect_img_platform(self.file_path)
            self.platform = detected_platform
            self.platform_specs = get_platform_specific_specs(detected_platform)
            

            # Check if it's a .dir file (Version 1 or 1.5 or Xbox)
            if self.file_path.lower().endswith('.dir'):
                img_path = _find_companion(self.file_path, '.img')
                if img_path:
                    # Xbox check first — same DIR+IMG structure but entries are LZO-compressed
                    if _detect_xbox_by_content(self.file_path):
                        self.version  = IMGVersion.VERSION_XBOX
                        self.platform = IMGPlatform.XBOX
                        return IMGVersion.VERSION_XBOX
                    v = _detect_v1_or_v1_5(self.file_path, img_path)
                    ver = IMGVersion.VERSION_1_5 if v == 'V1_5' else IMGVersion.VERSION_1
                    self.version = ver
                    return ver

            # Check for PS2 IRX (IOP MIPS ELF modules - CDSTREAM.IRX etc.)
            if self.file_path.lower().endswith('.irx'):
                try:
                    with open(self.file_path, 'rb') as _f:
                        _irx_magic = _f.read(4)
                    if _irx_magic == b'\x7fELF':
                        self.version  = IMGVersion.VERSION_IRX
                        self.platform = IMGPlatform.PS2
                        return IMGVersion.VERSION_IRX
                except Exception:
                    pass

            # Check for GAME.DTZ (LCS/VCS PS2/PSP zlib data container)
            if self.file_path.lower().endswith('.dtz'):
                from apps.core.img_dtz import detect_dtz, open_dtz
                if detect_dtz(self.file_path):
                    # Open immediately to determine LCS vs VCS
                    try:
                        _dtz = open_dtz(self.file_path)
                        _game = _dtz.get('game', 'UNKNOWN')
                        if _game == 'VCS':
                            self.version  = IMGVersion.VERSION_DTZ_VCS
                            self.platform = IMGPlatform.PS2
                            return IMGVersion.VERSION_DTZ_VCS
                        elif _game == 'LCS':
                            self.version  = IMGVersion.VERSION_DTZ_LCS
                            self.platform = IMGPlatform.PS2
                            return IMGVersion.VERSION_DTZ_LCS
                        else:
                            # Unknown game — default to VCS structure
                            self.version  = IMGVersion.VERSION_DTZ_VCS
                            self.platform = IMGPlatform.PS2
                            return IMGVersion.VERSION_DTZ_VCS
                    except Exception:
                        pass

            # Check for LVZ (PS2 VCS zlib streaming archive)
            if self.file_path.lower().endswith('.lvz'):
                from apps.core.img_ps2_vcs import detect_lvz
                if detect_lvz(self.file_path):
                    self.version = IMGVersion.VERSION_PS2_LVZ
                    return IMGVersion.VERSION_PS2_LVZ

            # Check for HXD/MXD/AGR (Bully bone/animation data)
            if os.path.splitext(self.file_path)[1].lower() in ('.hxd', '.mxd', '.agr'):
                from apps.core.img_ps2_vcs import detect_hxd
                if detect_hxd(self.file_path):
                    self.version = IMGVersion.VERSION_HXD
                    return IMGVersion.VERSION_HXD

            # Check for ANPK (PSP animation package, uses .img extension) - has clear magic
            if self.file_path.lower().endswith('.img'):
                from apps.core.img_ps2_vcs import detect_anpk
                if detect_anpk(self.file_path):
                    self.version = IMGVersion.VERSION_ANPK
                    return IMGVersion.VERSION_ANPK

            # Check if it's a single .img file (Version 2 or 1/1.5)
            if self.file_path.lower().endswith('.img'):
                try:
                    with open(self.file_path, 'rb') as f:
                        header = f.read(4)
                        if header == b'VER2':
                            # VER2 — distinguish PC SA, Android SA, Android LCS
                            # Android SA: filename pattern OR mobile texture DB alongside
                            _bn = os.path.basename(self.file_path).lower()
                            _dir = os.path.dirname(self.file_path)
                            _has_mobile_db = any(
                                os.path.exists(os.path.join(_dir, db))
                                for db in ('texdb.dat', 'texdb.toc', 'streaming.dat')
                            )
                            if 'lcs' in _bn or 'liberty' in _bn:
                                self.version  = IMGVersion.VERSION_LCS_ANDROID
                                self.platform = IMGPlatform.ANDROID
                                return IMGVersion.VERSION_LCS_ANDROID
                            if _has_mobile_db or 'android' in _bn or 'mobile' in _bn:
                                self.version  = IMGVersion.VERSION_SA_ANDROID
                                self.platform = IMGPlatform.ANDROID
                                return IMGVersion.VERSION_SA_ANDROID
                            self.version = IMGVersion.VERSION_2
                            return IMGVersion.VERSION_2
                        # GTA IV unencrypted magic: 0xA94E2A52 (little-endian)
                        import struct as _struct
                        magic_val = _struct.unpack('<I', header)[0] if len(header) == 4 else 0
                        if magic_val == 0xA94E2A52:
                            self.version = IMGVersion.VERSION_3
                            return IMGVersion.VERSION_3
                        # Check if V3 encrypted: read next 16 bytes, try AES decrypt, check Version==3
                        f.seek(0)
                        first16 = f.read(16)
                        if _is_v3_encrypted(first16):
                            self.version = IMGVersion.VERSION_3_ENC
                            return IMGVersion.VERSION_3_ENC
                        dir_path = _find_companion(self.file_path, '.dir')
                        if dir_path:
                            v = _detect_v1_or_v1_5(dir_path, self.file_path)
                            ver = IMGVersion.VERSION_1_5 if v == 'V1_5' else IMGVersion.VERSION_1
                        else:
                            # Check PS2 VCS before generic standalone fallback
                            from apps.core.img_ps2_vcs import detect_ps2_vcs, detect_ps2_v1, detect_bully
                            if detect_ps2_vcs(self.file_path):
                                self.version = IMGVersion.VERSION_PS2_VCS
                                return IMGVersion.VERSION_PS2_VCS
                            # iOS *_pvr.img: distinguish LCS iOS from GTA3/VC iOS
                            _fname = os.path.basename(self.file_path).lower()
                            if '_pvr' in _fname and detect_ps2_v1(self.file_path):
                                # LCS iOS uses same 12-byte format but different TXD version
                                if 'lcs' in _fname or 'liberty' in _fname:
                                    self.version  = IMGVersion.VERSION_LCS_IOS
                                    self.platform = IMGPlatform.IOS
                                    return IMGVersion.VERSION_LCS_IOS
                                self.version  = IMGVersion.VERSION_1_IOS
                                self.platform = IMGPlatform.IOS
                                return IMGVersion.VERSION_1_IOS
                            # Before treating as PS2_V1, check if a companion .lvz
                            # exists — if so this is a streaming segment (BEACH.IMG etc.)
                            # paired with a DLRW .LVZ index, not a standalone archive
                            _companion_lvz = _find_companion(self.file_path, '.lvz')
                            if _companion_lvz:
                                self._streaming_segment_error = (
                                    f"{os.path.basename(self.file_path)} is a streaming data file.\n\n"
                                    "This file is raw streaming asset data indexed by a companion\n"
                                    f"{os.path.basename(_companion_lvz)} file — it has no internal\n"
                                    "directory and cannot be opened as a standalone archive.\n\n"
                                    f"Open {os.path.basename(_companion_lvz)} instead."
                                )
                                self.version = IMGVersion.VERSION_STREAMING_SEG
                                return IMGVersion.VERSION_STREAMING_SEG
                            if detect_ps2_v1(self.file_path):
                                self.version = IMGVersion.VERSION_PS2_V1
                                return IMGVersion.VERSION_PS2_V1
                            if detect_bully(self.file_path):
                                self.version = IMGVersion.VERSION_BULLY
                                return IMGVersion.VERSION_BULLY
                            # Check for streaming segment (LCS/VCS iOS/PSP) —
                            # raw data file with no self-describing header;
                            # lives alongside a gta3.img in the same directory.
                            _sibling = os.path.join(
                                os.path.dirname(self.file_path), 'gta3.img')
                            if (os.path.exists(_sibling) and
                                    _sibling != self.file_path):
                                # sibling gta3.img exists -> this is a segment
                                try:
                                    with open(_sibling, 'rb') as _sf:
                                        _sm = _sf.read(4)
                                    if _sm == b'VER2':
                                        self.version = IMGVersion.VERSION_STREAMING_SEG
                                        return IMGVersion.VERSION_STREAMING_SEG
                                except Exception:
                                    pass
                            # Check for VCS .LVZ streaming companion before V1 fallback
                            _lvz = _find_companion(self.file_path, '.lvz')
                            if _lvz:
                                # .lvz companion exists → this IMG is a raw streaming
                                # segment (COMMER/BEACH/MALL etc.) — block it with a
                                # helpful message, suggest opening the .lvz instead
                                self._streaming_segment_error = (
                                    f"{os.path.basename(self.file_path)} is a raw streaming data file.\n\n"
                                    "This file has no internal directory — it is asset data\n"
                                    f"indexed by the companion {os.path.basename(_lvz)}.\n\n"
                                    f"Open {os.path.basename(_lvz)} instead to browse this archive."
                                )
                                self.version = IMGVersion.VERSION_STREAMING_SEG
                                return IMGVersion.VERSION_STREAMING_SEG
                            # No .dir and no .lvz - standalone V1/V1.5, check size
                            sz = os.path.getsize(self.file_path)
                            ver = IMGVersion.VERSION_1_5 if sz > 2 * 1024 * 1024 * 1024 else IMGVersion.VERSION_1
                        self.version = ver
                        return ver
                except:
                    pass

        except Exception as e:
            pass

        self.version = IMGVersion.UNKNOWN
        return IMGVersion.UNKNOWN

    def open(self) -> bool: #vers 5
        """Open and parse IMG file - FIXED WITH PROPER ENTRY PARSING"""
        try:
            if self.is_open:
                return True

            # Detect version first
            if self.version == IMGVersion.UNKNOWN:
                self.detect_version()

            # Clear existing entries
            self.entries.clear()

            # Open based on version
            success = False
            if self.version in (IMGVersion.VERSION_1,
                                IMGVersion.VERSION_1_5,
                                IMGVersion.VERSION_SOL):
                success = self._open_version_1()
            elif self.version == IMGVersion.VERSION_XBOX:
                success = self._open_xbox()
            elif self.version in (IMGVersion.VERSION_2,
                                  IMGVersion.VERSION_SA_ANDROID):
                success = self._open_version_2()
            elif self.version in (IMGVersion.VERSION_LCS_ANDROID,
                                  IMGVersion.VERSION_LCS_IOS):
                success = self._open_lcs()
            elif self.version == IMGVersion.VERSION_STREAMING_SEG:
                # Raw streaming segment — no internal directory
                # Message was already set during detection (LVZ companion case)
                # or set the default message here
                if not hasattr(self, '_streaming_segment_error'):
                    _lvz = _find_companion(self.file_path, '.lvz')
                    if _lvz:
                        self._streaming_segment_error = (
                            f"{os.path.basename(self.file_path)} is a streaming data file.\n\n"
                            "This file is raw streaming asset data indexed by the companion\n"
                            f"{os.path.basename(_lvz)}.\n\n"
                            f"Open {os.path.basename(_lvz)} instead to browse the streaming archive."
                        )
                    else:
                        self._streaming_segment_error = (
                            f"{os.path.basename(self.file_path)} is a streaming segment file.\n\n"
                            "This file contains raw asset data referenced by gta3.img — it has no "
                            "internal directory and cannot be opened as a standalone archive.\n\n"
                            "Open gta3.img from the same folder instead."
                        )
                return False
            elif self.version in (IMGVersion.VERSION_3,
                                  IMGVersion.VERSION_3_ENC):
                success = self._open_version_3()
            elif self.version in (IMGVersion.VERSION_DTZ_VCS,
                                  IMGVersion.VERSION_DTZ_LCS):
                success = self._open_dtz()
            elif self.version == IMGVersion.VERSION_IRX:
                # IRX = single ELF module, show as one entry
                try:
                    sz = os.path.getsize(self.file_path)
                    e = IMGEntry()
                    e.name = os.path.basename(self.file_path)
                    e.offset = 0
                    e.size = sz
                    e.is_readonly = True
                    self.entries.append(e)
                    success = True
                except Exception:
                    success = False
            elif self.version in (IMGVersion.VERSION_PS2_VCS,
                                  IMGVersion.VERSION_PS2_LVZ,
                                  IMGVersion.VERSION_IOS_LVZ,
                                  IMGVersion.VERSION_PS2_V1,
                                  IMGVersion.VERSION_1_IOS,
                                  IMGVersion.VERSION_ANPK,
                                  IMGVersion.VERSION_BULLY,
                                  IMGVersion.VERSION_HXD):
                success = self._open_ps2()

            if success:
                self.is_open = True
                # FIXED: Parse file types and versions for all entries
                self._parse_all_entries()
            
            return success

        except Exception as e:
            import traceback
            print(f"[IMGFile.open] Exception for {self.file_path}: {e}")
            traceback.print_exc()
            return False

    def _parse_all_entries(self): #vers 2
        """ADDED: Parse file types and versions for all entries + UNKNOWN RW DETECTION"""
        try:
            
            for i, entry in enumerate(self.entries):
                try:
                    # Detect file type and RW version
                    entry.detect_file_type_and_version()
                    
                    # Log progress for large files
                    if i > 0 and i % 100 == 0:
                        pass
                        
                except Exception as e:
                    pass
                    
            
            # ADDED: Trigger unknown RW file detection after parsing
            self._trigger_unknown_rw_detection()
            
        except Exception as e:
            pass

    def _trigger_unknown_rw_detection(self): #vers 1
        """ADDED: Trigger unknown RW file detection and snapshotting"""
        try:
            # Try to find main window reference for unknown RW detection
            # This will be set by the integration function
            if hasattr(self, '_main_window_ref') and self._main_window_ref:
                main_window = self._main_window_ref
                if hasattr(main_window, 'rw_snapshot_manager'):
                    unknown_files = main_window.rw_snapshot_manager.capture_unknown_rw_files(self)
                    if unknown_files:
                        print(f"[INFO] Captured {len(unknown_files)} unknown RW files for analysis")
                else:
                    pass
            else:
                pass
                
        except Exception as e:
            pass

    def set_main_window_reference(self, main_window): #vers 1
        """ADDED: Set main window reference for unknown RW detection"""
        self._main_window_ref = main_window

    def _open_version_2(self) -> bool: #vers 5
        """Open IMG version 2 (single file) - ENHANCED WITH PLATFORM SUPPORT"""
        try:
            # Use platform-specific specifications
            sector_size = self.platform_specs.get('sector_size', 2048)
            
            with open(self.file_path, 'rb') as f:
                # Skip VER2 header (4 bytes)
                f.seek(4)
                # Read entry count
                entry_count = struct.unpack('<I', f.read(4))[0]
                
                # Platform-specific entry count validation
                max_entries = self.platform_specs.get('max_entries', 65535)
                if entry_count > max_entries:
                    pass

                for i in range(entry_count):
                    # Read entry: offset(4), size(4), name(24)
                    entry_data = f.read(32)
                    if len(entry_data) < 32:
                        break

                    entry_offset, entry_size = struct.unpack('<II', entry_data[:8])
                    entry_name = _parse_entry_name(entry_data[8:32])

                    if entry_name:
                        entry = IMGEntry()
                        entry.name = entry_name
                        entry.offset = entry_offset * 2048  # Convert sectors to bytes
                        entry.size = entry_size * 2048
                        entry.set_img_file(self)
                        self.entries.append(entry)

            return True
        except Exception as e:
            return False

    def _open_version_1(self) -> bool: #vers 6
        """Open IMG version 1/1.5/1_MOBILE (DIR/IMG pair, or standalone embedded directory)"""
        dir_path = _find_companion(self.file_path, '.dir')
        if not dir_path:
            # Standalone IMG - try reading embedded directory from start of file
            return self._open_version_1_standalone()

        sector_size = 2048

        try:
            with open(dir_path, 'rb') as dir_file:
                dir_data = dir_file.read()

            # Parse directory entries (32 bytes each)
            entry_count = len(dir_data) // 32
            for i in range(entry_count):
                offset = i * 32
                entry_data = dir_data[offset:offset+32]

                if len(entry_data) < 32:
                    break

                # Parse entry: offset(4), size(4), name(24)
                entry_offset, entry_size = struct.unpack('<II', entry_data[:8])
                entry_name = _parse_entry_name(entry_data[8:32])

                if entry_name:
                    entry = IMGEntry()
                    entry.name = entry_name
                    entry.offset = entry_offset * sector_size
                    entry.size = entry_size * sector_size
                    entry.set_img_file(self)
                    self.entries.append(entry)

            return True
        except Exception as e:
            return False

    def _open_version_3(self) -> bool: #vers 1
        """Open IMG version 3 - GTA IV format (unencrypted or AES-256 ECB encrypted header)."""
        import struct as _struct
        GTAIV_MAGIC  = 0xA94E2A52
        GTAIV_KEY = bytes([
            0x1a,0xb5,0x6f,0xed,0x7e,0xc3,0xff,0x01,
            0x22,0x7b,0x69,0x15,0x33,0x97,0x5d,0xce,
            0x47,0xd7,0x69,0x65,0x3f,0xf7,0x75,0x42,
            0x6a,0x96,0xcd,0x6d,0x53,0x07,0x56,0x5d,
        ])
        try:
            with open(self.file_path, 'rb') as f:
                raw_header = f.read(20)

            if len(raw_header) < 20:
                return False

            # Decrypt header if encrypted (16 rounds AES-256 ECB on first 16 bytes)
            if self.version == IMGVersion.VERSION_3_ENC:
                try:
                    from Crypto.Cipher import AES
                    block = raw_header[:16]
                    for _ in range(16):
                        block = AES.new(GTAIV_KEY, AES.MODE_ECB).decrypt(block)
                    header_data = block + raw_header[16:]
                except ImportError:
                    return False
            else:
                header_data = raw_header

            magic, version, num_items, table_size, item_size, unknown =                 _struct.unpack('<IIIIHH', header_data[:20])

            if version != 3 or item_size != 16:
                return False

            # Read table (num_items * 16 bytes) + filenames block
            names_size = table_size - (num_items * 16)
            with open(self.file_path, 'rb') as f:
                f.seek(20)
                table_data  = f.read(num_items * 16)
                names_data  = f.read(names_size)

            # Decrypt table + names if encrypted
            if self.version == IMGVersion.VERSION_3_ENC:
                try:
                    from Crypto.Cipher import AES
                    # Decrypt in 16-byte blocks, 16 rounds each
                    def _decrypt_block(b):
                        for _ in range(16):
                            b = AES.new(GTAIV_KEY, AES.MODE_ECB).decrypt(b)
                        return b
                    # Pad to 16-byte boundary, decrypt, trim
                    def _decrypt_buf(buf):
                        pad = (16 - len(buf) % 16) % 16
                        padded = buf + b'\x00' * pad
                        result = b''.join(_decrypt_block(padded[i:i+16])
                                          for i in range(0, len(padded), 16))
                        return result[:len(buf)]
                    table_data = _decrypt_buf(table_data)
                    names_data = _decrypt_buf(names_data)
                except ImportError:
                    return False

            # Parse names (null-separated)
            names = []
            cur = 0
            while cur < len(names_data):
                end = names_data.find(b'\x00', cur)
                if end == -1:
                    end = len(names_data)
                name = names_data[cur:end].decode('ascii', errors='ignore')
                if name:
                    names.append(name)
                cur = end + 1

            # Parse table items (16 bytes each)
            for i in range(num_items):
                item = table_data[i*16:(i+1)*16]
                if len(item) < 16:
                    break
                size_bytes, resource_type, position, size_blocks, _unk =                     _struct.unpack('<IIIHH', item)
                entry = IMGEntry()
                entry.name        = names[i] if i < len(names) else f'entry_{i}'
                entry.offset      = position * 2048
                entry.size        = size_bytes
                entry.set_img_file(self)
                self.entries.append(entry)

            return len(self.entries) > 0
        except Exception:
            return False

    def _open_xbox(self) -> bool: #vers 1
        """Open Xbox GTA3/VC IMG — DIR+IMG pair, LZO-compressed entries.

        The directory format is identical to PC Version 1 (32-byte entries,
        2048-byte sectors).  Entry data is LZO-compressed; read_entry_data()
        handles decompression transparently via _xbox_lzo_decompress_entry().
        """
        # Reuse the V1 directory parser — structure is identical
        ok = self._open_version_1()
        if ok:
            self.platform = IMGPlatform.XBOX
            # Mark all entries as potentially LZO-compressed
            for entry in self.entries:
                entry.compression_type = CompressionType.LZO
        return ok

    def _open_lcs(self) -> bool: #vers 1
        """Open LCS Android / LCS iOS IMG.

        LCS Android: VER2 header, 2048-byte sectors, TXD entries use RW
          version 0x1005FFFF embedded directly in the IMG (no separate
          mobile texture DB).  Parsed as VER2 with platform tag.

        LCS iOS: 12-byte directory entries, 512-byte sectors, *_pvr.img
          suffix.  Same binary layout as PS2_V1 / VERSION_1_IOS.
        """
        if self.version == IMGVersion.VERSION_LCS_IOS:
            # Same 12-byte format as PS2_V1 / VERSION_1_IOS
            try:
                from apps.core.img_ps2_vcs import open_ps2_v1
                result = open_ps2_v1(self.file_path)
                if result.get('error'):
                    return False
                for e in result['entries']:
                    entry = IMGEntry()
                    entry.name        = e['name']
                    entry.offset      = e['offset']
                    entry.size        = e['size']
                    entry.is_readonly = True
                    self.entries.append(entry)
                self.platform = IMGPlatform.IOS
                return True
            except Exception:
                return False

        # LCS Android — VER2 layout
        ok = self._open_version_2()
        if ok:
            self.platform = IMGPlatform.ANDROID
        return ok

    def _open_dtz(self) -> bool: #vers 2
        """Open a GAME.DTZ (LCS/VCS PS2/PSP) zlib-compressed data container.

        GAME.DTZ contains MOCAPPS2.DIR (cutscene animations) and compiled game data.
        Entries from MOCAPPS2.DIR reference MOCAPPS2.IMG in the same directory.
        The main asset archives (GTA3.IMG, BEACH/MAINLA/MALL.IMG) are separate files.
        """
        try:
            from apps.core.img_dtz import open_dtz
            result = open_dtz(self.file_path)
            if result.get('error'):
                return False
            entries_raw = result.get('entries', [])
            if not entries_raw:
                return False
            # Cache the decompressed blob
            self._dtz_blob = result.get('blob')
            self.platform  = IMGPlatform.PS2
            # Find companion MOCAPPS2.IMG for source annotation
            dtz_dir = os.path.dirname(self.file_path)
            mocap_img = os.path.join(dtz_dir, 'MOCAPPS2.IMG')
            if not os.path.exists(mocap_img):
                mocap_img = os.path.join(dtz_dir, 'mocapps2.img')
            for e in entries_raw:
                entry             = IMGEntry()
                entry.name        = e['name']
                entry.offset      = e['offset']
                entry.size        = e['size']
                entry.is_readonly = True
                # Store sector position for Source column
                entry.cd_sector   = e.get('cd_sector', e['offset'] // 2048)
                # Build source reference string
                entry._source_ref = f"GTA3PS2.IMG @ sector {entry.cd_sector}"
                if os.path.exists(mocap_img):
                    entry._source_img = mocap_img
                self.entries.append(entry)
            return True
        except Exception:
            return False

    def _open_ps2(self) -> bool: #vers 5
        """Open PS2/PSP/iOS/Android/Bully/HXD archive - read-only."""
        try:
            from apps.core.img_ps2_vcs import (open_ps2_vcs, open_ps2_v1, open_lvz,
                                                open_anpk, open_bully, open_hxd)
            if self.version == IMGVersion.VERSION_PS2_VCS:
                result = open_ps2_vcs(self.file_path)
            elif self.version in (IMGVersion.VERSION_PS2_V1,
                                  IMGVersion.VERSION_1_IOS):
                result = open_ps2_v1(self.file_path)
            elif self.version == IMGVersion.VERSION_ANPK:
                result = open_anpk(self.file_path)
            elif self.version == IMGVersion.VERSION_BULLY:
                result = open_bully(self.file_path)
            elif self.version == IMGVersion.VERSION_HXD:
                result = open_hxd(self.file_path)
            elif self.version in (IMGVersion.VERSION_PS2_LVZ, IMGVersion.VERSION_IOS_LVZ):
                # The .LVZ is the archive — either we opened the .LVZ directly,
                # or we opened the companion .IMG and need to find the .LVZ
                lvz_path = self.file_path
                if not lvz_path.lower().endswith('.lvz'):
                    lvz_path = _find_companion(self.file_path, '.lvz')
                if not lvz_path:
                    return False
                # Try iOS format first (strict DLRW sentinel), then PS2
                from apps.core.img_ps2_vcs import open_ios_lvz
                result = open_ios_lvz(lvz_path)
                if result.get("error"):
                    result = open_lvz(lvz_path)
                else:
                    self.version = IMGVersion.VERSION_IOS_LVZ
            else:
                result = open_lvz(self.file_path)
            if result.get("error"):
                return False
            # Find companion .img for source reference
            _lvz_basename = os.path.basename(self.file_path)
            _img_companion = _find_companion(self.file_path, '.img') or ''
            _img_name = os.path.basename(_img_companion) if _img_companion else _lvz_basename.replace('.lvz','').replace('.LVZ','') + '.IMG'
            for e in result["entries"]:
                entry             = IMGEntry()
                entry.name        = e["name"]
                entry.offset      = e["offset"]
                entry.size        = e["size"]
                entry.is_readonly = True
                entry._source_ref = e.get("_source_ref", f"{_img_name} @ 0x{int(e['offset']):X}")
                self.entries.append(entry)
            return True
        except Exception as _e:
            import traceback
            print(f"[_open_ps2] Exception: {_e}")
            traceback.print_exc()
            return False

    def _open_version_1_standalone(self) -> bool: #vers 1
        """Open standalone V1/V1.5 IMG with embedded directory at start of file."""
        try:
            with open(self.file_path, 'rb') as f:
                # Read first 32 bytes as first entry - use to find dir size
                first = f.read(32)
                if len(first) < 32:
                    return False
                first_offset, first_size = struct.unpack('<II', first[:8])
                first_name = _parse_entry_name(first[8:32])
                if not first_name:
                    return False
                # Directory occupies sectors 0..(first_offset-1)
                dir_sector_count = first_offset
                dir_bytes = dir_sector_count * 2048
                f.seek(0)
                dir_data = f.read(dir_bytes)

            entry_count = len(dir_data) // 32
            for i in range(entry_count):
                entry_data = dir_data[i*32:(i+1)*32]
                if len(entry_data) < 32:
                    break
                entry_offset, entry_size = struct.unpack('<II', entry_data[:8])
                entry_name = _parse_entry_name(entry_data[8:32])
                if not entry_name:
                    continue
                entry = IMGEntry()
                entry.name = entry_name
                entry.offset = entry_offset * 2048
                entry.size = entry_size * 2048
                entry.set_img_file(self)
                self.entries.append(entry)

            return len(self.entries) > 0
        except Exception:
            return False

    def read_entry_data(self, entry: IMGEntry) -> bytes: #vers 3
        """Read data for a specific entry, transparently decompressing Xbox LZO if needed."""
        try:
            # DIR+IMG pair formats: V1, V1_5, SOL, Xbox all use .dir + .img
            if self.version in (IMGVersion.VERSION_1,
                                IMGVersion.VERSION_1_5,
                                IMGVersion.VERSION_SOL,
                                IMGVersion.VERSION_XBOX):
                # Resolve .img path from whatever we were opened with (.dir or .img)
                fp = self.file_path
                if fp.lower().endswith('.dir'):
                    img_path = _find_companion(fp, '.img')
                    if not img_path:
                        img_path = fp  # fallback (will likely fail)
                else:
                    img_path = fp
                with open(img_path, 'rb') as f:
                    f.seek(entry.offset)
                    data = f.read(entry.size)
            elif self.version in (IMGVersion.VERSION_PS2_LVZ,
                                    IMGVersion.VERSION_IOS_LVZ,
                                    IMGVersion.VERSION_DTZ_VCS,
                                    IMGVersion.VERSION_DTZ_LCS):
                # LVZ/DTZ entries: offset is a byte position in the companion .img
                # Use _source_img attr if set, otherwise find the companion
                img_path = getattr(entry, '_source_img', None)
                if not img_path:
                    img_path = _find_companion(self.file_path, '.img')
                if not img_path:
                    raise RuntimeError("No companion .img found for LVZ/DTZ entry")
                with open(img_path, 'rb') as f:
                    f.seek(entry.offset)
                    data = f.read(entry.size)
            else:
                with open(self.file_path, 'rb') as f:
                    f.seek(entry.offset)
                    data = f.read(entry.size)

            # Transparent Xbox LZO decompression
            if self.platform == IMGPlatform.XBOX and _is_xbox_lzo(data):
                try:
                    data = _xbox_lzo_decompress_entry(data)
                    entry.compression_type = CompressionType.LZO
                except Exception:
                    pass  # return raw on failure

            return data
        except Exception as e:
            raise RuntimeError(f"Failed to read entry data: {e}")

    def write_entry_data(self, entry: IMGEntry, data: bytes): #vers 2
        """Write data for a specific entry"""
        try:
            if self.version in (IMGVersion.VERSION_1,
                                IMGVersion.VERSION_1_5,
                                IMGVersion.VERSION_SOL,
                                IMGVersion.VERSION_XBOX):
                fp = self.file_path
                if fp.lower().endswith('.dir'):
                    img_path = _find_companion(fp, '.img')
                    if not img_path:
                        img_path = fp  # fallback (will likely fail)
                else:
                    img_path = fp
                with open(img_path, 'r+b') as f:
                    f.seek(entry.offset)
                    f.write(data)
            else:
                # Write to single .img file
                with open(self.file_path, 'r+b') as f:
                    f.seek(entry.offset)
                    f.write(data)
        except Exception as e:
            raise RuntimeError(f"Failed to write entry data: {e}")

    def close(self): #vers 1
        """Close IMG file"""
        self.is_open = False
        self.entries.clear()

    def get_creation_info(self) -> Dict[str, Any]: #vers 1
        """Get information about the IMG file"""
        if not self.file_path or not os.path.exists(self.file_path):
            return {}
        
        try:
            file_size = os.path.getsize(self.file_path)
            return {
                'path': self.file_path,
                'size_bytes': file_size,
                'size_mb': file_size / (1024 * 1024),
                'entries_count': len(self.entries),
                'version': self.version.name,
                'format': f'IMG Version {self.version.value}'
            }
        except Exception:
            return {}

def format_file_size(size_bytes: int) -> str: #vers 1
    """Format file size in human-readable format"""
    if size_bytes < 1024:
        return f"{size_bytes} B"
    elif size_bytes < 1024 * 1024:
        return f"{size_bytes / 1024:.1f} KB"
    elif size_bytes < 1024 * 1024 * 1024:
        return f"{size_bytes / (1024 * 1024):.1f} MB"
    else:
        return f"{size_bytes / (1024 * 1024 * 1024):.1f} GB"

# ALL ORIGINAL GUI CLASSES PRESERVED EXACTLY AS IN ORIGINAL
class IMGEntriesTable(DragSelectTableWidget): #vers 2
    """Enhanced table widget for IMG entries — supports click-drag row selection."""
    entry_double_clicked = pyqtSignal(object)

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.setColumnCount(7)
        self.setHorizontalHeaderLabels(['Name', 'Type', 'Size', 'Offset', 'Version', 'Compression', 'Status'])
        self.setAlternatingRowColors(True)

        # Auto-resize columns
        header = self.horizontalHeader()
        header.setStretchLastSection(True)
        for i in range(6):
            header.setSectionResizeMode(i, header.ResizeMode.ResizeToContents)

class FilterPanel(QWidget):
    """Filter panel for IMG entries"""
    filter_changed = pyqtSignal(str)
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._setup_ui()
    
    def _setup_ui(self): #vers 1
        layout = QVBoxLayout(self)
        
        # File type filter
        type_group = QGroupBox("File Type Filter")
        type_layout = QHBoxLayout(type_group)
        
        self.type_combo = QComboBox()
        self.type_combo.addItems(['All', 'DFF', 'TXD', 'COL', 'IFP', 'IPL', 'DAT', 'WAV'])
        self.type_combo.currentTextChanged.connect(self.filter_changed.emit)
        type_layout.addWidget(self.type_combo)
        
        # Search filter
        search_group = QGroupBox("Search")
        search_layout = QHBoxLayout(search_group)
        
        self.search_edit = QLineEdit()
        self.search_edit.setPlaceholderText("Search entries...")
        self.search_edit.textChanged.connect(self.filter_changed.emit)
        search_layout.addWidget(self.search_edit)
        
        layout.addWidget(type_group)
        layout.addWidget(search_group)

class IMGFileInfoPanel(QWidget):
    """Information panel for IMG file details"""
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._setup_ui()
    
    def _setup_ui(self): #vers 1
        layout = QVBoxLayout(self)
        
        self.info_label = QLabel("No IMG file loaded")
        layout.addWidget(self.info_label)

class TabFilterWidget(QWidget):
    """Tab-specific filter widget"""
    
    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._setup_ui()
    
    def _setup_ui(self): #vers 1
        layout = QHBoxLayout(self)
        
        self.filter_combo = QComboBox()
        self.filter_combo.addItems(['All Files', 'Models (DFF)', 'Textures (TXD)', 'Collision (COL)', 'Animations (IFP)'])
        layout.addWidget(self.filter_combo)

def integrate_filtering(main_window): #vers 2
    """Integrate filtering functionality into main window"""
    try:
        # Create filter widget
        filter_widget = FilterPanel(main_window)

        # Connect filter widget to table
        if hasattr(filter_widget, 'filter_changed'):
            filter_widget.filter_changed.connect(table_widget.apply_filter)

        return filter_widget
    except Exception as e:
        img_debugger.error(f"Error integrating filtering: {e}")
        return None

def create_entries_table_panel(main_window): #vers 4
    """Create the complete entries table panel"""
    panel = QWidget()
    layout = QVBoxLayout(panel)
    layout.setContentsMargins(0, 0, 0, 0)

    # IMG file information
    info_group = QGroupBox("IMG File Information")
    info_layout = QVBoxLayout(info_group)

    main_window.file_info_panel = IMGFileInfoPanel()
    info_layout.addWidget(main_window.file_info_panel)

    layout.addWidget(info_group)

    # Filter panel
    filter_group = QGroupBox("Filter & Search")
    filter_layout = QVBoxLayout(filter_group)

    main_window.filter_panel = FilterPanel()
    filter_layout.addWidget(main_window.filter_panel)

    layout.addWidget(filter_group)

    # Entries table
    entries_group = QGroupBox("Archive Entries")
    entries_layout = QVBoxLayout(entries_group)

    main_window.entries_table = IMGEntriesTable()
    entries_layout.addWidget(main_window.entries_table)

    layout.addWidget(entries_group)

    # Connect filter to table
    main_window.filter_panel.filter_changed.connect(main_window.entries_table.apply_filter)

    # SIMPLIFIED CONNECTION - Let main app handle its own signals
    # Don't auto-connect anything from here to prevent conflicts

    if hasattr(main_window, 'on_entry_double_clicked'):
        # Only connect double-click since that doesn't cause logging conflicts
        try:
            main_window.entries_table.entry_double_clicked.disconnect()
        except:
            pass
        main_window.entries_table.entry_double_clicked.connect(main_window.on_entry_double_clicked)

    return panel

def create_img_file(output_path: str, version: IMGVersion, **options) -> bool: #vers 2
    """Create IMG file using appropriate version creator"""
    img = IMGFile()
    return img.create_new(output_path, version, **options)

def detect_img_version(file_path: str) -> IMGVersion: #vers 2
    """Detect IMG version without fully opening the file"""
    img = IMGFile(file_path)
    return img.detect_version()

def populate_table_with_sample_data(table): #vers 3
    """Populate table with sample data for testing"""
    sample_entries = [
        {"name": "player.dff", "extension": "DFF", "size": 250880, "offset": 0x2000, "version": "RW 3.6"},
        {"name": "player.txd", "extension": "TXD", "size": 524288, "offset": 0x42000, "version": "RW 3.6"},
        {"name": "vehicle.col", "extension": "COL", "size": 131072, "offset": 0x84000, "version": "COL 2"},
        {"name": "dance.ifp", "extension": "IFP", "size": 1258291, "offset": 0xA4000, "version": "IFP 1"},
    ]

    # Convert to mock entry objects
    class MockEntry:
        def __init__(self, data): #vers 1
            self.name = data["name"]
            self.extension = data["extension"]
            self.size = data["size"]
            self.offset = data["offset"]
            self._version = data["version"]
            self.is_new_entry = False
            self.is_replaced = False
            self.compression_type = CompressionType.NONE

        def get_version_text(self): #vers 1
            return self._version

    mock_entries = [MockEntry(data) for data in sample_entries]
    table.populate_entries(mock_entries)

# Export classes and functions - EXACTLY AS ORIGINAL
__all__ = [
    'IMGVersion',
    'FileType', 
    'CompressionType',
    'Platform',
    'IMGEntry',
    'IMGFile',
    'ValidationResult',
    'RecentFilesManager',
    'create_img_file',
    'format_file_size',
    'IMGEntriesTable',
    'FilterPanel', 
    'IMGFileInfoPanel',
    'TabFilterWidget',
    'integrate_filtering',
    'create_entries_table_panel',
    'detect_img_version',
    'populate_table_with_sample_data',
    'get_img_platform_info',  # ADDED: Platform info function
    'IMGPlatform'  # ADDED: Now exported since moved here
]
