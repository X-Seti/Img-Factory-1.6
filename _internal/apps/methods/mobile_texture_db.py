#this belongs in apps/methods/mobile_texture_db.py - Version: 3
# X-Seti - October05 2026 - IMG Factory 1.6 - Mobile Texture Database

"""
SA/VC mobile texture database (.txt .toc .dat .tmb) read and write.
"""

# Files: name.txt shared, name.<plat>.toc/.dat/.tmb per platform.
# .toc: u32 dat size, i32 offset per entry, -1 affiliate.
# SA entry: u16 hash, enc, width, height|0x8000 mips.
#   Then u32 size; body is u32 RLE indicator plus stream.
# VC entry: same 8 byte header, raw chain, no size.
# RLE segment: max(4, block bytes); chain padded to segment.
# .tmb: one thumbnail entry per stored texture, same entry layout.

import os
import re
import struct

import numpy as np

from apps.methods.mobile_texture_decode import (
    GL_DXT1, GL_DXT1A, GL_DXT3, GL_DXT5, GL_ETC1, GL_L8, GL_LA8, GL_PVRTC2_RGB,
    GL_PVRTC2_RGBA, GL_PVRTC4_RGB, GL_PVRTC4_RGBA, GL_RGB565, GL_RGBA4444,
    GL_RGBA5551, GL_RGBA8888, decode_level, encode_level, level_size, mip_dims,
)

##Methods list -
# _build_entry
# _downsample
# _make_thumb
# _parse_entries
# _rle_segment
# _txt_set_size
# _walk_offsets
# _write_file
# decode_rle
# describe_mobile_db
# detect_mobile_db
# encode_rle
# get_encoding_bpp
# get_encoding_name
# hash_texture_name
# load_mobile_texture_db
# parse_toc_file
# parse_txt_file
# save_mobile_texture_db

##class MobileTexture: -
# __init__
# __repr__
# bpp
# encoding_name
# has_alpha
# is_etc1
# is_pvrtc
# levels

##class MobileTextureDB: -
# __init__
# get_by_name
# is_android
# is_ios
# texture_count

ENCODING_NAMES = {
    GL_RGBA8888: 'RGBA8888', GL_L8: 'L8', GL_LA8: 'LA8', GL_RGBA4444: 'RGBA4444',
    GL_RGBA5551: 'RGBA5551', GL_RGB565: 'RGB565', GL_DXT1: 'DXT1',
    GL_DXT1A: 'DXT1A', GL_DXT3: 'DXT3', GL_DXT5: 'DXT5',
    GL_PVRTC4_RGB: 'PVRTC4-RGB', GL_PVRTC2_RGB: 'PVRTC2-RGB',
    GL_PVRTC4_RGBA: 'PVRTC4-RGBA', GL_PVRTC2_RGBA: 'PVRTC2-RGBA',
    GL_ETC1: 'ETC1',
}
ENCODING_BPP = {
    GL_RGBA8888: 32, GL_L8: 8, GL_LA8: 16, GL_RGBA4444: 16, GL_RGBA5551: 16,
    GL_RGB565: 16, GL_DXT1: 4, GL_DXT1A: 4, GL_DXT3: 8, GL_DXT5: 8,
    GL_PVRTC4_RGB: 4, GL_PVRTC2_RGB: 2, GL_PVRTC4_RGBA: 4,
    GL_PVRTC2_RGBA: 2, GL_ETC1: 4,
}
ENCODING_IS_PVRTC = {GL_PVRTC4_RGB, GL_PVRTC2_RGB, GL_PVRTC4_RGBA, GL_PVRTC2_RGBA}
ENCODING_HAS_ALPHA = {GL_RGBA8888, GL_RGBA4444, GL_RGBA5551, GL_DXT1A,
                      GL_DXT3, GL_DXT5, GL_PVRTC4_RGBA, GL_PVRTC2_RGBA}

PLATFORM_IOS = 'pvr'
PLATFORM_ANDROID = 'etc'
PLATFORMS = ('pvr', 'dxt', 'etc', 'unc')
LAYOUT_SA = 'sa'
LAYOUT_VC = 'vc'


def get_encoding_name(encoding_type): #vers 2
    """Readable name for a GL encoding id."""
    return ENCODING_NAMES.get(encoding_type, f'Unknown (0x{encoding_type:04X})')


def get_encoding_bpp(encoding_type): #vers 2
    """Bits per pixel for a GL encoding id, 0 if unknown."""
    return ENCODING_BPP.get(encoding_type, 0)


def hash_texture_name(name): #vers 1
    """u16 name hash stored in each .dat entry."""
    h = 0
    for byte in name.encode('ascii', errors='replace'):
        h = (h + ((h << 5) & 0xFFFFFFFF) + byte) & 0xFFFFFFFF
    h = (h + (h >> 5)) & 0xFFFFFFFF
    return h & 0xFFFF


def _rle_segment(enc): #vers 2
    """RLE segment size in bytes for an encoding."""
    if enc in (GL_PVRTC2_RGB, GL_PVRTC2_RGBA):
        return 32
    if enc in (GL_DXT3, GL_DXT5, GL_PVRTC4_RGB, GL_PVRTC4_RGBA):
        return 16
    if enc in (GL_DXT1, GL_DXT1A, GL_ETC1):
        return 8
    return 4


def decode_rle(data, segment_size, indicator): #vers 2
    """Expand RLE stream: indicator, count, segment repeats."""
    out = bytearray()
    i, n = 0, len(data)
    while i < n:
        if data[i] == indicator:
            if i + 2 + segment_size > n:
                raise ValueError(f"RLE run truncated at {i}")
            out += data[i + 2:i + 2 + segment_size] * data[i + 1]
            i += 2 + segment_size
        else:
            if i + segment_size > n:
                raise ValueError(f"RLE literal truncated at {i}")
            out += data[i:i + segment_size]
            i += segment_size
    return bytes(out)


def encode_rle(data, segment_size, indicator): #vers 1
    """Compress data with the mobile RLE scheme."""
    if len(data) % segment_size:
        raise ValueError("RLE input not a whole number of segments")
    segs = [data[i:i + segment_size] for i in range(0, len(data), segment_size)]
    out = bytearray()
    i = 0
    while i < len(segs):
        j = i + 1
        while j < len(segs) and j - i < 255 and segs[j] == segs[i]:
            j += 1
        run = j - i
        if run >= 2 or segs[i][0] == indicator:
            out += bytes([indicator, run]) + segs[i]
        else:
            out += segs[i]
        i = j
    return bytes(out)


class MobileTexture:
    """One texture entry from a mobile texture database."""

    def __init__(self): #vers 2
        self.name = ''
        self.index = 0
        self.hash = 0
        self.encoding_type = 0
        self.width = 0
        self.height = 0
        self.has_mipmaps = False
        self.mip_count = 1
        self.compressed_size = 0
        self.rle_indicator = 0
        self.entry_offset = -1
        self.data_offset = 0
        self.entry_bytes = b''
        self.raw_data = b''
        self.pixel_data = b''
        self.thumb_index = -1
        self.txt_props = {}
        self.is_affiliate = False

    @property
    def encoding_name(self): #vers 1
        return get_encoding_name(self.encoding_type)

    @property
    def is_pvrtc(self): #vers 2
        return self.encoding_type in ENCODING_IS_PVRTC

    @property
    def is_etc1(self): #vers 2
        return self.encoding_type == GL_ETC1

    @property
    def bpp(self): #vers 1
        return get_encoding_bpp(self.encoding_type)

    @property
    def has_alpha(self): #vers 1
        return self.encoding_type in ENCODING_HAS_ALPHA

    def levels(self): #vers 1
        """Decoded mip levels as (width, height, rgba bytes)."""
        out, pos = [], 0
        for w, h in mip_dims(self.width, self.height, self.has_mipmaps):
            size = level_size(self.encoding_type, w, h)
            rgba = decode_level(self.encoding_type, self.pixel_data[pos:pos + size], w, h)
            out.append((w, h, rgba.tobytes()))
            pos += size
        return out

    def __repr__(self): #vers 1
        return (f'<MobileTexture {self.name!r} {self.width}x{self.height} '
                f'{self.encoding_name} mipmaps={self.has_mipmaps}>')


class MobileTextureDB:
    """Parsed mobile texture database for one platform."""

    def __init__(self): #vers 2
        self.name = ''
        self.platform = ''
        self.layout = ''
        self.folder = ''
        self.textures = []
        self.thumbs = []
        self.txt_path = ''
        self.toc_path = ''
        self.dat_path = ''
        self.tmb_path = ''
        self.dat_size = 0
        self.errors = []

    @property
    def texture_count(self): #vers 1
        return len(self.textures)

    @property
    def is_ios(self): #vers 1
        return self.platform == PLATFORM_IOS

    @property
    def is_android(self): #vers 2
        return self.platform in ('etc', 'dxt')

    def get_by_name(self, name): #vers 1
        for t in self.textures:
            if t.name == name:
                return t
        return None


def parse_txt_file(txt_path): #vers 2
    """Parse .txt into category props and texture prop dicts."""
    category, textures = {}, []
    with open(txt_path, 'r', errors='replace', newline='') as f:
        lines = f.read().splitlines()
    for line_no, raw in enumerate(lines):
        line = raw.strip()
        if not line or line.startswith('#'):
            continue
        name = None
        rest = line
        if line.startswith('"'):
            end = line.find('"', 1)
            if end == -1:
                raise ValueError(f"{txt_path}:{line_no + 1}: unterminated name")
            name, rest = line[1:end], line[end + 1:]
        props = {}
        for token in rest.split():
            if '=' in token:
                k, _, v = token.partition('=')
                props[k.strip('"')] = v.strip('"')
        if name is None:
            category.update(props)
        else:
            entry = {'name': name, 'is_affiliate': 'affiliate' in props, '_line': line_no}
            entry.update(props)
            textures.append(entry)
    return category, textures


def parse_toc_file(toc_path, entry_count): #vers 2
    """Read .toc: (stated dat size, offsets with -1 affiliates)."""
    with open(toc_path, 'rb') as f:
        data = f.read()
    if len(data) != 4 + 4 * entry_count:
        raise ValueError(f"{os.path.basename(toc_path)}: {len(data)} bytes, "
                         f"expected {4 + 4 * entry_count} for {entry_count} entries")
    return struct.unpack_from('<I', data, 0)[0], list(struct.unpack_from(f'<{entry_count}i', data, 4))


def _parse_entries(blob, starts, layout, load_pixel_data): #vers 1
    """Parse entries at sorted start offsets of a .dat or .tmb."""
    out = []
    ends = starts[1:] + [len(blob)]
    for start, end in zip(starts, ends):
        t = MobileTexture()
        t.entry_offset = start
        t.entry_bytes = blob[start:end]
        t.hash, t.encoding_type, t.width, hm = struct.unpack_from('<HHHH', blob, start)
        t.height = hm & 0x7FFF
        t.has_mipmaps = bool(hm & 0x8000)
        dims = mip_dims(t.width, t.height, t.has_mipmaps)
        t.mip_count = len(dims)
        chain = sum(level_size(t.encoding_type, w, h) for w, h in dims)
        if layout == LAYOUT_SA:
            t.compressed_size, t.rle_indicator = struct.unpack_from('<II', blob, start + 8)
            if 12 + t.compressed_size != end - start:
                raise ValueError(f"Entry at 0x{start:x}: size {t.compressed_size} != span {end - start - 12}")
            t.data_offset = start + 16
        else:
            if 8 + chain != end - start:
                raise ValueError(f"Entry at 0x{start:x}: chain {chain} != span {end - start - 8}")
            t.compressed_size = chain
            t.data_offset = start + 8
        t.raw_data = blob[t.data_offset:end]
        if load_pixel_data:
            if layout == LAYOUT_SA and t.rle_indicator:
                pix = decode_rle(t.raw_data, _rle_segment(t.encoding_type), t.rle_indicator)
            else:
                pix = t.raw_data
            if not chain <= len(pix) < chain + _rle_segment(t.encoding_type):
                raise ValueError(f"Entry at 0x{start:x}: pixel data {len(pix)} != chain {chain}")
            t.pixel_data = pix[:chain]
        out.append(t)
    return out


def detect_mobile_db(path): #vers 2
    """Return (db_name, platform, folder) for any DB file, else None."""
    if not path:
        return None
    folder, fname = os.path.split(path)
    low = fname.lower()
    if low.endswith('.txt'):
        db_name = fname[:-4]
        for plat in PLATFORMS:
            if os.path.isfile(os.path.join(folder, f'{db_name}.{plat}.dat')):
                return db_name, plat, folder
        return None
    m = re.match(r'^(.+)\.(pvr|dxt|etc|unc)\.(dat|toc|tmb)$', fname, re.I)
    if not m:
        return None
    db_name, plat = m.group(1), m.group(2).lower()
    if not os.path.isfile(os.path.join(folder, f'{db_name}.{plat}.dat')):
        return None
    return db_name, plat, folder


def load_mobile_texture_db(path, load_pixel_data=True): #vers 3
    """Load a mobile texture database from any of its files."""
    detected = detect_mobile_db(path)
    if not detected:
        return None
    db_name, platform, folder = detected
    db = MobileTextureDB()
    db.name, db.platform, db.folder = db_name, platform, folder
    db.txt_path = os.path.join(folder, f'{db_name}.txt')
    db.toc_path = os.path.join(folder, f'{db_name}.{platform}.toc')
    db.dat_path = os.path.join(folder, f'{db_name}.{platform}.dat')
    db.tmb_path = os.path.join(folder, f'{db_name}.{platform}.tmb')
    if os.path.isfile(db.txt_path):
        _, entries = parse_txt_file(db.txt_path)
    else:                                   # ported sets ship without .txt
        count = (os.path.getsize(db.toc_path) - 4) // 4
        entries = [{'name': '', 'is_affiliate': False, '_line': -1} for _ in range(count)]
        db.txt_path = ''
        db.errors.append(f"No {db_name}.txt - texture names generated")
    db.dat_size, offsets = parse_toc_file(db.toc_path, len(entries))
    with open(db.dat_path, 'rb') as f:
        dat = f.read()
    if db.dat_size != len(dat):
        raise ValueError(f".toc dat size {db.dat_size} != actual {len(dat)}")
    starts = sorted(o for o in offsets if o >= 0)
    if not starts:
        raise ValueError("No stored textures in .toc")
    # SA entries carry a size field matching the span
    nxt = starts[1] if len(starts) > 1 else len(dat)
    db.layout = LAYOUT_SA if struct.unpack_from('<I', dat, starts[0] + 8)[0] + 12 == nxt - starts[0] else LAYOUT_VC
    parsed = {t.entry_offset: t for t in _parse_entries(dat, starts, db.layout, load_pixel_data)}
    stored = 0
    for i, (props, off) in enumerate(zip(entries, offsets)):
        if off < 0:
            t = MobileTexture()
            t.is_affiliate = True
        else:
            t = parsed[off]
            t.thumb_index = stored
            stored += 1
            if not props['name']:
                props['name'] = f"tex_{i:04d}_{t.hash:04x}"
            elif t.hash != hash_texture_name(props['name']):
                db.errors.append(f"Hash mismatch for {props['name']}")
        if not props['name']:
            props['name'] = f"affiliate_{i:04d}"
        t.name, t.index, t.txt_props = props['name'], i, dict(props)
        db.textures.append(t)
    if os.path.isfile(db.tmb_path):
        with open(db.tmb_path, 'rb') as f:
            tmb = f.read()
        db.thumbs = _parse_entries(tmb, _walk_offsets(tmb, db.layout), db.layout, load_pixel_data)
        if len(db.thumbs) != stored:
            raise ValueError(f".tmb has {len(db.thumbs)} entries, expected {stored}")
    return db


def _walk_offsets(blob, layout): #vers 1
    """Entry start offsets of a sequential .tmb file."""
    pos, starts = 0, []
    while pos < len(blob):
        starts.append(pos)
        enc, w, hm = struct.unpack_from('<HHH', blob, pos + 2)
        if layout == LAYOUT_SA:
            pos += 12 + struct.unpack_from('<I', blob, pos + 8)[0]
        else:
            dims = mip_dims(w, hm & 0x7FFF, bool(hm & 0x8000))
            pos += 8 + sum(level_size(enc, a, b) for a, b in dims)
    if pos != len(blob):
        raise ValueError(".tmb entries overrun file end")
    return starts


def _downsample(rgba): #vers 1
    """2x2 box filter to the next mip size."""
    h, w = rgba.shape[:2]
    a = rgba.astype(np.uint32)
    if h > 1:
        a = a[0:h // 2 * 2:2] + a[1:h // 2 * 2:2]
    if w > 1:
        a = a[:, 0:w // 2 * 2:2] + a[:, 1:w // 2 * 2:2]
    div = (2 if h > 1 else 1) * (2 if w > 1 else 1)
    return ((a + div // 2) // div).astype(np.uint8)


def _build_entry(layout, name_hash, enc, rgba, mips, rle_indicator): #vers 1
    """Encode RGBA array as a full .dat or .tmb entry."""
    h, w = rgba.shape[:2]
    if enc in ENCODING_IS_PVRTC and (w & (w - 1) or h & (h - 1)):
        raise ValueError(f"PVRTC needs power of two size, got {w}x{h}")
    parts, cur = [], rgba
    for lw, lh in mip_dims(w, h, mips):
        if cur.shape[:2] != (lh, lw):
            cur = _downsample(cur)
        parts.append(encode_level(enc, cur, lw, lh))
    chain = b''.join(parts)
    head = struct.pack('<HHHH', name_hash, enc, w, h | (0x8000 if mips else 0))
    if layout == LAYOUT_VC:
        return head + chain
    if rle_indicator:
        seg = _rle_segment(enc)
        chain += b'\0' * (-len(chain) % seg)
        body = struct.pack('<I', rle_indicator) + encode_rle(chain, seg, rle_indicator)
    else:
        body = struct.pack('<I', 0) + chain
    return head + struct.pack('<I', len(body)) + body


def _make_thumb(layout, tex, thumb, rgba): #vers 1
    """New thumbnail entry in the old thumbnail's format."""
    small = rgba
    while small.shape[0] > thumb.height or small.shape[1] > thumb.width:
        small = _downsample(small)
    if small.shape[:2] != (thumb.height, thumb.width):
        raise ValueError(f"{tex.name}: cannot fit thumbnail {thumb.width}x{thumb.height}")
    return _build_entry(layout, thumb.hash, thumb.encoding_type, small,
                        thumb.has_mipmaps, thumb.rle_indicator)


def _txt_set_size(text, line_no, width, height): #vers 1
    """Rewrite width and height on one .txt line."""
    lines = text.splitlines(True)
    line = lines[line_no]
    line = re.sub(r'\bwidth=\d+', f'width={width}', line)
    line = re.sub(r'\bheight=\d+', f'height={height}', line)
    lines[line_no] = line
    return ''.join(lines)


def _write_file(path, data): #vers 1
    """Write bytes to path."""
    with open(path, 'wb') as f:
        f.write(data)


def save_mobile_texture_db(db_or_path, edited, out_dir=None): #vers 2
    """Re-encode edited textures; rewrite .dat .toc .tmb .txt."""
    db = db_or_path if isinstance(db_or_path, MobileTextureDB) else load_mobile_texture_db(db_or_path)
    if db is None:
        raise ValueError(f"Not a mobile texture database: {db_or_path}")
    out_dir = out_dir or db.folder
    os.makedirs(out_dir, exist_ok=True)
    by_name = {t.name: t for t in db.textures}
    new_entry, new_thumb, resized = {}, {}, {}
    for name, val in edited.items():
        tex = by_name.get(name)
        if tex is None or tex.is_affiliate:
            raise KeyError(f"No stored texture named {name!r}")
        rgba, w, h = (val, tex.width, tex.height) if isinstance(val, (bytes, bytearray)) else val
        if len(rgba) != w * h * 4:
            raise ValueError(f"{name}: rgba is {len(rgba)} bytes, expected {w * h * 4}")
        arr = np.frombuffer(bytes(rgba), dtype=np.uint8).reshape(h, w, 4)
        new_entry[tex.entry_offset] = _build_entry(db.layout, tex.hash, tex.encoding_type, arr,
                                                   tex.has_mipmaps, tex.rle_indicator)
        if db.thumbs:
            new_thumb[tex.thumb_index] = _make_thumb(db.layout, tex, db.thumbs[tex.thumb_index], arr)
        if (w, h) != (tex.width, tex.height):
            resized[tex.index] = (w, h)
    # Rebuild .dat in original physical order
    stored = sorted((t for t in db.textures if not t.is_affiliate), key=lambda t: t.entry_offset)
    dat, remap = bytearray(), {}
    for t in stored:
        remap[t.entry_offset] = len(dat)
        dat += new_entry.get(t.entry_offset, t.entry_bytes)
    toc = struct.pack('<I', len(dat)) + b''.join(
        struct.pack('<i', -1 if t.is_affiliate else remap[t.entry_offset]) for t in db.textures)
    base = os.path.join(out_dir, db.name)
    written = []
    for path, data in ((f'{base}.{db.platform}.dat', bytes(dat)), (f'{base}.{db.platform}.toc', toc)):
        _write_file(path, data)
        written.append(path)
    # Thumbnails: platform .tmb plus shared .unc.tmb
    tmb_paths = [db.tmb_path] if db.thumbs else []
    unc = os.path.join(db.folder, f'{db.name}.unc.tmb')
    if db.platform != 'unc' and os.path.isfile(unc):
        tmb_paths.append(unc)
    for src in tmb_paths:
        with open(src, 'rb') as f:
            blob = f.read()
        thumbs = db.thumbs if src == db.tmb_path else _parse_entries(
            blob, _walk_offsets(blob, db.layout), db.layout, False)
        if len(thumbs) != len(db.thumbs):
            raise ValueError(f"{os.path.basename(src)} entry count differs from {db.tmb_path}")
        data = bytearray()
        for i, th in enumerate(thumbs):
            if i in new_thumb:
                tex = next(t for t in db.textures if t.thumb_index == i)
                val = edited[tex.name]
                rgba, w, h = (val, tex.width, tex.height) if isinstance(val, (bytes, bytearray)) else val
                arr = np.frombuffer(bytes(rgba), dtype=np.uint8).reshape(h, w, 4)
                data += _make_thumb(db.layout, tex, th, arr)
            else:
                data += th.entry_bytes
        dst = os.path.join(out_dir, os.path.basename(src))
        _write_file(dst, bytes(data))
        written.append(dst)
    if not db.txt_path:
        if resized:
            raise ValueError("Resizing needs the set's .txt file")
        return written
    with open(db.txt_path, 'r', newline='') as f:
        text = f.read()
    for idx, (w, h) in resized.items():
        text = _txt_set_size(text, db.textures[idx].txt_props['_line'], w, h)
    dst = os.path.join(out_dir, os.path.basename(db.txt_path))
    with open(dst, 'w', newline='') as f:
        f.write(text)
    written.append(dst)
    return written


def describe_mobile_db(db): #vers 2
    """One-line summary of a loaded database."""
    real = [t for t in db.textures if not t.is_affiliate]
    enc_counts = {}
    for t in real:
        enc_counts[t.encoding_name] = enc_counts.get(t.encoding_name, 0) + 1
    parts = [f'{db.name}.{db.platform}', f'{db.layout.upper()} layout', f'{len(real)} textures']
    if len(db.textures) > len(real):
        parts.append(f'{len(db.textures) - len(real)} affiliates')
    if enc_counts:
        parts.append(', '.join(f'{k}:{v}' for k, v in sorted(enc_counts.items())))
    return ' | '.join(parts)
