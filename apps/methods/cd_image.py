#this belongs in apps/methods/cd_image.py - Version: 2
# X-Seti - October 23 2026 - IMG Factory 1.6 - CD / DVD disc image read and write

"""
Disc images: CloneCD / raw .img and .bin (2352-byte sectors, Mode 1 or
Mode 2 as used by PS1) and plain .iso (2048-byte sectors). Lists the
ISO 9660 file tree, extracts files (XA / STR video and audio as raw
2336-byte sectors, CD audio tracks as .wav) and replaces files in place when they fit the sectors
they already use; directory sizes (ISO 9660 and Joliet) and sector EDC /
ECC are rewritten. Bigger files and new files go after the last sector when
the data track ends the image (volume size and a CloneCD .ccd lead-out are
updated); new files need room in their folder's directory sectors.
"""

##Methods list -
# _ecc_block
# _edc
# _tables
# is_cd_image
# sector_fix
#
##class CDImage: -
# __init__
# _alias_dir
# _append
# _ccd_leadout
# _kind
# _new_sector
# _put_record
# _raw
# _read_dir
# _tail_is_data
# _user
# _write_user
# add_file
# entries
# extract_all
# find
# grow
# read
# replace
# save

import os
import struct
from typing import Dict, List, Optional

RAW, USER, FORM2 = 2352, 2048, 2324
SYNC = b'\x00' + b'\xff' * 10 + b'\x00'

_EDC = _ECC_F = _ECC_B = None


def _tables(): #vers 1
    """EDC (CRC) and ECC (GF 2^8) lookup tables."""
    global _EDC, _ECC_F, _ECC_B
    if _EDC is not None:
        return
    edc, f, b = [0] * 256, [0] * 256, [0] * 256
    for i in range(256):
        j = ((i << 1) ^ (0x11D if i & 0x80 else 0)) & 0xFF
        f[i] = j
        b[i ^ j] = i
        e = i
        for _k in range(8):
            e = (e >> 1) ^ (0xD8018001 if e & 1 else 0)
        edc[i] = e
    _EDC, _ECC_F, _ECC_B = edc, f, b


def _edc(data) -> int: #vers 1
    _tables()
    e = 0
    for c in data:
        e = (e >> 8) ^ _EDC[(e ^ c) & 0xFF]
    return e


def _ecc_block(src, major_count, minor_count, major_mult, minor_inc) -> bytes: #vers 1
    """P or Q parity bytes over src (sector from offset 12)."""
    size = major_count * minor_count
    out = bytearray(major_count * 2)
    f, b = _ECC_F, _ECC_B
    for major in range(major_count):
        index = (major >> 1) * major_mult + (major & 1)
        a = c = 0
        for _m in range(minor_count):
            t = src[index]
            index += minor_inc
            if index >= size:
                index -= size
            a ^= t
            c ^= t
            a = f[a]
        a = b[f[a] ^ c]
        out[major] = a
        out[major + major_count] = a ^ c
    return bytes(out)


def sector_fix(sec: bytearray): #vers 1
    """Recompute EDC / ECC of a raw 2352-byte sector (Mode 1, Mode 2 Form 1 / 2)."""
    _tables()
    mode = sec[15]
    if mode == 1:
        sec[0x810:0x814] = struct.pack('<I', _edc(sec[0:0x810]))
        sec[0x814:0x81C] = bytes(8)
        zero_addr = False
    elif mode == 2 and sec[18] & 0x20:                     # Form 2: EDC only
        sec[0x92C:0x930] = struct.pack('<I', _edc(sec[0x10:0x92C]))
        return
    elif mode == 2:
        sec[0x818:0x81C] = struct.pack('<I', _edc(sec[0x10:0x818]))
        zero_addr = True
    else:
        return
    addr = bytes(sec[12:16])
    if zero_addr:
        sec[12:16] = bytes(4)
    sec[0x81C:0x8C8] = _ecc_block(memoryview(sec)[0xC:], 86, 24, 2, 86)
    sec[0x8C8:0x930] = _ecc_block(memoryview(sec)[0xC:], 52, 43, 86, 88)
    sec[12:16] = addr


def is_cd_image(path: str) -> bool: #vers 1
    """True for a raw (2352) or ISO (2048) disc image with an ISO 9660 volume."""
    try:
        with open(path, 'rb') as f:
            head = f.read(16)
            if head[:12] == SYNC:
                f.seek(16 * RAW)
                s = f.read(RAW)
                off = 24 if s[15] == 2 else 16
                return s[off + 1:off + 6] == b'CD001'
            f.seek(16 * USER)
            return f.read(6)[1:6] == b'CD001'
    except OSError:
        return False


class CDImage: #vers 1
    """ISO 9660 file tree of a disc image; in-place file replace."""

    def __init__(self, path: str): #vers 3
        self.path = path
        self._f = open(path, 'rb')
        head = self._f.read(16)
        self.raw = head[:12] == SYNC
        self.sector = RAW if self.raw else USER
        self.total = os.path.getsize(path) // self.sector
        self._dirty: Dict[int, bytes] = {}                  # lba -> raw / user sector
        self.mode = self._raw(16)[15] if self.raw else 0
        pvd = self._user(16)
        if pvd[1:6] != b'CD001':
            raise ValueError("No ISO 9660 volume in this image")
        self.volume = pvd[40:72].decode('latin-1').strip()
        self.system = pvd[8:40].decode('latin-1').strip()
        self._entries: List[dict] = []
        root = pvd[156:190]
        self._root = (struct.unpack_from('<I', root, 2)[0], struct.unpack_from('<I', root, 10)[0])
        self._read_dir(self._root[0], self._root[1], '')
        self._alias: Dict[int, list] = {}                   # file lba -> Joliet records (sector, offset)
        self._vds = [16]                                    # volume descriptors holding the volume size
        self.joliet = False
        for n in range(17, 32):                             # supplementary (Joliet) trees
            vd = self._user(n)
            if vd[1:6] != b'CD001' or vd[0] == 255:
                break
            self._vds.append(n)
            if vd[0] == 2:
                self.joliet = True
                jr = vd[156:190]
                self._alias_dir(struct.unpack_from('<I', jr, 2)[0], struct.unpack_from('<I', jr, 10)[0], 0)

    def _raw(self, lba: int) -> bytes: #vers 1
        if lba in self._dirty:
            return self._dirty[lba]
        self._f.seek(lba * self.sector)
        return self._f.read(self.sector)

    def _user(self, lba: int) -> bytes: #vers 1
        """2048 user bytes of a sector."""
        s = self._raw(lba)
        if not self.raw:
            return s
        off = 24 if s[15] == 2 else 16
        return s[off:off + USER]

    def _read_dir(self, lba: int, size: int, path: str): #vers 1
        for n in range((size + USER - 1) // USER):
            d = self._user(lba + n)
            o = 0
            while o < USER and d[o]:
                ln = d[o]
                e_lba, e_size = struct.unpack_from('<I', d, o + 2)[0], struct.unpack_from('<I', d, o + 10)[0]
                flags, nl = d[o + 25], d[o + 32]
                name = d[o + 33:o + 33 + nl].decode('latin-1').split(';')[0]
                if name not in ('\x00', '\x01'):
                    full = f"{path}/{name}" if path else name
                    kind = 'dir' if flags & 2 else self._kind(name, e_lba, e_size)
                    self._entries.append({'path': full, 'lba': e_lba, 'size': e_size, 'dir': bool(flags & 2),
                                          'kind': kind, 'form2': kind == 'form2', 'rec': (lba + n, o)})
                    if flags & 2:
                        self._read_dir(e_lba, e_size, full)
                o += ln

    def _alias_dir(self, lba: int, size: int, depth: int): #vers 1
        """Record where a second (Joliet) tree keeps each file's size."""
        if depth > 16:
            return
        for n in range((size + USER - 1) // USER):
            d = self._user(lba + n)
            o = 0
            while o < USER and d[o]:
                e_lba, e_size = struct.unpack_from('<I', d, o + 2)[0], struct.unpack_from('<I', d, o + 10)[0]
                if d[o + 32] == 1 and d[o + 33] in (0, 1):
                    pass                                     # . and ..
                elif d[o + 25] & 2:
                    self._alias_dir(e_lba, e_size, depth + 1)
                else:
                    self._alias.setdefault(e_lba, []).append((lba + n, o))
                o += d[o]

    def _kind(self, name: str, lba: int, size: int) -> str: #vers 1
        """'data', 'form2' (STR / XA: raw 2336-byte sectors) or 'audio' (CD audio track)."""
        if not self.raw:
            return 'data'
        if self._raw(lba)[:12] != SYNC:
            return 'audio'
        if name.upper().endswith(('.STR', '.XA')):
            return 'form2'
        for i in range(min(32, (size + USER - 1) // USER)):
            s = self._raw(lba + i)
            if s[15] == 2 and s[18] & 0x20:
                return 'form2'
        return 'data'

    def entries(self) -> List[dict]: #vers 1
        return self._entries

    def find(self, path: str) -> Optional[dict]: #vers 1
        p = path.lower().strip('/')
        return next((e for e in self._entries if e['path'].lower() == p), None)

    def read(self, e: dict) -> bytes: #vers 2
        """File bytes; STR / XA as raw 2336-byte sectors; CD audio as a .wav (44.1 kHz stereo)."""
        secs = (e['size'] + USER - 1) // USER
        if e.get('kind') == 'audio':
            pcm = b''.join(self._raw(e['lba'] + i) for i in range(secs))
            return (b'RIFF' + struct.pack('<I', 36 + len(pcm)) + b'WAVEfmt ' +
                    struct.pack('<IHHIIHH', 16, 1, 2, 44100, 176400, 4, 16) +
                    b'data' + struct.pack('<I', len(pcm)) + pcm)
        if e['form2']:
            return b''.join(self._raw(e['lba'] + i)[16:16 + 2336] for i in range(secs))
        return b''.join(self._user(e['lba'] + i) for i in range(secs))[:e['size']]

    def extract_all(self, out: str) -> int: #vers 1
        """Write the whole tree under out; returns the file count."""
        n = 0
        for e in self._entries:
            p = os.path.join(out, *e['path'].split('/'))
            if e['dir']:
                os.makedirs(p, exist_ok=True)
                continue
            os.makedirs(os.path.dirname(p), exist_ok=True)
            with open(p, 'wb') as f:
                f.write(self.read(e))
            n += 1
        return n

    def _write_user(self, lba: int, data: bytes): #vers 1
        """Put 2048 user bytes (or a raw 2336 Form 2 block) into a sector."""
        if not self.raw:
            self._dirty[lba] = data.ljust(USER, b'\0')
            return
        sec = bytearray(self._raw(lba))
        if len(data) == 2336:                              # raw Form 2 block: subheader + data
            sec[16:16 + 2336] = data
        else:
            off = 24 if sec[15] == 2 else 16
            sec[off:off + USER] = data.ljust(USER, b'\0')
        sector_fix(sec)
        self._dirty[lba] = bytes(sec)

    def replace(self, e: dict, data: bytes): #vers 3
        """Replace a file in place; it must fit the sectors it already uses."""
        if e['dir']:
            raise ValueError("Can't replace a folder")
        if e.get('kind') == 'audio':
            raise ValueError(f"{e['path']}: CD audio tracks are not replaced here")
        have = (e['size'] + USER - 1) // USER
        if e['form2']:
            if len(data) % 2336:
                raise ValueError(f"{e['path']}: XA / STR data is raw 2336-byte sectors")
            need = len(data) // 2336
            new_size = need * USER
        else:
            need = (len(data) + USER - 1) // USER
            new_size = len(data)
        if need > have:
            raise ValueError(f"{e['path']}: {need} sectors needed, only {have} on the disc for it")
        for i in range(have):
            if e['form2']:
                chunk = data[i * 2336:(i + 1) * 2336] if i < need else bytes(self._raw(e['lba'] + i)[16:24]) + bytes(2328)
            else:
                chunk = data[i * USER:(i + 1) * USER] if i < need else b''
            self._write_user(e['lba'] + i, chunk)
        for dl, do in [e['rec']] + self._alias.get(e['lba'], []):   # directory record sizes (LE + BE)
            d = bytearray(self._user(dl))
            d[do + 10:do + 18] = struct.pack('<I', new_size) + struct.pack('>I', new_size)
            self._write_user(dl, bytes(d))
        e['size'] = new_size

    def _tail_is_data(self) -> bool: #vers 1
        """True when the image ends with the data track (new sectors can follow it)."""
        return not self.raw or self._raw(self.total - 1)[:12] == SYNC

    def _new_sector(self, lba: int, data: bytes, sub: bytes = b'', last: bool = False): #vers 1
        """A sector after the end: Mode 1 / Mode 2 Form 1 user data, or a raw 2336 Form 2 block."""
        if not self.raw:
            self._dirty[lba] = data.ljust(USER, b'\0')
            return
        m = lba + 150
        msf = bytes(((v // 10) << 4) | (v % 10) for v in (m // 4500, (m // 75) % 60, m % 75))
        sec = bytearray(RAW)
        sec[0:12] = SYNC
        sec[12:15] = msf
        sec[15] = self.mode
        if len(data) == 2336:
            sec[16:16 + 2336] = data
        elif self.mode == 2:
            sh = bytearray(sub[:4] if sub else bytes([0, 0, 0x08, 0]))
            sh[2] = (sh[2] & ~0x81 & 0xFF) | (0x81 if last else 0)
            sec[16:24] = bytes(sh) * 2
            sec[24:24 + USER] = data.ljust(USER, b'\0')
        else:
            sec[16:16 + USER] = data.ljust(USER, b'\0')
        sector_fix(sec)
        self._dirty[lba] = bytes(sec)

    def _put_record(self, rec, lba: int, size: int): #vers 1
        """Start sector and size (both byte orders) of a directory record."""
        dl, do = rec
        d = bytearray(self._user(dl))
        d[do + 2:do + 18] = struct.pack('<I', lba) + struct.pack('>I', lba) + struct.pack('<I', size) + struct.pack('>I', size)
        self._write_user(dl, bytes(d))

    def _append(self, data: bytes, form2: bool, sub: bytes) -> tuple: #vers 1
        """Write data in new sectors after the end; returns (first lba, record size)."""
        if not self._tail_is_data():
            raise ValueError("The image ends with audio tracks - no room after the data track")
        lba = self.total
        if form2:
            if len(data) % 2336:
                raise ValueError("XA / STR data is raw 2336-byte sectors")
            n, size = len(data) // 2336, len(data) // 2336 * USER
            for i in range(n):
                self._new_sector(lba + i, data[i * 2336:(i + 1) * 2336])
        else:
            n, size = max(1, (len(data) + USER - 1) // USER), len(data)
            for i in range(n):
                self._new_sector(lba + i, data[i * USER:(i + 1) * USER], sub, i == n - 1)
        self.total += n
        for v in self._vds:                                 # volume space size
            d = bytearray(self._user(v))
            d[80:88] = struct.pack('<I', self.total) + struct.pack('>I', self.total)
            self._write_user(v, bytes(d))
        return lba, size

    def grow(self, e: dict, data: bytes): #vers 1
        """Replace a file; when it no longer fits its sectors it moves after the end of the image."""
        try:
            self.replace(e, data)
            return
        except ValueError as ex:
            if 'sectors needed' not in str(ex):
                raise
        sub = self._raw(e['lba'])[16:20] if self.raw and self.mode == 2 else b''
        aliases = self._alias.pop(e['lba'], [])
        lba, size = self._append(data, e['form2'], sub)
        for rec in [e['rec']] + aliases:
            self._put_record(rec, lba, size)
        if aliases:
            self._alias[lba] = aliases
        e['lba'], e['size'] = lba, size

    def add_file(self, folder: str, name: str, data: bytes) -> dict: #vers 1
        """New file in a folder ('' = root), written after the end of the image."""
        if self.joliet:
            raise ValueError("Disc has a Joliet tree - adding files is not supported, replace / grow only")
        if self.find(f"{folder}/{name}" if folder else name):
            raise ValueError(f"{name} is already on the disc")
        if folder:
            fe = self.find(folder)
            if fe is None or not fe['dir']:
                raise ValueError(f"No folder {folder} on the disc")
            dlba, dsize = fe['lba'], fe['size']
        else:
            dlba, dsize = self._root
        iso = (name.upper() + ';1').encode('latin-1')
        sib = next((x for x in self._entries if not x['dir'] and x['path'].rpartition('/')[0] == folder), None)
        su = b''
        if sib is not None:                                 # copy the XA system-use area of a sibling
            sd = self._user(sib['rec'][0])
            so = sib['rec'][1]
            base = 33 + sd[so + 32] + (1 - sd[so + 32] % 2)
            su = bytes(sd[so + base:so + sd[so]])
        base = 33 + len(iso) + (1 - len(iso) % 2)           # name padded to an even record offset
        rlen = base + len(su) + (base + len(su)) % 2
        for n in range((dsize + USER - 1) // USER):
            d = bytearray(self._user(dlba + n))
            o = 0
            while o < USER and d[o]:
                o += d[o]
            if USER - o >= rlen:
                break
        else:
            raise ValueError(f"No room for another file in folder '{folder or '/'}'")
        form2 = name.upper().endswith(('.STR', '.XA'))
        lba, size = self._append(data, form2, self._raw(sib['lba'])[16:20] if sib and self.raw and self.mode == 2 else b'')
        dot = self._user(dlba)
        rec = bytearray(rlen)
        rec[0] = rlen
        rec[18:25] = dot[18:25]                             # date from the folder's '.' record
        rec[28:32] = struct.pack('<H', 1) + struct.pack('>H', 1)
        rec[32] = len(iso)
        rec[33:33 + len(iso)] = iso
        rec[base:base + len(su)] = su
        d[o:o + rlen] = rec
        self._write_user(dlba + n, bytes(d))
        self._put_record((dlba + n, o), lba, size)
        e = {'path': f"{folder}/{name}" if folder else name, 'lba': lba, 'size': size, 'dir': False,
             'kind': 'form2' if form2 else 'data', 'form2': form2, 'rec': (dlba + n, o)}
        self._entries.append(e)
        return e

    def _ccd_leadout(self, target: str): #vers 1
        """CloneCD .ccd next to target: lead-out (point A2) moved to the new image end."""
        ccd = os.path.splitext(target)[0] + '.ccd'
        if not self.raw or not os.path.isfile(ccd):
            return
        with open(ccd, encoding='latin-1') as f:
            lines = f.read().split('\n')
        m = self.total + 150
        vals = {'PMin': m // 4500, 'PSec': (m // 75) % 60, 'PFrame': m % 75, 'PLBA': self.total}
        in_a2 = False
        for i, ln in enumerate(lines):
            s = ln.strip()
            if s.startswith('['):
                in_a2 = False
            elif s.lower() == 'point=0xa2':
                in_a2 = True
            elif in_a2 and '=' in s and s.split('=')[0] in vals:
                lines[i] = f"{s.split('=')[0]}={vals[s.split('=')[0]]}" + ('\r' if ln.endswith('\r') else '')
        with open(ccd, 'w', encoding='latin-1', newline='') as f:
            f.write('\n'.join(lines))

    def save(self, path: str = ''): #vers 2
        """Write changed sectors (into this image, or a copy at path); new sectors extend it."""
        target = path or self.path
        if target != self.path:
            import shutil
            shutil.copyfile(self.path, target)
            src_ccd, dst_ccd = os.path.splitext(self.path)[0] + '.ccd', os.path.splitext(target)[0] + '.ccd'
            if os.path.isfile(src_ccd) and not os.path.exists(dst_ccd):
                shutil.copyfile(src_ccd, dst_ccd)
        with open(target, 'r+b') as f:
            for lba, s in sorted(self._dirty.items()):
                f.seek(lba * self.sector)
                f.write(s)
        self._ccd_leadout(target)
        if target == self.path:
            self._f.close()
            self._f = open(self.path, 'rb')
            self._dirty.clear()
