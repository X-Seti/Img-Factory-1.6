#this belongs in apps/methods/retro_text.py - Version: 2
# X-Seti - October 24 2026 - IMG Factory 1.6 - GTA1 .fxt / GTA2 .gxt text

"""
Game text of the top-down GTAs.
GTA1 .fxt: '[key]text' strings, 0-terminated, every byte +1; the first 8
bytes also + 0x63, 0xC6, 0x8C ... (0x63 doubled each byte).
GTA2 .gxt: 'GBL' + language letter, u16 version, TKEY chunk (u32 offset into
TDAT + 8-byte key), TDAT chunk (UTF-16LE, 0-terminated). Rebuilt with
strings in their original order, identical strings shared.
"""

##Methods list -
#
##class RetroText: -
# __init__
# _fxt_crypt
# _from_fxt
# _from_gxt
# _to_fxt
# _to_gxt
# find
# from_bytes
# set_text
# to_bytes

import struct
from typing import List


class RetroText: #vers 1
    """entries = [[key, text]] of a .fxt or .gxt; kind 'fxt' / 'gxt'."""

    def __init__(self): #vers 1
        self.kind = 'fxt'
        self.entries: List[List[str]] = []
        self.header = b''
        self.tail = b''
        self.path = ''

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'RetroText': #vers 1
        t = cls()
        t.path = path
        if data[:3] == b'GBL':
            t._from_gxt(data)
        else:
            t._from_fxt(data)
        return t

    @staticmethod
    def _fxt_crypt(data: bytes, sign: int) -> bytes: #vers 1
        """sign -1 decodes, +1 encodes."""
        out = bytearray((b + sign) & 0xFF for b in data)
        v = 0x63
        for i in range(min(8, len(out))):
            out[i] = (out[i] + sign * v) & 0xFF
            v = (v << 1) & 0xFF
        return bytes(out)

    def _from_fxt(self, data: bytes): #vers 1
        self.kind = 'fxt'
        plain = self._fxt_crypt(data, -1)
        end = plain.rfind(b'\0[]')                      # '[]' end marker and what follows
        body, self.tail = (plain[:end + 1], plain[end + 1:]) if end >= 0 else (plain, b'')
        for s in body.split(b'\0'):
            if not s:
                continue
            if not s.startswith(b'[') or b']' not in s:
                raise ValueError("Not a GTA1 .fxt text file")
            k, txt = s[1:].split(b']', 1)
            self.entries.append([k.decode('latin-1'), txt.decode('latin-1')])

    def _from_gxt(self, data: bytes): #vers 1
        self.kind = 'gxt'
        self.header = data[:6]
        chunks, o = {}, 6
        while o + 8 <= len(data):
            tag, n = data[o:o + 4], struct.unpack_from('<I', data, o + 4)[0]
            chunks[tag] = data[o + 8:o + 8 + n]
            o += 8 + n
        if b'TKEY' not in chunks or b'TDAT' not in chunks:
            raise ValueError("GTA2 .gxt without TKEY / TDAT")
        tk, td = chunks[b'TKEY'], chunks[b'TDAT']
        for i in range(len(tk) // 12):
            off, key = struct.unpack_from('<I8s', tk, i * 12)
            end = off
            while end + 1 < len(td) and td[end:end + 2] != b'\0\0':
                end += 2
            self.entries.append([key.split(b'\0')[0].decode('latin-1'),
                                 td[off:end].decode('utf-16-le', 'surrogatepass')])
        self.offs = [struct.unpack_from('<I', tk, i * 12)[0] for i in range(len(tk) // 12)]

    def find(self, key: str) -> int: #vers 1
        return next((i for i, (k, _t) in enumerate(self.entries) if k == key), -1)

    def set_text(self, i: int, text: str): #vers 1
        self.entries[i][1] = text

    def to_bytes(self) -> bytes: #vers 1
        return self._to_gxt() if self.kind == 'gxt' else self._to_fxt()

    def _to_fxt(self) -> bytes: #vers 1
        body = b''.join(b'[' + k.encode('latin-1') + b']' + t.encode('latin-1') + b'\0' for k, t in self.entries)
        return self._fxt_crypt(body + self.tail, 1)

    def _to_gxt(self) -> bytes: #vers 1
        old = list(getattr(self, 'offs', []))
        old += [1 << 32] * (len(self.entries) - len(old))          # added keys go last
        order = sorted(range(len(self.entries)), key=lambda i: (old[i], i))
        td, at, offs = bytearray(), {}, [0] * len(self.entries)
        for i in order:
            key = (old[i], self.entries[i][1]) if old[i] < 1 << 32 else (i, self.entries[i][1])
            if key not in at:                                    # shared only if shared before
                at[key] = len(td)
                td += self.entries[i][1].encode('utf-16-le', 'surrogatepass') + b'\0\0'
            offs[i] = at[key]
        tk = b''.join(struct.pack('<I8s', offs[i], k.encode('latin-1')[:8]) for i, (k, _t) in enumerate(self.entries))
        return (self.header + b'TKEY' + struct.pack('<I', len(tk)) + tk +
                b'TDAT' + struct.pack('<I', len(td)) + bytes(td))
