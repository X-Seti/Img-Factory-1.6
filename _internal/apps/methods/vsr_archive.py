#this belongs in apps/methods/vsr_archive.py - Version: 1
# X-Seti - October 23 2026 - IMG Factory 1.6 - PS1 GTA1 / London .VSR container

"""
PS1 GTA1 / London .vsr resource container ('3RSV'): header, OFFS / DIRC /
VSRN / STRT chunks (tags stored backwards), a directory of 40-byte entries
(folder entries hold a child count, data entries an offset / size / kind /
four parameters) and a name table, then the data blobs, each with an
8-byte prefix (4-char tag, size). Blobs are read by folder / name and the
file is rebuilt with every blob in its original order.
"""

##Methods list -
#
##class VSRArchive: -
# __init__
# blob
# entry
# from_bytes
# names
# set_blob
# to_bytes

import struct
from typing import Dict, List, Optional, Tuple

FOLDER, REF = 0x16, 0x15
KIND = {0x0E: 'data', 0x16: 'folder', 0x19: 'texture page', 0x1A: 'texture index', 0x1B: 'clut page'}


class VSRArchive: #vers 1
    """Entries (folder, name, kind, params, data) of a .vsr; rebuildable."""

    def __init__(self): #vers 1
        self.header = b''                       # file start up to the first blob
        self.dirc_at = 0
        self.entries: List[dict] = []
        self.path = ''
        self.syncs = []                         # callables run before to_bytes (map / style edits)

    @classmethod
    def from_bytes(cls, data: bytes, path: str = '') -> 'VSRArchive': #vers 1
        if data[:4] != b'3RSV':
            raise ValueError("Not a PS1 GTA .vsr file")
        a = cls()
        a.path = path
        o, chunks = 0x24, {}
        while o + 8 <= len(data):
            tag = data[o:o + 4][::-1].decode('latin-1')
            n = struct.unpack_from('<I', data, o + 4)[0]
            chunks[tag] = (o + 8, n)
            o += 8 + n
            if tag == 'STRT':
                break
        d_at, d_size = chunks['DIRC']
        s_at, _s = chunks['STRT']
        count = struct.unpack_from('<I', data, s_at)[0]
        offs = struct.unpack_from(f'<{count}I', data, s_at + 4)
        names = [data[s_at + x:data.index(b'\0', s_at + x)].decode('latin-1') for x in offs]
        a.dirc_at = d_at
        folder, left, first = '', 0, None
        for e_at in range(d_at + 4, d_at + d_size - 39, 40):
            off, size, name_i, _idx, kind = struct.unpack_from('<IIHHH', data, e_at)
            params = struct.unpack_from('<4I', data, e_at + 24)
            name = names[name_i] if name_i < len(names) else f"#{name_i}"
            e = {'name': name, 'kind': kind, 'params': params, 'raw': data[e_at:e_at + 40],
                 'folder': '' if kind == FOLDER else folder, 'tag': b'', 'data': b''}
            if kind == FOLDER:
                folder, left = name, params[0]
            elif kind & 0xFF == REF:                       # reference to another .vsr (GTAMOG)
                e['kind'] = kind
            else:
                e['tag'] = data[off:off + 4]
                e['data'] = data[off + 8:off + 8 + size]
                first = off if first is None else min(first, off)
                left -= 1
                if left <= 0:
                    folder = ''
            a.entries.append(e)
        a.header = data[:first] if first is not None else data   # index-only files (GTAMOG) have no blobs
        return a

    def names(self) -> List[str]: #vers 1
        return [f"{e['folder']}/{e['name']}" if e['folder'] else e['name'] for e in self.entries
                if e['kind'] != FOLDER]                     # references listed too

    def entry(self, folder: str, name: str) -> Optional[dict]: #vers 1
        return next((e for e in self.entries if e['kind'] != FOLDER and e['folder'] == folder
                     and e['name'] == name), None)

    def blob(self, folder: str, name: str) -> bytes: #vers 1
        e = self.entry(folder, name)
        if e is None:
            raise KeyError(f"{folder}/{name} not in {self.path}")
        return e['data']

    def set_blob(self, folder: str, name: str, data: bytes): #vers 1
        self.entry(folder, name)['data'] = bytes(data)

    def to_bytes(self) -> bytes: #vers 1
        """Rebuild: blobs packed in entry order, directory offsets / sizes and header sizes updated."""
        for sync in self.syncs:
            sync()
        head = bytearray(self.header)
        body = bytearray()
        at = len(head)
        for i, e in enumerate(self.entries):
            if e['kind'] == FOLDER or e['kind'] & 0xFF == REF:
                continue
            off = at + len(body)
            body += e['tag'] + struct.pack('<I', len(e['data'])) + e['data']
            struct.pack_into('<II', head, self.dirc_at + 4 + i * 40, off, len(e['data']))
        if not body:
            return bytes(head)
        struct.pack_into('<I', head, 4, len(head) + len(body) - 8)          # file size - 8
        struct.pack_into('<I', head, 0x14, len(body))                       # data size
        if head[-8:-4] == b'ATAD':                                          # DATA chunk size
            struct.pack_into('<I', head, len(head) - 4, len(body))
        return bytes(head + body)
