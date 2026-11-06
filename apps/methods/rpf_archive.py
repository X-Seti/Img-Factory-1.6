#this belongs in apps/methods/rpf_archive.py - Version: 1
# X-Seti - November 06 2026 - IMG Factory 1.6 - GTA IV RPF2 archives

"""
GTA IV .rpf (RPF2) read and write. Header at 0, table of contents at 0x800
(16-byte entries then names, AES-encrypted with the IV IMG key), file data
after. Resources (.wdr / .wft / .wtd ...) are stored with their RSC5 header;
plain files may be zlib-compressed and, in an encrypted archive, are AES'd
too (whole 16-byte blocks). Writing rebuilds the whole archive.
"""

##Methods list -
# _crypt
# pack_data
# read_rpf
# unpack_data
# write_rpf

import os
import struct
import zlib
from typing import List, Tuple

MAGIC = b'RPF2'
TOC_START = 0x800
_DIR = 0x80000000
_COMPRESSED = 0x40000000


def _crypt(buf: bytes, encrypt: bool) -> bytes: #vers 1
    """AES whole 16-byte blocks with the IV key; a short tail stays plain."""
    from apps.core.img_encryption import _process_buffer
    n = len(buf) // 16 * 16
    return _process_buffer(buf[:n], encrypt=encrypt) + buf[n:]


def read_rpf(path: str) -> Tuple[dict, List[dict]]: #vers 1
    """(header, files): files have path, offset, size (stored), usize, type, flags, resource, compressed."""
    with open(path, 'rb') as f:
        magic, toc_size, count, unknown, enc = struct.unpack('<4sIIII', f.read(20))
        if magic != MAGIC:
            raise ValueError("Not a GTA IV RPF2 archive")
        f.seek(TOC_START)
        toc = f.read(toc_size)
    if enc:
        toc = _crypt(toc, False)
    ents = [struct.unpack_from('<IIII', toc, i * 16) for i in range(count)]
    names = toc[count * 16:]

    def name(o):
        end = names.find(b'\0', o)
        return names[o:end if end >= 0 else len(names)].decode('latin-1')
    files, prefix = [], {0: ''}
    for i, (no, a, b, c) in enumerate(ents):
        if b & _DIR:                                     # directory: children b & 0x7FFFFFFF .. + c
            first = b & 0x7FFFFFFF
            base = prefix.get(i, '')
            for k in range(first, first + c):
                prefix[k] = base
            if i:
                sub = (base + name(no) + '/')
                for k in range(first, first + c):
                    prefix[k] = sub
            continue
        resource = bool(c & 0x80000000)
        p = prefix.get(i, '') + name(no)
        if resource:
            files.append({'path': p, 'offset': b & 0xFFFFFF00, 'size': a, 'usize': a, 'type': b & 0xFF,
                          'flags': c, 'resource': True, 'compressed': False, 'index': i, 'encrypted': False})
        else:
            files.append({'path': p, 'offset': b, 'size': c & 0x3FFFFFFF, 'usize': a, 'type': 0, 'flags': c,
                          'resource': False, 'compressed': bool(c & _COMPRESSED), 'index': i,
                          'encrypted': bool(enc)})
    head = {'encrypted': enc, 'unknown': unknown, 'root': name(ents[0][0]) if ents else '/'}
    return head, files


def unpack_data(stored: bytes, info: dict) -> bytes: #vers 1
    """File bytes as tools want them: plain files decrypted / inflated; resources as stored."""
    if info.get('resource'):
        return stored
    data = _crypt(stored, False) if info.get('encrypted') else stored
    if info.get('compressed'):
        try:
            return zlib.decompress(data)
        except zlib.error:
            return zlib.decompress(data, -15)
    return data


def pack_data(data: bytes, encrypted: bool) -> Tuple[bytes, dict]: #vers 1
    """(stored bytes, info) for new file data: resources as given, plain files AES'd when encrypted."""
    if data[:4] == b'RSC\x05':
        return data, {'resource': True}
    return (_crypt(data, True) if encrypted else data), {'resource': False, 'compressed': False,
                                                         'usize': len(data), 'encrypted': bool(encrypted)}


def write_rpf(path: str, head: dict, items: List[Tuple[str, bytes, dict]]) -> None: #vers 1
    """Write a whole RPF2: items (path in archive, stored bytes, info from read_rpf or {})."""
    tree = {'': {'dirs': [], 'files': []}}                # folder -> sub folders, files
    for p, data, info in items:
        folder, _sep, _n = p.rpartition('/')
        parts = folder.split('/') if folder else []
        cur = ''
        for part in parts:
            nxt = f"{cur}{part}/"
            if nxt not in tree:
                tree[nxt] = {'dirs': [], 'files': []}
                tree[cur]['dirs'].append(nxt)
            cur = nxt
        tree[cur]['files'].append((p, data, info))
    order = [('', None)]                                   # breadth first: each folder's children together
    plan = {}
    q, i = [''], 0
    while i < len(q):
        d = q[i]
        i += 1
        kids = [(sd, None) for sd in tree[d]['dirs']] + [(None, f) for f in tree[d]['files']]
        plan[d] = (len(order), len(kids))
        order += kids
        q += tree[d]['dirs']
    names, name_off = bytearray(), {}

    def add_name(n):
        if n not in name_off:
            name_off[n] = len(names)
            names.extend(n.encode('latin-1') + b'\0')
        return name_off[n]
    count = len(order)
    toc_len = count * 16 + sum(len(n.encode('latin-1')) + 1 for n in
                               [head.get('root', '/')] + [(sd or f[0]).rstrip('/').rsplit('/', 1)[-1]
                                                         for sd, f in order[1:]])
    toc_size = (toc_len + 0x7FF) & ~0x7FF
    pos = TOC_START + toc_size
    table, blobs = [], []
    for k, (sd, f) in enumerate(order):
        if k == 0 or sd is not None:
            key = '' if k == 0 else sd
            first, n = plan[key]
            nm = head.get('root', '/') if k == 0 else sd.rstrip('/').rsplit('/', 1)[-1]
            table.append(struct.pack('<IIII', add_name(nm), n, _DIR | first, n))
            continue
        p, data, info = f
        nm = p.rsplit('/', 1)[-1]
        off = pos
        if data[:4] == b'RSC\x05':                         # resource: RSC5 header holds type and flags
            rtype, rflags = struct.unpack_from('<II', data, 4)
            table.append(struct.pack('<IIII', add_name(nm), len(data), off | (rtype & 0xFF), rflags | 0x80000000))
        else:
            comp = bool(info.get('compressed'))
            usize = info.get('usize', len(data)) if comp else len(data)
            table.append(struct.pack('<IIII', add_name(nm), usize, off, len(data) | (_COMPRESSED if comp else 0)))
        blobs.append((off, data))
        pos = (off + len(data) + 0x7FF) & ~0x7FF
    toc = b''.join(table) + bytes(names)
    toc += b'\0' * (toc_size - len(toc))
    if head.get('encrypted'):
        toc = _crypt(toc, True)
    tmp = path + '.tmp'
    with open(tmp, 'wb') as f:
        f.write(struct.pack('<4sIIII', MAGIC, toc_size, count, head.get('unknown', 0), head.get('encrypted', 0)))
        f.write(b'\0' * (TOC_START - 20))
        f.write(toc)
        for off, data in blobs:
            f.seek(off)
            f.write(data)
        end = f.tell()
        f.write(b'\0' * (((end + 0x7FF) & ~0x7FF) - end))
    os.replace(tmp, path)
