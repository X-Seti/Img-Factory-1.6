#this belongs in apps/methods/ifp_format.py - Version: 1
# X-Seti - October 21 2026 - IMG Factory 1.6 - IFP animation read / write

"""
GTA IFP animation packages: ANPK (III / VC) and ANP3 (SA). Read into
IFPFile / IFPAnim / IFPBone / IFPKey and write back (unchanged files
come back byte-identical). Rotations are held in ANP3 order; ANPK
quaternions are conjugated on read and write.
"""

##Methods list -
# _cstr
# _pad4
# build_ifp
# load_ifp
# parse_ifp
# save_ifp
#
##class IFPBone: -
# sample
#
##class IFPAnim: -
# bone
# duration
#
##class IFPKey: -
# copy

import struct
from dataclasses import dataclass, field
from typing import List, Optional, Tuple

# ANP3 frame types: 1 KR00, 2 KRT0, 3 KR00 compressed, 4 KRT0 compressed
_SA_TYPE = {1: ('KR00', False), 2: ('KRT0', False), 3: ('KR00', True), 4: ('KRT0', True)}


@dataclass
class IFPKey: #vers 1
    """One keyframe: time (s), rotation quat (x, y, z, w), optional trans / scale."""
    time: float
    rot: Tuple[float, float, float, float]
    trans: Optional[Tuple[float, float, float]] = None
    scale: Optional[Tuple[float, float, float]] = None

    def copy(self): #vers 1
        return IFPKey(self.time, tuple(self.rot), self.trans and tuple(self.trans),
                      self.scale and tuple(self.scale))


@dataclass
class IFPBone: #vers 1
    """Keys for one bone (matched by bone_id on SA, name on III / VC)."""
    name: str
    bone_id: int = -1
    kind: str = 'KR00'               # KR00 / KRT0 / KRTS
    compressed: bool = False
    keys: List[IFPKey] = field(default_factory=list)
    name_raw: bytes = b''
    extra: bytes = b''               # ANPK ANIM fields after frame count

    def sample(self, t: float): #vers 1
        """(rot quat, trans or None) at time t, slerp / lerp between keys."""
        ks = self.keys
        if not ks:
            return None, None
        if t <= ks[0].time or len(ks) == 1:
            return ks[0].rot, ks[0].trans
        if t >= ks[-1].time:
            return ks[-1].rot, ks[-1].trans
        lo, hi = 0, len(ks) - 1
        while hi - lo > 1:
            mid = (lo + hi) // 2
            if ks[mid].time <= t:
                lo = mid
            else:
                hi = mid
        a, b = ks[lo], ks[hi]
        f = (t - a.time) / max(b.time - a.time, 1e-9)
        qa, qb = a.rot, b.rot
        dot = sum(x * y for x, y in zip(qa, qb))
        if dot < 0:
            qb, dot = tuple(-x for x in qb), -dot
        if dot > 0.9995:
            q = [x + (y - x) * f for x, y in zip(qa, qb)]
        else:
            import math
            th = math.acos(min(1.0, dot))
            s = math.sin(th)
            wa, wb = math.sin((1 - f) * th) / s, math.sin(f * th) / s
            q = [x * wa + y * wb for x, y in zip(qa, qb)]
        n = sum(x * x for x in q) ** 0.5 or 1.0
        tr = None
        if a.trans is not None and b.trans is not None:
            tr = tuple(x + (y - x) * f for x, y in zip(a.trans, b.trans))
        return tuple(x / n for x in q), tr


@dataclass
class IFPAnim: #vers 1
    name: str
    bones: List[IFPBone] = field(default_factory=list)
    name_raw: bytes = b''
    extra: bytes = b''               # ANPK DGAN INFO tail / ANP3 trailing int

    def duration(self) -> float: #vers 1
        return max((b.keys[-1].time for b in self.bones if b.keys), default=0.0)

    def bone(self, name: str = '', bone_id: int = -1) -> Optional[IFPBone]: #vers 1
        for b in self.bones:
            if bone_id >= 0 and b.bone_id == bone_id:
                return b
        n = name.lower()
        return next((b for b in self.bones if b.name.lower() == n), None)


@dataclass
class IFPFile:
    version: str                     # 'ANPK' or 'ANP3'
    name: str
    anims: List[IFPAnim] = field(default_factory=list)
    name_raw: bytes = b''
    path: str = ''


def _cstr(raw: bytes) -> str: #vers 1
    return raw.split(b'\0', 1)[0].decode('latin-1')


def _pad4(n: int) -> int: #vers 1
    return (n + 3) & ~3


def parse_ifp(data: bytes) -> IFPFile: #vers 1
    """Parse ANPK or ANP3 bytes."""
    tag = data[:4]
    if tag == b'ANP3':
        return _parse_anp3(data)
    if tag == b'ANPK':
        return _parse_anpk(data)
    raise ValueError(f"not an IFP file ({tag!r})")


def _parse_anp3(d: bytes) -> IFPFile:
    name_raw = d[8:32]
    count = struct.unpack_from('<i', d, 32)[0]
    f = IFPFile('ANP3', _cstr(name_raw), name_raw=name_raw)
    o = 36
    for _ in range(count):
        a_raw = d[o:o + 24]
        nobj, _size, unk = struct.unpack_from('<3i', d, o + 24)
        o += 36
        anim = IFPAnim(_cstr(a_raw), name_raw=a_raw, extra=struct.pack('<i', unk))
        for _ in range(nobj):
            b_raw = d[o:o + 24]
            ftype, nframes, bid = struct.unpack_from('<3i', d, o + 24)
            o += 36
            kind, comp = _SA_TYPE[ftype]
            bone = IFPBone(_cstr(b_raw), bid, kind, comp, name_raw=b_raw)
            for _ in range(nframes):
                if comp:
                    qx, qy, qz, qw, t = struct.unpack_from('<5h', d, o)
                    o += 10
                    tr = None
                    if kind == 'KRT0':
                        tr = tuple(v / 1024.0 for v in struct.unpack_from('<3h', d, o))
                        o += 6
                    bone.keys.append(IFPKey(t / 60.0, (qx / 4096.0, qy / 4096.0, qz / 4096.0, qw / 4096.0), tr))
                else:
                    q = struct.unpack_from('<4f', d, o)
                    o += 16
                    tr = None
                    if kind == 'KRT0':
                        tr = struct.unpack_from('<3f', d, o)
                        o += 12
                    t = struct.unpack_from('<f', d, o)[0]
                    o += 4
                    bone.keys.append(IFPKey(t, q, tr))
            anim.bones.append(bone)
        f.anims.append(anim)
    return f


def _parse_anpk(d: bytes) -> IFPFile:
    def chunk(o, want):
        tag, size = d[o:o + 4], struct.unpack_from('<I', d, o + 4)[0]
        if tag != want:
            raise ValueError(f"IFP: expected {want!r} at {o:#x}, got {tag!r}")
        return o + 8, size

    o, _ = chunk(0, b'ANPK')
    p, size = chunk(o, b'INFO')
    count = struct.unpack_from('<i', d, p)[0]
    name_raw = d[p + 4:p + size]
    f = IFPFile('ANPK', _cstr(name_raw), name_raw=name_raw)
    o = p + _pad4(size)
    for _ in range(count):
        p, size = chunk(o, b'NAME')
        anim = IFPAnim(_cstr(d[p:p + size]), name_raw=d[p:p + size])
        o = p + _pad4(size)
        p, dg_size = chunk(o, b'DGAN')
        end = p + dg_size
        q, size = chunk(p, b'INFO')
        nobj = struct.unpack_from('<i', d, q)[0]
        anim.extra = d[q + 4:q + size]
        o = q + _pad4(size)
        for _ in range(nobj):
            p, _cp = chunk(o, b'CPAN')
            q, size = chunk(p, b'ANIM')
            b_raw = d[q:q + 28]
            nframes = struct.unpack_from('<i', d, q + 28)[0]
            extra = d[q + 32:q + size]
            bone = IFPBone(_cstr(b_raw), -1, name_raw=b_raw, extra=extra)   # matched by name
            o = q + _pad4(size)
            if nframes:
                kind = d[o:o + 4].decode('latin-1')
                ksize = struct.unpack_from('<I', d, o + 4)[0]
                bone.kind = kind
                k = o + 8
                for _ in range(nframes):
                    qx, qy, qz, qw = struct.unpack_from('<4f', d, k)
                    rot = (-qx, -qy, -qz, qw)                  # ANPK stores the conjugate
                    k += 16
                    tr = sc = None
                    if kind in ('KRT0', 'KRTS'):
                        tr = struct.unpack_from('<3f', d, k)
                        k += 12
                    if kind == 'KRTS':
                        sc = struct.unpack_from('<3f', d, k)
                        k += 12
                    t = struct.unpack_from('<f', d, k)[0]
                    k += 4
                    bone.keys.append(IFPKey(t, rot, tr, sc))
                o += 8 + _pad4(ksize)
            anim.bones.append(bone)
        f.anims.append(anim)
        o = end
    return f


def build_ifp(f: IFPFile) -> bytes: #vers 1
    """Serialise an IFPFile in its own version."""
    return _build_anp3(f) if f.version == 'ANP3' else _build_anpk(f)


def _fixed(raw: bytes, name: str, n: int) -> bytes:
    """Original padded name bytes when the name is unchanged."""
    if raw and _cstr(raw) == name and len(raw) == n:
        return raw
    return name.encode('latin-1')[:n - 1].ljust(n, b'\0')


def _build_anp3(f: IFPFile) -> bytes:
    out = bytearray()
    for a in f.anims:
        body = bytearray()
        for b in a.bones:
            ftype = {('KR00', False): 1, ('KRT0', False): 2, ('KR00', True): 3, ('KRT0', True): 4}[
                (b.kind if b.kind != 'KRTS' else 'KRT0', b.compressed)]
            body += _fixed(b.name_raw, b.name, 24) + struct.pack('<3i', ftype, len(b.keys), b.bone_id)
            for k in b.keys:
                if b.compressed:
                    body += struct.pack('<5h', *(int(round(v * 4096.0)) for v in k.rot), int(round(k.time * 60.0)))
                    if ftype == 4:
                        body += struct.pack('<3h', *(int(round(v * 1024.0)) for v in (k.trans or (0, 0, 0))))
                else:
                    body += struct.pack('<4f', *k.rot)
                    if ftype == 2:
                        body += struct.pack('<3f', *(k.trans or (0, 0, 0)))
                    body += struct.pack('<f', k.time)
        size = len(body) - 36 * len(a.bones)
        unk = a.extra if len(a.extra) == 4 else struct.pack('<i', 1)
        out += _fixed(a.name_raw, a.name, 24) + struct.pack('<2i', len(a.bones), size) + unk + body
    head = _fixed(f.name_raw, f.name, 24) + struct.pack('<i', len(f.anims))
    payload = head + out
    return b'ANP3' + struct.pack('<I', len(payload)) + payload


def _chunk(tag: bytes, payload: bytes, size: Optional[int] = None) -> bytes:
    n = len(payload) if size is None else size
    return tag + struct.pack('<I', n) + payload.ljust(_pad4(len(payload)), b'\0')


def _build_anpk(f: IFPFile) -> bytes:
    name = f.name_raw if _cstr(f.name_raw) == f.name else f.name.encode('latin-1') + b'\0'
    body = _chunk(b'INFO', struct.pack('<i', len(f.anims)) + name)
    for a in f.anims:
        nm = a.name_raw if _cstr(a.name_raw) == a.name else a.name.encode('latin-1') + b'\0'
        body += _chunk(b'NAME', nm)
        dg = _chunk(b'INFO', struct.pack('<i', len(a.bones)) + a.extra)
        for b in a.bones:
            extra = b.extra or struct.pack('<3i', 0, 0, 0)
            anim = _chunk(b'ANIM', _fixed(b.name_raw, b.name, 28) + struct.pack('<i', len(b.keys)) + extra)
            keys = b''
            if b.keys:
                kb = bytearray()
                for k in b.keys:
                    kb += struct.pack('<4f', -k.rot[0], -k.rot[1], -k.rot[2], k.rot[3])
                    if b.kind in ('KRT0', 'KRTS'):
                        kb += struct.pack('<3f', *(k.trans or (0, 0, 0)))
                    if b.kind == 'KRTS':
                        kb += struct.pack('<3f', *(k.scale or (1, 1, 1)))
                    kb += struct.pack('<f', k.time)
                keys = _chunk(b.kind.encode('latin-1'), bytes(kb))
            dg += _chunk(b'CPAN', anim + keys)
        body += _chunk(b'DGAN', dg)
    return _chunk(b'ANPK', body)


def load_ifp(path: str) -> IFPFile: #vers 1
    with open(path, 'rb') as fh:
        f = parse_ifp(fh.read())
    f.path = path
    return f


def save_ifp(f: IFPFile, path: str = '') -> str: #vers 1
    """Write with a backup of the old file; returns the path."""
    from apps.methods.file_backup import safe_write_bytes
    path = path or f.path
    safe_write_bytes(path, build_ifp(f), f"IFP: {f.name}")
    f.path = path
    return path
