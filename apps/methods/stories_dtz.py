#this belongs in apps/methods/stories_dtz.py - Version: 1
# X-Seti - October 08 2026 - IMG Factory 1.6 - LCS/VCS GAME.DTZ reader

"""
GTA LCS/VCS (PS2/PSP) GAME.DTZ: zlib'd relocatable resource image
holding model infos, entity pools, streaming directory and water.
Layouts follow g3DTZ by guard3 (MIT).
"""

##Methods list -
# _ci
# _entity_quat
# _modelinfo
# _pool
# _streaming_infos
# dtz_paths
# find_stories_root
# StoriesDTZ.__init__
# StoriesDTZ.img_entries
# StoriesDTZ.instances
# StoriesDTZ.model_name
# StoriesDTZ.objects
# StoriesDTZ.path_links
# StoriesDTZ.txd_name
# StoriesDTZ.waterpro_bytes

import math
import os
import struct
import zlib
from typing import Dict, List, Optional, Tuple

_HEADER = 32
_ENTITY = 96
_TEXLIST = 28
_MI_TYPES = {1: 'objs', 3: 'tobj', 4: 'weap', 5: 'hier', 6: 'cars', 7: 'peds'}
# Resource image field index of CWaterLevel*
_WATER_FIELD = {'lcs': 33, 'vcs': 34}
# Streaming: LCS fixed arrays (mdl, tex, col); VCS pointer + offsets
_LCS_COUNTS = (4900, 1200, 15)

# game key -> (dtz path, IMG name) per platform folder layout
_DTZ_LAYOUTS = (
    ('lcs', 'ps2', 'CHK/PS2/GAME.DTZ', 'MODELS/GTA3PS2.IMG'),
    ('lcs', 'psp', 'chk/psphr/game.dtz', 'models/gta3psphr.img'),
    ('vcs', 'ps2', 'PS2/GAME.DTZ', 'GTA3PS2.IMG'),
    ('vcs', 'psp', 'psp/game.dtz', 'gta3psp.img'),
)


def _ci(base: str, rel: str) -> Optional[str]: #vers 1
    """Case-insensitive path under base, or None."""
    cur = base
    for part in rel.split('/'):
        try:
            hit = next((e for e in os.listdir(cur) if e.lower() == part.lower()), None)
        except OSError:
            return None
        if hit is None:
            return None
        cur = os.path.join(cur, hit)
    return cur


def dtz_paths(root: str) -> Optional[Tuple[str, str, str, str]]: #vers 1
    """(game, platform, dtz path, img path) for a console LCS/VCS root."""
    for game, plat, dtz, img in _DTZ_LAYOUTS:
        d, i = _ci(root, dtz), _ci(root, img)
        if d and i:
            return game, plat, d, i
    return None


def find_stories_root(path: str) -> str: #vers 1
    """Walk up from a file/folder to the LCS/VCS game root, or ''."""
    p = os.path.abspath(path)
    if os.path.isfile(p):
        p = os.path.dirname(p)
    for _ in range(4):
        if dtz_paths(p):
            return p
        parent = os.path.dirname(p)
        if parent == p:
            break
        p = parent
    return ''


def _entity_quat(m: Tuple[float, ...]) -> Tuple[float, float, float, float]: #vers 1
    """RslMatrix rows to IPL quaternion (inverted, as g3DTZ writes)."""
    m00, m01, m02, m10, m11, m12, m20, m21, m22 = m[0], m[1], m[2], m[4], m[5], m[6], m[8], m[9], m[10]
    tr = m00 + m11 + m22
    if tr > 0.0:
        sq = math.sqrt(1.0 + tr); rp = 0.5 / sq
        q = (rp * (m12 - m21), rp * (m20 - m02), rp * (m01 - m10), sq * 0.5)
    elif m00 > m11 and m00 > m22:
        sq = math.sqrt(1.0 + m00 - m11 - m22); rp = 0.5 / sq
        q = (sq * 0.5, rp * (m10 + m01), rp * (m20 + m02), rp * (m12 - m21))
    elif m11 > m22:
        sq = math.sqrt(1.0 + m11 - m00 - m22); rp = 0.5 / sq
        q = (rp * (m10 + m01), sq * 0.5, rp * (m21 + m12), rp * (m20 - m02))
    else:
        sq = math.sqrt(1.0 + m22 - m00 - m11); rp = 0.5 / sq
        q = (rp * (m20 + m02), rp * (m21 + m12), sq * 0.5, rp * (m01 - m10))
    return -q[0] + 0.0, -q[1] + 0.0, -q[2] + 0.0, q[3]


class StoriesDTZ: #vers 1
    """Parsed GAME.DTZ for one LCS/VCS PS2/PSP build."""

    def __init__(self, path: str, game: str, platform: str): #vers 1
        with open(path, 'rb') as f:
            raw = f.read()
        self.data = raw if raw[:4] == b'GATG' else zlib.decompress(raw)
        if self.data[:4] != b'GATG':
            raise ValueError(f"{os.path.basename(path)} is not a GAME.DTZ resource image")
        self.game, self.platform, self.path = game, platform, path
        self.vcs = game == 'vcs'
        self._named_pools = self.vcs or platform == 'ps2'
        self.fields = struct.unpack_from('<40I', self.data, _HEADER)
        from apps.core.lcs_vcs_names import LCS_NAMES, VCS_NAMES
        self._names = VCS_NAMES if self.vcs else LCS_NAMES
        n, ptrs = self.fields[6], self.fields[7]
        self._mi = [struct.unpack_from('<I', self.data, ptrs + i * 4)[0] for i in range(n)]
        tex = self._pool(self.fields[16], _TEXLIST)
        self._texlists = {i: self.data[p + 8:p + 28].split(b'\0', 1)[0].decode('latin-1')
                          for i, p in tex}

    def _u32(self, off: int) -> int:
        return struct.unpack_from('<I', self.data, off)[0]

    def _pool(self, ptr: int, stride: int) -> List[Tuple[int, int]]: #vers 1
        """(slot index, entry offset) for every used slot of a CPool."""
        if not ptr:
            return []
        entries, flags, size = struct.unpack_from('<3I', self.data, ptr)
        return [(i, entries + i * stride) for i in range(size) if not self.data[flags + i] & 0x80]

    def _modelinfo(self, idx: int) -> Optional[int]: #vers 1
        """CBaseModelInfo offset for a model index, or None."""
        return self._mi[idx] or None if 0 <= idx < len(self._mi) else None

    def model_name(self, idx: int) -> str: #vers 1
        """Model name (cracked hash table, else hash_XXXXXXXX)."""
        p = self._modelinfo(idx)
        if p is None:
            return ''
        h = self._u32(p + 8)
        return self._names.get(h) or f"hash_{h:08X}"

    def txd_name(self, slot: int) -> str: #vers 1
        """Texlist (TXD) name for a texlist store slot."""
        return self._texlists.get(slot, '')

    def objects(self) -> List[dict]: #vers 1
        """Model infos as IDE-style dicts (id, name, txd, section, extra)."""
        out = []
        base = 36 if self.vcs else 32                  # vtable end -> CSimpleModelInfo
        for idx, p in enumerate(self._mi):
            if not p:
                continue
            mtype = self.data[p + 16]
            section = _MI_TYPES.get(mtype, 'objs')
            tex = struct.unpack_from('<h', self.data, p + 30)[0]
            extra = {}
            if mtype in (1, 3, 4):
                lods = struct.unpack_from('<3f', self.data, p + base + 8)
                num = self.data[p + base + 20]
                flags = struct.unpack_from('<H', self.data, p + base + 22)[0]
                extra = {'mesh_count': num, 'draw_dist': lods[0],
                         'flags': ((flags >> 2) & 1) | ((flags & 0xFFE0) >> 4)}
                if num > 1:
                    extra['draw_dist2'] = lods[1]
                if mtype == 3:
                    extra['time_on'], extra['time_off'] = struct.unpack_from('<2i', self.data, p + base + 28)
            out.append({'id': idx, 'name': self.model_name(idx), 'txd': self.txd_name(tex),
                        'section': section, 'extra': extra})
        return out

    def instances(self) -> List[dict]: #vers 1
        """Building, treadable and dummy pool entities as IPL-style dicts."""
        out = []
        for field, label in ((1, 'buildings'), (2, 'treadables'), (3, 'dummys')):
            for _i, e in self._pool(self.fields[field], _ENTITY):
                idx = struct.unpack_from('<h', self.data, e + 88)[0]
                name = self.model_name(idx)
                if not name:
                    continue
                m = struct.unpack_from('<16f', self.data, e)
                out.append({'id': idx, 'name': name, 'pos': m[12:15],
                            'rot': _entity_quat(m), 'area': self.data[e + 91], 'pool': label})
        return out

    def _streaming_infos(self) -> List[Tuple[str, int, int]]: #vers 1
        """(kind, cd position, cd size) for model, texture and col slots."""
        s = self.fields[23]
        if not self.vcs:
            base, step, kinds = s + 4, 20, []
            for kind, count in zip(('mdl', 'tex', 'col'), _LCS_COUNTS):
                kinds += [kind] * count
            off = 12
        else:
            tex_off, col_off, anm_off, total = struct.unpack_from('<4i', self.data, s + 8)
            base = self._u32(s + (200 if self.platform == 'ps2' else 152))
            step, off = (24, 16) if self.platform == 'ps2' else (20, 12)
            kinds = ['mdl'] * tex_off + ['tex'] * (col_off - tex_off) + ['col'] * (anm_off - col_off)
        out = []
        for i, kind in enumerate(kinds):
            pos, size = struct.unpack_from('<II', self.data, base + i * step + off)
            out.append((kind, pos, size))
        return out

    def img_entries(self) -> List[Tuple[str, int, int]]: #vers 1
        """Streamed IMG directory: (name, offset sectors, size sectors), by offset."""
        col_stride, col_name = (72, 40) if self.vcs else (52, 24)
        cols = {i: self.data[p + col_name:p + col_name + 20].split(b'\0', 1)[0].decode('latin-1')
                for i, p in self._pool(self.fields[18], col_stride)}
        tex_ext = '.xtx' if self.vcs else '.chk'
        counters = {'mdl': 0, 'tex': 0, 'col': 0}
        out = []
        for kind, pos, size in self._streaming_infos():
            i = counters[kind]
            counters[kind] += 1
            if not size:
                continue
            if kind == 'mdl':
                name = self.model_name(i) + '.mdl'
            elif kind == 'tex':
                name = self.txd_name(i) + tex_ext
            else:
                name = cols.get(i, f'col{i}') + '.col2'
            if name[0] != '.':
                out.append((name, pos, size))
        out.sort(key=lambda e: e[1])
        return out

    def path_links(self) -> List[Tuple[str, Tuple[float, float, float], Tuple[float, float, float]]]: #vers 1
        """CPathFind graph as ('car'|'ped', node a, node b) links, each once."""
        P = self.fields[0]
        if not P:
            return []
        d = self.data
        if self.vcs:
            nodes = self._u32(P)
            n, n_car = struct.unpack_from('<2i', d, P + 12)
            n_conn = struct.unpack_from('<h', d, P + 26)[0]
            conns, stride = self._u32(P + (0x7BA0 if self.platform == 'ps2' else 0x7558)), 10   # PSP: no 0x648 pad
        else:
            nodes, conns = self._u32(P), self._u32(P + 8)
            n, n_car = struct.unpack_from('<2i', d, P + 20)
            n_conn = struct.unpack_from('<h', d, P + 34)[0]
            stride = 20
        pts, first = [], []
        for i in range(n):
            o = nodes + i * stride
            if self.vcs:
                x, y = struct.unpack_from('<2h', d, o)
                pts.append((x / 8.0, y / 8.0, float(struct.unpack_from('<b', d, o + 4)[0])))
                first.append(struct.unpack_from('<h', d, o + 6)[0])
            else:
                x, y, z = struct.unpack_from('<3h', d, o + 4)
                pts.append((x / 8.0, y / 8.0, z / 8.0))
                first.append(struct.unpack_from('<h', d, o + 12)[0])
        first.append(n_conn & 0xFFFF)
        out, seen = [], set()
        for i in range(n):
            for k in range(first[i], first[i + 1]):
                j = struct.unpack_from('<H', d, conns + k * 2)[0] & 0x3FFF
                key = (min(i, j), max(i, j))
                if j < n and key not in seen:
                    seen.add(key)
                    out.append(('car' if i < n_car else 'ped', pts[i], pts[j]))
        return out

    def waterpro_bytes(self) -> bytes: #vers 1
        """CWaterLevel as waterpro.dat bytes (count, 48 Zs, 48 rects, grids)."""
        w = self._u32(_HEADER + _WATER_FIELD[self.game] * 4)
        if not w:
            return b''
        num, zs, rects = struct.unpack_from('<iII', self.data, w)
        grids = self.data[w + 12:w + 12 + 64 * 64 + 128 * 128]
        return (struct.pack('<i', num) + self.data[zs:zs + 48 * 4]
                + self.data[rects:rects + 48 * 16] + grids)
