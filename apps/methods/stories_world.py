#this belongs in apps/methods/stories_world.py - Version: 1
# X-Seti - October 21 2026 - IMG Factory 1.6 - LCS/VCS level world sectors

"""
LCS/VCS PS2 level worlds: .LVZ (zlib WRLD sector directory) + level .IMG
(sector chunks holding building geometry and instances). Layout per librwgta.
"""

##Methods list -
# _half
# _strip_batches
# building_geometry
# level_files
# level_world
# StoriesLevel.__init__
# StoriesLevel._sector
# StoriesLevel.origin

import os
import struct
import zlib
from typing import Dict, List, Optional, Tuple

import numpy as np

# game -> (level names, sectors x, sectors y, x step, y step, x start, y start)
_GRID = {'lcs': (('INDUST', 'COMMER', 'SUBURB', 'UNDERG'), 40, 46, 100.0, 86.6, -2000.0, -2000.0),
         'vcs': (('BEACH', 'MAINLA', 'MALL'), 32, 36, 125.0, 108.25, -2400.0, -2000.0)}
_WRLD = b'DLRW'
_INST_SIZE = 0x50


def _half(h: int) -> float: #vers 1
    """IEEE half (16 bit) to float."""
    return float(np.frombuffer(struct.pack('<H', h), dtype=np.float16)[0])


def level_files(root: str, game: str) -> List[Tuple[str, str, str]]: #vers 1
    """(level name, .LVZ path, .IMG path) found under an LCS/VCS PS2 root."""
    from apps.methods.stories_dtz import _ci
    out = []
    for name in _GRID[game][0]:
        for base in ('MODELS/', ''):
            lvz, img = _ci(root, f"{base}{name}.LVZ"), _ci(root, f"{base}{name}.IMG")
            if lvz and img:
                out.append((name, lvz, img))
                break
    return out


def _strip_batches(d: bytes, o: int, end: int, uvs: Tuple[float, float]): #vers 2
    """PS2 VIF strip batches of one building mesh -> list of (pos, uv, col) arrays."""
    from apps.methods.lcs_geometry import _vif_commands
    out, cur = [], None
    for at, _size, c, num, _imm in _vif_commands(d, o, end):
        if c == 0x6C:                                # batch header qword: vertex count
            cur = {'n': struct.unpack_from('<I', d, at + 16)[0] & 0x7FFF}
        elif cur is not None and c >= 0x60:
            cur[{0x9: 'pos', 0x6: 'uv', 0xF: 'col'}.get(c & 0xF, 'other')] = at + 4
        elif cur is not None and c == 0x14:          # MSCAL: batch complete
            n = cur['n']
            pos = np.frombuffer(d, '<i2', n * 3, cur['pos']).reshape(-1, 3) / 32767.0
            uv = np.frombuffer(d, 'u1', n * 2, cur['uv']).reshape(-1, 2) / 128.0 * np.array(uvs)
            cc = np.frombuffer(d, '<u2', n, cur['col']).astype(np.int32)
            col = np.stack([(cc & 0x1F) * 255 // 31, (cc >> 5 & 0x1F) * 255 // 31,
                            (cc >> 10 & 0x1F) * 255 // 31, np.where(cc & 0x8000, 255, 0)], 1)
            out.append((pos, uv, col))
            cur = None
    return out


def building_geometry(d: bytes, g: int, vcs: bool, tex_name=None): #vers 1
    """sBuildingGeometry at offset g -> dff_classes Geometry (tristrips to triangles)."""
    from apps.methods.dff_classes import BoundingSphere, Geometry, Material, RGBA, TexCoord, Triangle, Vector3
    num, size = struct.unpack_from('<2H', d, g)
    m = g + 4
    data = m + size
    mesh_size = 24 if vcs else 22
    mats: Dict[int, int] = {}
    P, U, C, T = [], [], [], []
    base = 0
    for i in range(num):
        r = m + i * mesh_size
        if vcs:
            packet, tex = struct.unpack_from('<iH', d, r)
            packet >>= 1
            uv_o = r + 6
        else:
            tex, packet = struct.unpack_from('<2H', d, r)
            packet &= 0x7FFF
            uv_o = r + 4
        uvs = (_half(struct.unpack_from('<H', d, uv_o)[0]), _half(struct.unpack_from('<H', d, uv_o + 2)[0]))
        mid = mats.setdefault(tex, len(mats))
        for pos, uv, col in _strip_batches(d, data, data + packet, uvs):
            n = len(pos)
            if n >= 3:
                k = np.arange(n - 2)
                a, b, c = k, k + 1, k + 2
                ok = ~(np.all(pos[a] == pos[b], 1) | np.all(pos[b] == pos[c], 1) | np.all(pos[a] == pos[c], 1))
                odd = (k % 2).astype(bool)
                a2, b2 = np.where(odd, b, a), np.where(odd, a, b)    # alternate winding
                tri = np.stack([a2, b2, c], 1)[ok] + base
                T.append(np.column_stack([tri, np.full(len(tri), mid)]))
            P.append(pos); U.append(uv); C.append(col)
            base += n
        data += packet
    geo = Geometry()
    if P:
        P, U, C = np.vstack(P), np.vstack(U), np.vstack(C)
        geo.vertices = [Vector3(*v) for v in P.tolist()]
        geo.uv_layers = [[TexCoord(*t) for t in U.tolist()]]
        geo.colors = [RGBA(*c) for c in C.tolist()]
    if T:
        geo.triangles = [Triangle(*t) for t in np.vstack(T).tolist()]
    for t, _i in sorted(mats.items(), key=lambda kv: kv[1]):
        mat = Material(texture_name=tex_name(t) if tex_name else f"tex_{t:04x}")
        mat.colour = mat.color                       # viewport reads .colour
        geo.materials.append(mat)
    if len(geo.vertices):
        lo, hi = P.min(0), P.max(0)
        centre = (lo + hi) / 2.0
        geo.bounding_sphere = BoundingSphere(Vector3(*centre.tolist()),
                                             float(np.linalg.norm(P - centre, axis=1).max()))
    return geo


def level_world(level, passes=(2, 3, 4, 5, 6)): #vers 1
    """Unique world-sector instances of one level: (models, instances, textures).
    models {name: DFFModel}; instances [(name, pos, quat, scale)]; textures {name: tex dict}."""
    from apps.methods.dff_classes import Atomic, DFFModel, Frame
    from apps.methods.stories_dtz import _entity_quat
    from apps.methods.xtx_reader import _raster_try, decode_raster
    tag = level.name.lower()
    nl = 9 if level.vcs else 8
    if level.vcs:                                   # VCS lists: UNDERWATER after SUPERLOD
        passes = tuple(p + 1 for p in passes)
    seen, inst, rids, overlay = set(), [], set(), {}
    with open(level.img_path, 'rb') as img:
        for n in range(len(level.headers)):
            s = level._sector(n, img)
            if len(s) < 32 + 8 + nl * 4:
                continue
            rp, nr = struct.unpack_from('<IH', s, 32)
            for k in range(nr):                     # resources streamed with this sector
                rid, raw = struct.unpack_from('<iI', s, rp + k * 8)
                overlay.setdefault(rid, (n, raw))
            if n >= level.world_count:
                continue
            ox, oy = level.sector_origins[n]
            ps = struct.unpack_from(f'<{nl}I', s, 40)
            for li in passes:
                for o in range(ps[li], ps[li + 1], _INST_SIZE):
                    rid = struct.unpack_from('<H', s, o + 2)[0]
                    m = np.array(struct.unpack_from('<16f', s, o + 16), dtype=np.float64)
                    m[12] += ox
                    m[13] += oy
                    key = (rid,) + tuple(np.round(m, 2))
                    if key in seen:
                        continue                    # same piece listed by every sector it overlaps
                    seen.add(key)
                    rows = m.reshape(4, 4)[:3, :3]
                    scale = np.linalg.norm(rows, axis=1)
                    unit = rows / np.where(scale == 0, 1, scale)[:, None]
                    q = _entity_quat(tuple(np.concatenate([np.c_[unit, np.zeros(3)].ravel(), m[12:]])))
                    inst.append((f"{tag}_{rid:04x}", tuple(float(v) for v in m[12:15]),
                                 tuple(float(v) for v in q), tuple(float(v) for v in scale)))
                    rids.add(rid)
    d, stride = level.lvz, level.res_stride
    sectors = {}
    areas = {}                                      # VCS: resources streamed per area chunk
    if level.vcs:
        num_areas, area_tab = struct.unpack_from('<iI', d, 0x2F0)
        with open(level.img_path, 'rb') as img:
            for a in range(max(0, num_areas)):
                foff, fsize = struct.unpack_from('<2I', d, area_tab + a * 16 + 4)
                img.seek(foff)
                chunk = img.read(fsize)
                n_res, res_ptr = struct.unpack_from('<iI', chunk, 32)
                for k in range(n_res):
                    rid, _id2, raw = struct.unpack_from('<hhI', chunk, res_ptr + k * 8)
                    areas.setdefault(rid, (chunk, raw))

    def _res(rid):                                  # (bytes, offset) of a resource, or None
        raw = struct.unpack_from('<I', d, level.res_table + rid * stride)[0]
        if raw:
            return d, raw
        if rid in overlay:
            n, raw = overlay[rid]
            if n not in sectors:
                with open(level.img_path, 'rb') as img:
                    sectors[n] = level._sector(n, img)
            return sectors[n], raw
        return areas.get(rid)

    models, tex_ids = {}, set()
    for rid in sorted(rids):
        src = _res(rid)
        if src is None:
            continue
        geo = building_geometry(src[0], src[1], level.vcs, lambda t: f"{tag}_t{t:04x}")
        tex_ids.update(int(mt.texture_name.rsplit('_t', 1)[1], 16) for mt in geo.materials)
        models[f"{tag}_{rid:04x}"] = DFFModel(frames=[Frame(name=f"{tag}_{rid:04x}")], geometries=[geo],
                                               atomics=[Atomic(0, 0)], source_path=level.img_path)
    textures = {}
    kind = 'PS2_VCS' if level.vcs else 'PS2_LCS'
    for tid in sorted(tex_ids):
        src = _res(tid)
        lay = _raster_try(src[0], src[1], kind, len(src[0]), strict=False) if src else None
        if lay is None:
            continue
        rgba = decode_raster(src[0], lay)[0]
        name = f"{tag}_t{tid:04x}"
        textures[name] = {'name': name, 'width': lay['width'], 'height': lay['height'],
                          'rgba_data': rgba, 'has_alpha': False, 'format': 'PAL8' if lay['depth'] == 8 else 'PAL4',
                          'mipmaps': 1, '_group': 'stories'}
    inst = [i for i in inst if i[0] in models]
    return models, inst, textures


class StoriesLevel: #vers 1
    """One LCS/VCS PS2 level: sector directory (.LVZ) + sector chunks (.IMG)."""

    def __init__(self, name: str, lvz_path: str, img_path: str, game: str): #vers 1
        with open(lvz_path, 'rb') as f:
            self.lvz = zlib.decompress(f.read())
        if self.lvz[:4] != _WRLD:
            raise ValueError(f"{os.path.basename(lvz_path)} is not a WRLD level")
        self.name, self.img_path, self.game = name, img_path, game
        self.vcs = game == 'vcs'
        names, self.nx, self.ny, self.xinc, self.yinc, self.xs, self.ys = _GRID[game]
        u = lambda o: struct.unpack_from('<I', self.lvz, o)[0]
        self.res_table = u(0x20)
        self.rows = [(u(0x24 + i * 8), struct.unpack_from('<i', self.lvz, 0x28 + i * 8)[0])
                     for i in range(self.ny + 1)]
        self.res_stride = 12 if self.vcs else 8
        # sector headers: contiguous WRLD chunk headers from row 0
        self.headers = []
        h = self.rows[0][0]
        while self.lvz[h:h + 4] == _WRLD:
            self.headers.append(h)
            h += 32
        self.world_count = (self.rows[self.ny][0] - self.rows[0][0]) // 32
        self.sector_origins = []                    # world sector index -> grid origin
        for sy in range(self.ny):
            for j in range((self.rows[sy + 1][0] - self.rows[sy][0]) // 32):
                self.sector_origins.append(self.origin(self.rows[sy][1] + j, sy))

    def origin(self, sx: int, sy: int) -> Tuple[float, float]: #vers 1
        """World position of a grid sector (odd rows shifted half a step)."""
        return (self.xs + self.xinc / 2 + self.xinc * sx - (sy & 1) * self.xinc / 2,
                self.ys + self.yinc / 2 + self.yinc * sy)

    def _sector(self, n: int, img) -> Optional[bytes]: #vers 1
        """Sector chunk n with its 32 byte header in front (pointers are offsets into it)."""
        h = self.headers[n]
        file_size, glob = struct.unpack_from('<I', self.lvz, h + 8)[0], struct.unpack_from('<I', self.lvz, h + 24)[0]
        img.seek(glob)
        body = img.read(file_size - 32)
        return self.lvz[h:h + 32] + body
