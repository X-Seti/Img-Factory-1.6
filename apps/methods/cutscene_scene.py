#this belongs in apps/methods/cutscene_scene.py - Version: 10
# X-Seti - November 02 2026 - IMG Factory 1.6 - Cutscene playback scene

"""
Cutscene playback: the .cut 'model' list loaded (DFF + TXD), each posed by
its animation from the cutscene .ifp at a time, as viewport geometry.
The player (csplay) is built from models/player.img parts (default outfit,
clothes.dat CUTS swaps); a stand-in ped when player.img isn't found. III / VC have no .cut (the script is
in main.scm): the models are then the IFP's animation names. GTA IV: one .wad
per .cut section; peds are .wdd components on the .wft skeleton, moved by
the anim's mover track; props (.wdr) by their root bone.
"""

##Methods list -
#
##class CutsceneScene: -
# __init__
# _add_item
# _bone_map
# _geo_of
# _item_ltms
# _iv_item
# _locals
# _player_parts
# actor_at
# actor_paths
# from_iv
# geoms_at
# move_actor
# textures

from typing import Callable, List, Optional

import numpy as np

from apps.methods.dff_parser import parse_model_any
from apps.methods.dff_skin import quat_to_matrix, read_skeleton
from apps.methods.ifp_format import parse_ifp

PLAYER_STAND_IN = 'bmybar'                      # SA ped used when csplay has no model
_ALIAS = {'spine': 'spine 1', 'spine1': 'spine 2',   # in-game ped names -> cutscene skeleton
          'ripped': 'root'}                          # player.img parts: root frame is 'Ripped'


class CutsceneScene: #vers 1
    """Models, skeletons and animations of one cutscene."""

    def __init__(self, sections, ifp_bytes: Optional[bytes], fetch: Callable[[str, str], Optional[bytes]],
                 clothes_rules: Optional[dict] = None): #vers 5
        """sections: parse_cut output; fetch(name, '.dff' / '.txd') -> bytes or None;
        clothes_rules: parse_clothes_dat output (player cutscene parts)."""
        self.items, self.notes, self._tex = [], [], []
        self.lift = 0.0                                  # SA: models sit 1 m above the anim root
        self._fetch, self._rules = fetch, clothes_rules
        ifp = parse_ifp(ifp_bytes) if ifp_bytes else None
        self.ifp = ifp                                   # kept for actor route edits
        anims = {a.name.lower(): a for a in (ifp.anims if ifp else [])}
        self.duration = max((a.duration() for a in anims.values()), default=0.0)
        rows = dict(sections).get('model', [])
        if not rows and ifp is not None:                 # III / VC: one anim per model
            rows = [f"1,{a.name},{a.name}" for a in ifp.anims]
        for row in rows:
            parts = [p.strip() for p in row.split(',')]
            if len(parts) < 2 or not parts[1]:
                continue
            name = parts[1]
            anim = anims.get((parts[2] if len(parts) > 2 and parts[2] else name).lower())
            if anim is None:
                self.notes.append(f"{name}: no animation")
            dff = fetch(name, '.dff')
            if dff is None and name.lower() == 'csplay':
                if self._player_parts(name, anim):
                    continue
                dff = fetch(PLAYER_STAND_IN, '.dff')
                if dff is not None:
                    self.notes.append(f"{name}: stand-in {PLAYER_STAND_IN}")
                    self._add_item(name, dff, fetch(PLAYER_STAND_IN, '.txd'), anim)
                    continue
            if dff is None:
                self.notes.append(f"{name}: model not found")
                continue
            self._add_item(name, dff, fetch(name, '.txd'), anim)

    def _player_parts(self, name: str, anim) -> bool: #vers 1
        """CJ from player.img: default outfit with cutscene parts; False if no parts found."""
        from apps.methods.cj_player import DEFAULT_OUTFIT, resolve_outfit
        found = False
        for comp, model, tex in resolve_outfit(DEFAULT_OUTFIT, self._rules, cutscene=True):
            dff = self._fetch(model, '.dff')
            if dff is None:
                continue
            txd = self._fetch(tex, '.txd') if tex else None
            found = self._add_item(f"{name}:{comp}", dff, txd, anim, skin_all=bool(txd)) or found
        return found

    def _add_item(self, name: str, dff: bytes, txd: Optional[bytes], anim, skin_all: bool = False) -> bool: #vers 2
        """Parse one model (+ textures) into a scene item. skin_all: its TXD's first texture on
        every material (player parts carry no texture names)."""
        model = parse_model_any(dff, name)
        if model is None or not model.geometries:
            self.notes.append(f"{name}: model not readable")
            return False
        texs = []
        if txd:
            from apps.methods.txd_parser import parse_txd
            texs = parse_txd(txd)
            self._tex += texs
        if skin_all and texs:
            for g in model.geometries:
                for m in g.materials:
                    if not getattr(m, 'texture_name', ''):
                        m.texture_name = texs[0]['name']
        sk = read_skeleton(dff)
        self.items.append({'name': name, 'sk': sk, 'anim': anim, 'geo': self._geo_of(model),
                           'bones': self._bone_map(sk, anim)})
        return True

    @staticmethod
    def _geo_of(model) -> dict: #vers 1
        """{geometry index: (verts, normals, uvs, tris, prelit, materials, flags)} arrays."""
        geo = {}
        for gi, g in enumerate(model.geometries):
            geo[gi] = (np.array([(v.x, v.y, v.z) for v in g.vertices], dtype=np.float64),
                       np.array([(n.x, n.y, n.z) for n in g.normals], dtype=np.float64) if g.normals else None,
                       [(u.u, u.v) for u in g.uv_layers[0]] if g.uv_layers else [],
                       [(t.v1, t.v2, t.v3, t.material_id) for t in g.triangles],
                       [(c.r, c.g, c.b, c.a) for c in g.colors] if g.colors else [],
                       g.materials, getattr(g, 'flags', 0))
        return geo

    @classmethod
    def from_iv(cls, parts: list, wads: dict, fetch: Callable[[str, str], Optional[bytes]],
                names=frozenset()): #vers 2
        """GTA IV scene: parts from iv_cut_parts, wads {anim name lower: .wad bytes}, fetch(name, ext);
        names: file names (lower) in the model archives (Niko's parts in playerped.rpf)."""
        from apps.methods.iv_anim import parse_wad, wad_extra_tracks
        self = cls.__new__(cls)
        self.items, self.notes, self._tex, self.lift = [], [], [], 0.0
        self._fetch, self._rules, self._names = fetch, None, names
        by_model, t0 = {}, 0.0
        for part in parts:
            raw = wads.get(part['anim'].lower())
            if raw is None:
                self.notes.append(f"{part['anim']}.wad not found")
                t0 += part['duration']
                continue
            anims = {a.name.lower(): a for a in parse_wad(raw, part['anim']).anims}
            extra = wad_extra_tracks(raw)
            dur = part['duration'] or max((a.duration() for a in anims.values()), default=0.0)
            for idx, name, body, face in part['models']:
                key = name.lower()
                if key not in by_model:
                    var = [v for v in part['variation'] if v and str(v[0]) == idx]
                    it = self._iv_item(name, var)
                    by_model[key] = it
                    if it is not None:
                        self.items.append(it)
                it = by_model[key]
                if it is None:
                    continue
                bones = self._bone_map(it['sk'], anims.get(body.lower()))
                if face:
                    bones.update(self._bone_map(it['sk'], anims.get(face.lower())))
                mover = extra.get(body.lower(), {})
                it['segs'].append((t0, t0 + dur, bones, mover.get('mover_pos'), mover.get('mover_rot')))
            t0 += dur
        self.duration = t0
        return self

    def _iv_item(self, name: str, variation: list): #vers 4
        """Scene item of an IV ped (.wdd + .wft), prop (.wdr) or fragment / car (.wft); None if missing."""
        from apps.methods.dff_skin import Skeleton
        from apps.methods.wdr_model import parse_wdr, parse_wft
        from apps.methods.xtd_textures import parse_iv_wtd
        wdd, wft, wdr = self._fetch(name, '.wdd'), self._fetch(name, '.wft'), self._fetch(name, '.wdr')
        wtd = self._fetch(name, '.wtd')
        texs = parse_iv_wtd(wtd) if wtd else []
        self._tex += texs
        if wdd is not None and wft is not None:
            from apps.methods.iv_anim import iv_skeleton, iv_skins
            from apps.methods.ped_files import iv_default_picks, iv_ped_model
            model = iv_ped_model(wdd, iv_default_picks(wdd, [t['name'] for t in texs], variation), name)
            sk = iv_skeleton(wft)
            iv_skins(sk, model.geometries)
        elif name.lower() == 'player' and wft is not None and any(n.endswith('.wdr') for n in self._names):
            from apps.methods.iv_anim import iv_skeleton, iv_skins          # Niko: playerped.rpf
            from apps.methods.ped_files import iv_player_model
            model, ptex = iv_player_model(self._names, self._fetch, variation)
            self._tex += ptex
            sk = iv_skeleton(wft)
            iv_skins(sk, model.geometries)
        elif wdr is not None or wft is not None:
            from apps.methods.wdr_model import wdr_embedded_textures
            model = parse_wdr(wdr, name) if wdr is not None else parse_wft(wft, name)
            self._tex += wdr_embedded_textures(wdr if wdr is not None else wft)   # props carry their textures
            if wdr is None and not getattr(self, '_vehshare', False) and any(
                    m.texture_name.lower().startswith('vehicle_generic') for g in model.geometries for m in g.materials):
                self._vehshare = True                         # cars: shared vehicle textures
                share = self._fetch('vehshare', '.wtd')
                self._tex += parse_iv_wtd(share) if share else []
            sk = Skeleton()                                   # props: whole model on the root bone
            sk.frames.append({'name': name, 'local': np.eye(4), 'node_id': 0, 'parent': -1})
            sk.atomics = {gi: 0 for gi in range(len(model.geometries))}
        else:
            self.notes.append(f"{name}: model not found" + (" (Niko: needs models/cdimages/playerped.rpf)"
                                                             if name.lower() == 'player' else ""))
            return None
        if not model.geometries:
            self.notes.append(f"{name}: model not readable")
            return None
        return {'name': name, 'sk': sk, 'anim': None, 'geo': self._geo_of(model), 'bones': {}, 'segs': []}

    @staticmethod
    def _bone_map(sk, anim) -> dict: #vers 1
        """Frame index -> IFP bone: by id, name, 'Object:name' suffix, or ped alias."""
        out = {}
        if anim is None:
            return out
        by_name = {}
        for b in anim.bones:
            n = b.name.strip().lower()
            by_name.setdefault(n, b)
            by_name.setdefault(n.split(':')[-1], b)
        for i, f in enumerate(sk.frames):
            b = next((x for x in anim.bones if f['node_id'] >= 0 and x.bone_id == f['node_id']), None)
            n = f['name'].strip().lower()
            b = b or by_name.get(n) or by_name.get(_ALIAS.get(n, ''))
            if b is not None and b.keys:
                out[i] = b
        return out

    @staticmethod
    def _locals(sk, bones: dict, t: float) -> dict: #vers 1
        """{frame: local 4x4} sampled at t."""
        out = {}
        for i, b in bones.items():
            q, tr = b.sample(t)
            m = np.eye(4)
            m[:3, :3] = quat_to_matrix(q) if q is not None else sk.frames[i]['local'][:3, :3]
            m[3, :3] = tr if tr is not None else sk.frames[i]['local'][3, :3]
            out[i] = m
        return out

    def textures(self) -> List[dict]: #vers 1
        """Every loaded texture (for the viewport upload)."""
        return self._tex

    def _item_ltms(self, it: dict, t: float) -> list: #vers 1
        """Frame matrices of one item at t (IV: the section's anim and mover)."""
        sk = it['sk']
        if 'segs' not in it:
            return sk.pose_ltms(self._locals(sk, it['bones'], t))
        if not it['segs']:
            return sk.pose_ltms({})
        seg = next((s for s in it['segs'] if s[0] <= t < s[1]), it['segs'][-1] if t >= it['segs'][-1][0]
                   else it['segs'][0])
        lt = max(0.0, t - seg[0])
        ltms = sk.pose_ltms(self._locals(sk, seg[2], lt))
        pos, rot = seg[3], seg[4]
        if pos is not None or rot is not None:
            f = int(lt * 30.0)
            M = np.eye(4)
            if rot is not None:
                M[:3, :3] = quat_to_matrix(tuple(rot[min(f, len(rot) - 1)]))
            if pos is not None:
                M[3, :3] = pos[min(f, len(pos) - 1)]
            ltms = [L @ M for L in ltms]
        return ltms

    def actor_paths(self, step: float = 0.25, only: Optional[int] = None) -> list: #vers 2
        """[(name, [(t, x, y)])]: each item's centre over the scene (for the plan view); only: one item."""
        out = []
        n = max(1, int(self.duration / step) + 1)
        for it in (self.items if only is None else [self.items[only]]):
            pts = []
            for k in range(n):
                t = min(k * step, self.duration)
                L = self._item_ltms(it, t)
                if L:
                    c = np.mean([m[3, :2] for m in L], axis=0)
                    pts.append((t, float(c[0]), float(c[1])))
            out.append((it['name'], pts))
        return out

    def actor_at(self, index: int, t: float) -> tuple: #vers 1
        """Scene-space centre (x, y, z) of an item at t (models' lift included)."""
        t = min(max(t, 0.0), self.duration)
        L = self._item_ltms(self.items[index], t)
        c = np.mean([m[3, :3] for m in L], axis=0) if L else np.zeros(3)
        return float(c[0]), float(c[1]), float(c[2] + (self.lift or 0.0))

    def move_actor(self, index: int, t: float, dx: float, dy: float, window: float = 1.0) -> bool: #vers 1
        """Bend an item's route: shift its root translation keys by (dx, dy) at t, fading out over window s."""
        import math
        it = self.items[index]
        if 'segs' in it or not it['bones']:
            return False                                 # IV routes live in the .wad mover
        sk = it['sk']

        def depth(i):
            d = 0
            while sk.frames[i]['parent'] >= 0:
                i, d = sk.frames[i]['parent'], d + 1
            return d
        movers = [i for i, b in it['bones'].items() if any(k.trans is not None for k in b.keys)]
        if not movers:
            return False
        fi = min(movers, key=depth)
        parent = sk.frames[fi]['parent']
        ltms = self._item_ltms(it, t)
        R = ltms[parent][:3, :3] if parent >= 0 else np.eye(3)
        d = np.array([dx, dy, 0.0]) @ R.T                # world shift into the parent's space
        for k in it['bones'][fi].keys:
            gap = abs(k.time - t)
            if k.trans is None or gap >= window:
                continue
            w = 0.5 * (1 + math.cos(math.pi * gap / window))
            k.trans = tuple(float(a + w * b) for a, b in zip(k.trans, d))
        return True

    def geoms_at(self, t: float) -> list: #vers 3
        """Viewport geometry tuples of every model posed at time t (scene space)."""
        out = []
        for it in self.items:
            sk = it['sk']
            ltms = self._item_ltms(it, t)
            for gi, (V, N, uvs, tris, prelit, mats, flags) in it['geo'].items():
                fi = sk.atomics.get(gi, 0)
                M = ltms[fi] if fi < len(ltms) else np.eye(4)
                if gi in sk.skins:
                    P, PN = sk.skin_vertices(gi, V, N, ltms)       # atomic frame space
                else:
                    P, PN = V, N
                P = P @ M[:3, :3] + M[3, :3]
                if self.lift:
                    P = P + (0.0, 0.0, self.lift)
                PN = PN @ M[:3, :3] if PN is not None else None
                out.append((P.tolist(), PN.tolist() if PN is not None else [], uvs, tris, mats, prelit, flags))
        return out
