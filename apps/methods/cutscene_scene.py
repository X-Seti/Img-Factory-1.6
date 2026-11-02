#this belongs in apps/methods/cutscene_scene.py - Version: 4
# X-Seti - November 02 2026 - IMG Factory 1.6 - Cutscene playback scene

"""
Cutscene playback: the .cut 'model' list loaded (DFF + TXD), each posed by
its animation from the cutscene .ifp at a time, as viewport geometry.
The player (csplay) is built from models/player.img parts (default outfit,
clothes.dat CUTS swaps); a stand-in ped when player.img isn't found. III / VC have no .cut (the script is
in main.scm): the models are then the IFP's animation names.
"""

##Methods list -
#
##class CutsceneScene: -
# __init__
# _add_item
# _player_parts
# _bone_map
# _locals
# geoms_at
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
                 clothes_rules: Optional[dict] = None): #vers 4
        """sections: parse_cut output; fetch(name, '.dff' / '.txd') -> bytes or None;
        clothes_rules: parse_clothes_dat output (player cutscene parts)."""
        self.items, self.notes, self._tex = [], [], []
        self.lift = 0.0                                  # SA: models sit 1 m above the anim root
        self._fetch, self._rules = fetch, clothes_rules
        ifp = parse_ifp(ifp_bytes) if ifp_bytes else None
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

    def _add_item(self, name: str, dff: bytes, txd: Optional[bytes], anim, skin_all: bool = False) -> bool: #vers 1
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
        geo = {}
        for gi, g in enumerate(model.geometries):
            geo[gi] = (np.array([(v.x, v.y, v.z) for v in g.vertices], dtype=np.float64),
                       np.array([(n.x, n.y, n.z) for n in g.normals], dtype=np.float64) if g.normals else None,
                       [(u.u, u.v) for u in g.uv_layers[0]] if g.uv_layers else [],
                       [(t.v1, t.v2, t.v3, t.material_id) for t in g.triangles],
                       [(c.r, c.g, c.b, c.a) for c in g.colors] if g.colors else [],
                       g.materials, getattr(g, 'flags', 0))
        sk = read_skeleton(dff)
        self.items.append({'name': name, 'sk': sk, 'anim': anim, 'geo': geo, 'bones': self._bone_map(sk, anim)})
        return True

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

    def geoms_at(self, t: float) -> list: #vers 2
        """Viewport geometry tuples of every model posed at time t (scene space)."""
        out = []
        for it in self.items:
            sk = it['sk']
            ltms = sk.pose_ltms(self._locals(sk, it['bones'], t))
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
