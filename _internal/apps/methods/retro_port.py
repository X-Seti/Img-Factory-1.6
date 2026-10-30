#this belongs in apps/methods/retro_port.py - Version: 1
# X-Seti - October 24 2026 - IMG Factory 1.6 - port top-down GTA maps between styles

"""
Port a map's blocks onto another map of the same game (GTA1 PC <-> PS1
.vsr, GTA2 PC <-> PS1): columns and blocks copied, every face tile
replaced by the closest-looking tile of the target style (8 x 8 colour
signatures, side and lid tiles matched separately). The target keeps its
own header, routes, zones and other data.
"""

##Methods list -
# _signature
# port_blocks
# tile_mapping

import copy
from typing import Dict, Optional

import numpy as np

from apps.methods.gta2_map import face_tile

SIDES = ('left', 'right', 'top', 'bottom')
SIG = 8


def _signature(rgba: np.ndarray) -> Optional[np.ndarray]: #vers 1
    """8 x 8 average RGBA of a tile (any size), None if empty."""
    a = rgba.astype(np.float32)
    h, w = a.shape[:2]
    if h < SIG or w < SIG or not a[:, :, 3].any():
        return None
    a = a[:h // SIG * SIG, :w // SIG * SIG]
    return a.reshape(SIG, h // SIG, SIG, w // SIG, 4).mean((1, 3)).ravel()


def tile_mapping(src_style, dst_style, face: str, used) -> Dict[int, int]: #vers 1
    """{src tile number: closest dst tile number} for the used tiles of one face kind."""
    cand, sigs = [], []
    for n in range(1, 1024):
        if dst_style.face_global(face, n) is None:
            continue
        s = _signature(dst_style.face_rgba(face, n))
        if s is not None:
            cand.append(n)
            sigs.append(s)
    if not cand:
        raise ValueError(f"Target style has no {face} tiles")
    sigs = np.array(sigs)
    out = {0: 0}
    for n in used:
        if n in out:
            continue
        s = _signature(src_style.face_rgba(face, n)) if src_style.face_global(face, n) is not None else None
        out[n] = 0 if s is None else cand[int(((sigs - s) ** 2).sum(1).argmin())]
    return out


def port_blocks(src_map, src_style, dst_map, dst_style) -> dict: #vers 1
    """Copy src columns / blocks into dst with tiles matched; returns counts."""
    if type(src_map).LEVELS != type(dst_map).LEVELS or src_map.GAME.split()[0] != dst_map.GAME.split()[0]:
        raise ValueError(f"Can't port {src_map.GAME} blocks into a {dst_map.GAME} map")
    used = {'side': set(), 'lid': set()}
    for b in src_map.blocks:
        for f in SIDES:
            used['side'].add(face_tile(src_map.face_word(b, f)))
        used['lid'].add(face_tile(src_map.face_word(b, 'lid')))
    maps = {k: tile_mapping(src_style, dst_style, k, v) for k, v in used.items()}
    blocks = []
    for b in src_map.blocks:
        nb = copy.copy(b)
        for f in SIDES + ('lid',):
            w = src_map.face_word(b, f)
            t = maps['lid' if f == 'lid' else 'side'][face_tile(w)]
            dst_map.set_face_word(nb, f, (w & ~0x3FF) | t)
        blocks.append(nb)
    dst_map.blocks = blocks
    dst_map.columns = [[dict(c) for c in row] for row in src_map.columns]
    dst_map.edited = True
    return {'blocks': len(blocks), 'side tiles': len(maps['side']) - 1, 'lid tiles': len(maps['lid']) - 1}
