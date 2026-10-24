#this belongs in apps/methods/retro_mapimage.py - Version: 1
# X-Seti - October 24 2026 - IMG Factory 1.6 - top-down GTA map as one image and back

"""
The map's lids as one RGB image (every level stacked bottom to top, lids
rotated clockwise then flipped as the game draws them), px pixels a block,
for an area of blocks. Importing an edited image back: only blocks whose
pixels changed are matched against every lid tile in its 8 rotations /
flips, and that block's top lid takes the closest one.
"""

##Methods list -
# _lid_rgba
# _scaled
# import_map_image
# lid_words
# map_image

from typing import Dict, Tuple

import numpy as np

from apps.methods.gta2_map import face_flags, face_tile

Area = Tuple[int, int, int, int]


def _scaled(a: np.ndarray, px: int) -> np.ndarray: #vers 1
    """Nearest-neighbour resize of a square tile to px x px."""
    n = a.shape[0]
    idx = (np.arange(px) * n // px)
    return a[idx][:, idx]


def _lid_rgba(style, word: int, px: int, cache: Dict) -> np.ndarray: #vers 1
    key = word & 0xF3FF
    if key not in cache:
        a = style.face_rgba('lid', face_tile(word))
        f = face_flags(word)
        a = np.rot90(a, -f['rotation'])
        if f['flip']:
            a = a[:, ::-1]
        cache[key] = _scaled(np.ascontiguousarray(a), px).astype(np.float32)
    return cache[key]


def lid_words(gmap, x: int, y: int) -> list: #vers 1
    """Lid words of a cell, bottom first."""
    col = gmap.column(x, y)
    out = []
    for z in sorted(col):
        w = gmap.face_word(gmap.blocks[col[z]], 'lid')
        if face_tile(w):
            out.append(w)
    return out


def map_image(gmap, style, px: int = 16, area: Area = (0, 0, 255, 255)) -> np.ndarray: #vers 1
    """H x W x 3 uint8 image of the area's lids."""
    x0, y0, x1, y1 = area
    img = np.zeros(((y1 - y0 + 1) * px, (x1 - x0 + 1) * px, 3), np.float32)
    cache: Dict = {}
    for y in range(y0, y1 + 1):
        for x in range(x0, x1 + 1):
            cell = img[(y - y0) * px:(y - y0 + 1) * px, (x - x0) * px:(x - x0 + 1) * px]
            for w in lid_words(gmap, x, y):
                t = _lid_rgba(style, w, px, cache)
                al = t[:, :, 3:4] / 255.0
                cell[:] = cell * (1 - al) + t[:, :, :3] * al
    return img.astype(np.uint8)


def import_map_image(gmap, style, img: np.ndarray, px: int, area: Area = (0, 0, 255, 255)) -> int: #vers 1
    """Changed blocks of an edited map image become the closest lid (tile, rotation, flip); returns the count."""
    x0, y0, x1, y1 = area
    want = ((y1 - y0 + 1) * px, (x1 - x0 + 1) * px)
    if img.shape[:2] != want:
        raise ValueError(f"Image must be {want[1]} x {want[0]} ({px} px a block)")
    now = map_image(gmap, style, px, area).astype(np.int16)
    new = img[:, :, :3].astype(np.int16)
    diff = np.abs(now - new).reshape(want[0] // px, px, want[1] // px, px, 3).mean((1, 3, 4))
    cells = np.argwhere(diff > 6)
    if not len(cells):
        return 0
    sig = max(1, px // 8)
    cand, sigs = [], []
    for n in range(1, 1024):
        if style.face_global('lid', n) is None:
            continue
        base = style.face_rgba('lid', n)
        if not base[:, :, 3].any():
            continue
        for flip in (0, 1):
            for rot in range(4):
                a = np.rot90(base, -rot)
                a = a[:, ::-1] if flip else a
                s = _scaled(np.ascontiguousarray(a), px)[::sig, ::sig, :3].astype(np.float32)
                cand.append(n | (rot << 14) | (0x2000 if flip else 0))
                sigs.append(s.ravel())
    sigs = np.array(sigs)
    count = 0
    for cy, cx in cells:
        x, y = x0 + int(cx), y0 + int(cy)
        reg = new[cy * px:(cy + 1) * px, cx * px:(cx + 1) * px][::sig, ::sig].astype(np.float32).ravel()
        word = cand[int(((sigs - reg) ** 2).sum(1).argmin())]
        z, b = gmap.top_block(x, y)
        if b is None:
            col = gmap.column(x, y)
            z = max(col) if col else 1
            b = gmap.block_at(x, y, z) or gmap.new_block()
        nb = b.copy()
        old = gmap.face_word(nb, 'lid')
        caps = gmap.face_caps('lid')
        keep = old & ~(0x3FF | 0xC000 | 0x2000)
        w = (word & 0x3FF) | (word & 0xC000 if 'rotation' in caps else 0) | (word & 0x2000 if 'flip' in caps else 0)
        gmap.set_face_word(nb, 'lid', keep | w)
        gmap.set_block(x, y, z, nb)
        count += 1
    gmap.edited = True
    return count
