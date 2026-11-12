#this belongs in apps/components/DP5_Workshop/depends/dp5_mask_tools.py - Version: 2
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 magic wand, gradient, clash maths

"""
Pixel maths for DP5 canvas tools, on RGBA bytes (no Qt):
magic wand masks (tolerance, contiguous or global), the mask outline
overlay, linear / radial FG -> BG gradients limited to a mask, and
ZX Spectrum attribute clash cells (8 x 8 cells with more than 2 colours),
light edge fringes to alpha or a dark matte, and Scale2x upscaling.
"""

##Methods list -
# _arr
# clash_cells
# edge_fringe
# gradient
# mask_outline
# scale2x
# wand_mask

import numpy as np


def _arr(rgba, w: int, h: int) -> np.ndarray: #vers 1
    return np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)


def wand_mask(rgba, w: int, h: int, x: int, y: int, tol: int = 32, contiguous: bool = True) -> np.ndarray: #vers 1
    """h x w bool mask of pixels within tol (max channel difference, alpha included) of pixel x, y."""
    a = _arr(rgba, w, h).astype(np.int16)
    near = (np.abs(a - a[y, x]).max(axis=2) <= tol)
    if not contiguous:
        return near
    mask = np.zeros((h, w), bool)
    stack = [(x, y)]
    while stack:                                         # scanline flood inside 'near'
        sx, sy = stack.pop()
        if mask[sy, sx] or not near[sy, sx]:
            continue
        row, done = near[sy], mask[sy]
        lx = sx
        while lx > 0 and row[lx - 1] and not done[lx - 1]:
            lx -= 1
        rx = sx
        while rx < w - 1 and row[rx + 1] and not done[rx + 1]:
            rx += 1
        mask[sy, lx:rx + 1] = True
        for ny in (sy - 1, sy + 1):
            if 0 <= ny < h:
                seg = near[ny, lx:rx + 1] & ~mask[ny, lx:rx + 1]
                starts = np.flatnonzero(seg & ~np.concatenate(([False], seg[:-1])))
                stack.extend((lx + int(s), ny) for s in starts)
    return mask


def mask_outline(mask: np.ndarray, rgb=(0, 220, 255)) -> bytes: #vers 1
    """RGBA bytes, same size as mask: rgb on the mask's edge pixels, clear elsewhere."""
    m = mask
    edge = m & ~(np.roll(m, 1, 0) & np.roll(m, -1, 0) & np.roll(m, 1, 1) & np.roll(m, -1, 1))
    edge[0, :] |= m[0, :]
    edge[-1, :] |= m[-1, :]
    edge[:, 0] |= m[:, 0]
    edge[:, -1] |= m[:, -1]
    out = np.zeros(m.shape + (4,), np.uint8)
    out[edge] = (*rgb, 255)
    return out.tobytes()


def gradient(rgba, w: int, h: int, p0, p1, c0, c1, radial: bool = False, mask=None) -> bytearray: #vers 1
    """FG c0 at p0 to BG c1 at p1 (RGBA tuples), linear or radial, only where mask (None = all)."""
    a = _arr(rgba, w, h).copy()
    ys, xs = np.mgrid[0:h, 0:w].astype(np.float32)
    dx, dy = p1[0] - p0[0], p1[1] - p0[1]
    length2 = float(dx * dx + dy * dy) or 1.0
    if radial:
        t = np.sqrt(((xs - p0[0]) ** 2 + (ys - p0[1]) ** 2) / length2)
    else:
        t = ((xs - p0[0]) * dx + (ys - p0[1]) * dy) / length2
    t = np.clip(t, 0, 1)[..., None]
    col = (np.array(c0, np.float32) * (1 - t) + np.array(c1, np.float32) * t + 0.5).astype(np.uint8)
    if mask is None:
        a[:] = col
    else:
        a[mask] = col[mask]
    return bytearray(a.tobytes())


def clash_cells(rgba, w: int, h: int) -> list: #vers 1
    """(cx, cy) of 8 x 8 cells holding more than 2 colours (opaque pixels, 3 bits a channel)."""
    cw, ch = w // 8, h // 8
    if not cw or not ch:
        return []
    a = _arr(rgba, w, h)[:ch * 8, :cw * 8]
    key = ((a[..., 0] >> 5).astype(np.int32) << 6) | ((a[..., 1] >> 5).astype(np.int32) << 3) | (a[..., 2] >> 5)
    key = np.where(a[..., 3] > 0, key, -1)
    cells = key.reshape(ch, 8, cw, 8).transpose(0, 2, 1, 3).reshape(ch, cw, 64)
    s = np.sort(cells, axis=2)
    distinct = (np.diff(s, axis=2) != 0).sum(axis=2) + 1 - (s[..., 0] == -1)
    return [(int(x), int(y)) for y, x in np.argwhere(distinct > 2)]


def edge_fringe(rgba, w: int, h: int, thr: int = 200, width: int = 2, matte=None) -> bytes: #vers 1
    """Light pixels within width px of transparency: colour-to-alpha, or onto matte rgb."""
    a = _arr(rgba, w, h).astype(np.float32)
    clear = a[..., 3] < 8
    near = clear.copy()
    for _ in range(max(1, width)):
        p = np.pad(near, 1)
        near = near | p[:-2, 1:-1] | p[2:, 1:-1] | p[1:-1, :-2] | p[1:-1, 2:]
    mn = a[..., :3].min(axis=2)
    zone = near & ~clear & (mn >= thr)
    al = np.clip((255.0 - mn) / max(1.0, 255.0 - thr), 0.0, 1.0)     # whiteness = coverage lost
    safe = np.maximum(al, 1e-3)[..., None]
    fg = np.clip((a[..., :3] - (1.0 - al[..., None]) * 255.0) / safe, 0, 255)
    out = a.copy()
    if matte is None:
        out[..., :3] = np.where(zone[..., None], fg, a[..., :3])
        out[..., 3] = np.where(zone, a[..., 3] * al, a[..., 3])
    else:
        m = np.array(matte[:3], np.float32)
        out[..., :3] = np.where(zone[..., None], fg * al[..., None] + m * (1.0 - al[..., None]), a[..., :3])
        out[..., 3] = np.where(zone, 255.0, a[..., 3])
    return np.rint(out).astype(np.uint8).tobytes()


def scale2x(rgba, w: int, h: int) -> bytes: #vers 1
    """Scale2x / EPX pixel-art upscale to 2w x 2h."""
    a = _arr(rgba, w, h)
    k = a.copy().view(np.uint32)[..., 0]                    # one key per pixel
    p = np.pad(k, 1, mode='edge')
    B, D, F, H = p[:-2, 1:-1], p[1:-1, :-2], p[1:-1, 2:], p[2:, 1:-1]
    pi = np.pad(np.arange(w * h).reshape(h, w), 1, mode='edge')
    iB, iD, iF, iH = pi[:-2, 1:-1], pi[1:-1, :-2], pi[1:-1, 2:], pi[2:, 1:-1]
    iE = pi[1:-1, 1:-1]
    e0 = np.where((D == B) & (B != F) & (D != H), iD, iE)
    e1 = np.where((B == F) & (B != D) & (F != H), iF, iE)
    e2 = np.where((D == H) & (D != B) & (H != F), iD, iE)
    e3 = np.where((H == F) & (D != H) & (B != F), iF, iE)
    idx = np.empty((h * 2, w * 2), np.int64)
    idx[0::2, 0::2], idx[0::2, 1::2], idx[1::2, 0::2], idx[1::2, 1::2] = e0, e1, e2, e3
    return a.reshape(-1, 4)[idx].tobytes()
