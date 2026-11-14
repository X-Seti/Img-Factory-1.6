#this belongs in apps/components/DP5_Workshop/depends/dp5_brush_media.py - Version: 1
# X-Seti - November 14 2026 - IMG Factory 1.6 - DP5 natural media brushes, smoothing and pen pressure

"""
Natural media dabs (chalk, watercolour, oil), pen pressure scaling and freehand smoothing.
Pure numpy on the canvas RGBA bytearray, no Qt.
"""

##Methods list -
# mirror_points
# paint_dab
# smooth_point

import numpy as np

KINDS = ('off', 'chalk', 'watercolour', 'oil')
_RNG = np.random.default_rng(5)


def smooth_point(prev, new, amount: float): #vers 1
    """Freehand smoothing: move prev toward new; amount 0 (none) to 0.95 (heavy lag)."""
    if prev is None or amount <= 0:
        return float(new[0]), float(new[1])
    k = 1.0 - min(0.95, amount)
    return prev[0] + (new[0] - prev[0]) * k, prev[1] + (new[1] - prev[1]) * k


def mirror_points(cx: int, cy: int, w: int, h: int, sym: str) -> list: #vers 1
    """Dab centres including symmetry mirrors."""
    pts = [(cx, cy)]
    if sym in ('H', 'quad'):
        pts.append((w - 1 - cx, cy))
    if sym in ('V', 'quad'):
        pts.append((cx, h - 1 - cy))
    if sym == 'quad':
        pts.append((w - 1 - cx, h - 1 - cy))
    return pts


def paint_dab(rgba, w: int, h: int, cx: int, cy: int, colour, kind: str, size: float,
              opacity: float = 1.0, carried=None): #vers 1
    """Blend one dab into rgba; colour (r, g, b); oil returns the colour carried on the brush."""
    r = max(0.5, size / 2.0)
    n = int(np.ceil(r)) + 1
    x0, x1 = max(0, cx - n), min(w, cx + n + 1)
    y0, y1 = max(0, cy - n), min(h, cy + n + 1)
    if x1 <= x0 or y1 <= y0:
        return carried
    ys, xs = np.mgrid[y0:y1, x0:x1]
    d = np.sqrt((xs - cx) ** 2 + (ys - cy) ** 2)
    a = np.frombuffer(rgba, np.uint8).reshape(h, w, 4)
    reg = a[y0:y1, x0:x1].astype(np.float64)
    col = np.array(colour[:3], np.float64)
    if kind == 'chalk':
        alpha = (d <= r) * (_RNG.random(d.shape) < 0.55) * 0.9
    elif kind == 'watercolour':
        fall = np.clip(1.0 - d / r, 0, 1)
        alpha = np.where(d <= r, 0.18 + 0.30 * (fall < 0.35), 0.0) * (0.7 + 0.3 * _RNG.random(d.shape))
    elif kind == 'oil':
        sel = d <= r
        if carried is None:
            carried = col
        under = reg[sel & (reg[..., 3] > 0)][:, :3]
        if len(under):
            carried = carried * 0.82 + under.mean(axis=0) * 0.18
        col = np.asarray(carried, np.float64)
        alpha = sel * (1.0 - np.clip(d / r, 0, 1) ** 2 * 0.6) * 0.85
    else:
        alpha = (d <= r) * 1.0
    alpha = (alpha * opacity)[..., None]
    da = reg[..., 3:4] / 255.0
    oa = alpha + da * (1 - alpha)
    rgb = np.where(oa > 0, (col * alpha + reg[..., :3] * da * (1 - alpha)) / np.maximum(oa, 1e-9), reg[..., :3])
    a[y0:y1, x0:x1] = np.concatenate([rgb, oa * 255], axis=2).round().astype(np.uint8)
    return carried
