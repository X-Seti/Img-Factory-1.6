#this belongs in apps/methods/terrain_brush.py - Version: 2
# X-Seti - October 21 2026 - IMG Factory 1.6 - Terrain brush maths

"""
Terrain sculpt brushes (numpy): falloff weights, ray / mesh hits and the
new heights for Raise, Lower, Flatten, Level, Smooth, Roughen and Ramp;
path strips (spline, ground profile, level across).
Works on world-space vertex arrays; callers map back to model space.
"""

##Methods list -
# brush_heights
# falloff_weights
# gl_rotation
# path_heights
# path_profile
# path_project
# ramp_heights
# ray_mesh_hit
# smooth_targets
# spline_points
# vertex_normals

import math

import numpy as np

MODES = ('raise', 'lower', 'flatten', 'level', 'smooth', 'roughen', 'ramp')


def gl_rotation(m16) -> np.ndarray: #vers 1
    """3x3 rotation from a column-major 4x4 GL matrix (16 floats)."""
    return np.asarray(m16, dtype=np.float64).reshape(4, 4)[:3, :3].T


def falloff_weights(d: np.ndarray, radius: float, falloff: float) -> np.ndarray: #vers 1
    """1 inside the hard core, smooth fade to 0 at the radius."""
    w = (d <= radius).astype(np.float64)
    inner = radius * (1.0 - max(0.0, min(1.0, falloff)))
    if radius - inner > 1e-6:
        t = np.clip((d - inner) / (radius - inner), 0.0, 1.0)
        w *= 1.0 - (3.0 * t * t - 2.0 * t * t * t)
    return w


def ray_mesh_hit(origin, direction, V: np.ndarray, T: np.ndarray): #vers 1
    """Nearest distance along the ray to a triangle of (V, T), or None."""
    if not len(T):
        return None
    o, d = np.asarray(origin, np.float64), np.asarray(direction, np.float64)
    a, b, c = V[T[:, 0]], V[T[:, 1]], V[T[:, 2]]
    e1, e2 = b - a, c - a
    p = np.cross(d, e2)
    det = np.einsum('ij,ij->i', e1, p)
    ok = np.abs(det) > 1e-9
    inv = np.where(ok, 1.0 / np.where(ok, det, 1.0), 0.0)
    s = o - a
    u = np.einsum('ij,ij->i', s, p) * inv
    q = np.cross(s, e1)
    v = (q @ d) * inv
    t = np.einsum('ij,ij->i', e2, q) * inv
    hit = ok & (u >= 0) & (v >= 0) & (u + v <= 1) & (t > 1e-4)
    if not hit.any():
        return None
    return float(t[hit].min())


def smooth_targets(P: np.ndarray, cell: float) -> np.ndarray: #vers 1
    """Mean height of each point's 3x3 neighbour cells (XY grid)."""
    cell = max(cell, 1e-3)
    ij = np.floor(P[:, :2] / cell).astype(np.int64)
    ij -= ij.min(axis=0)
    w, h = ij[:, 0].max() + 3, ij[:, 1].max() + 3
    total, count = np.zeros((w, h)), np.zeros((w, h))
    np.add.at(total, (ij[:, 0] + 1, ij[:, 1] + 1), P[:, 2])
    np.add.at(count, (ij[:, 0] + 1, ij[:, 1] + 1), 1.0)
    st, sc = np.zeros_like(total), np.zeros_like(count)
    for dx in (-1, 0, 1):
        for dy in (-1, 0, 1):
            st += np.roll(np.roll(total, dx, 0), dy, 1)
            sc += np.roll(np.roll(count, dx, 0), dy, 1)
    cx, cy = ij[:, 0] + 1, ij[:, 1] + 1
    return st[cx, cy] / np.maximum(sc[cx, cy], 1.0)


def brush_heights(mode: str, P: np.ndarray, centre, radius: float, falloff: float,
                  strength: float, slope_deg: float, target: float = 0.0) -> np.ndarray: #vers 1
    """New z for world points P (n,3) after one brush tick at centre."""
    z = P[:, 2]
    d = np.hypot(P[:, 0] - centre[0], P[:, 1] - centre[1])
    w = falloff_weights(d, radius, falloff)
    tan_s = math.tan(math.radians(max(1.0, min(89.0, slope_deg))))
    k = np.clip(w * min(1.0, strength / max(radius, 1e-3) * 4.0), 0.0, 1.0)
    rim_sel = (d >= radius * 0.8) & (d <= radius)
    rim = float(z[rim_sel].mean()) if rim_sel.any() else float(z[d <= radius].mean() if (d <= radius).any() else centre[2])
    if mode == 'raise':
        cap = rim + tan_s * (radius - d)                      # slope limits the hill from the rim
        return np.where(w > 0, np.minimum(z + strength * w, np.maximum(z, cap)), z)
    if mode == 'lower':
        floor = rim - tan_s * (radius - d)
        return np.where(w > 0, np.maximum(z - strength * w, np.minimum(z, floor)), z)
    if mode in ('flatten', 'level'):
        return z + (target - z) * k
    if mode == 'smooth':
        return z + (smooth_targets(P, radius / 4.0) - z) * k
    if mode == 'roughen':
        n = np.sin(P[:, 0] * 12.9898 + P[:, 1] * 78.233) * 43758.5453
        n = (n - np.floor(n)) * 2.0 - 1.0                     # stable per-position noise
        return z + n * strength * 0.5 * w
    return z


def ramp_heights(P: np.ndarray, a, b, radius: float, falloff: float, slope_deg: float) -> np.ndarray: #vers 1
    """New z for a straight ramp from a to b, width = radius."""
    z = P[:, 2]
    ax, ay, az = a
    bx, by, bz = b
    abx, aby = bx - ax, by - ay
    ln2 = abx * abx + aby * aby
    if ln2 < 1e-6:
        return z
    t = np.clip(((P[:, 0] - ax) * abx + (P[:, 1] - ay) * aby) / ln2, 0.0, 1.0)
    d = np.hypot(P[:, 0] - (ax + abx * t), P[:, 1] - (ay + aby * t))
    rise = math.tan(math.radians(max(1.0, min(89.0, slope_deg)))) * math.sqrt(ln2)
    dz = max(-rise, min(rise, bz - az))                       # slope caps the gradient
    w = falloff_weights(d, radius, falloff)
    return z + (az + dz * t - z) * w


def vertex_normals(V: np.ndarray, T: np.ndarray) -> np.ndarray: #vers 1
    """Smooth per-vertex normals from triangles."""
    n = np.zeros_like(V)
    if len(T):
        fn = np.cross(V[T[:, 1]] - V[T[:, 0]], V[T[:, 2]] - V[T[:, 0]])
        for i in range(3):
            np.add.at(n, T[:, i], fn)
    ln = np.linalg.norm(n, axis=1)
    n[ln > 0] /= ln[ln > 0, None]
    n[ln == 0] = (0.0, 0.0, 1.0)
    return n


def spline_points(pts, step: float = 1.0) -> np.ndarray: #vers 1
    """Catmull-Rom curve through the (x, y) points, about step metres apart."""
    P = np.asarray(pts, np.float64)[:, :2]
    if len(P) < 2:
        return P.copy()
    E = np.vstack([2 * P[0] - P[1], P, 2 * P[-1] - P[-2]])
    out = []
    for k in range(1, len(E) - 2):
        p0, p1, p2, p3 = E[k - 1], E[k], E[k + 1], E[k + 2]
        n = max(1, int(math.ceil(np.hypot(*(p2 - p1)) / max(step, 1e-3))))
        t = np.arange(n)[:, None] / n
        out.append(0.5 * (2 * p1 + (p2 - p0) * t + (2 * p0 - 5 * p1 + 4 * p2 - p3) * t ** 2
                          + (3 * p1 - p0 - 3 * p2 + p3) * t ** 3))
    out.append(P[-1:])
    return np.vstack(out)


def path_profile(line: np.ndarray, ground, smooth: float, slope_deg: float) -> np.ndarray: #vers 1
    """Height along the line: ground(x, y) averaged over smooth metres,
    then the gradient capped at slope_deg."""
    z = np.array([ground(x, y) for x, y in line[:, :2]], np.float64)
    ds = np.hypot(*np.diff(line[:, :2], axis=0).T)
    s = np.concatenate([[0.0], np.cumsum(ds)])
    if smooth > 0 and len(z) > 2:
        half = smooth / 2.0
        csum = np.concatenate([[0.0], np.cumsum(z)])
        lo = np.searchsorted(s, s - half, 'left')
        hi = np.searchsorted(s, s + half, 'right')
        z = (csum[hi] - csum[lo]) / np.maximum(hi - lo, 1)
    g = math.tan(math.radians(max(0.5, min(89.0, slope_deg)))) * ds
    for k in range(1, len(z)):                                 # cap climbing forwards
        z[k] = min(max(z[k], z[k - 1] - g[k - 1]), z[k - 1] + g[k - 1])
    for k in range(len(z) - 2, -1, -1):                        # and backwards
        z[k] = min(max(z[k], z[k + 1] - g[k]), z[k + 1] + g[k])
    return z


def path_project(Q: np.ndarray, line: np.ndarray): #vers 1
    """(distance, segment, t) of each (x, y) in Q to the nearest point of the polyline."""
    A, B = line[:-1, :2], line[1:, :2]
    best = np.full(len(Q), np.inf)
    seg = np.zeros(len(Q), np.int64)
    tt = np.zeros(len(Q))
    if len(A) == 0:
        return np.hypot(*(Q[:, :2] - line[0, :2]).T), seg, tt
    for k0 in range(0, len(A), 256):
        a, b = A[k0:k0 + 256], B[k0:k0 + 256]
        ab = b - a
        ln2 = np.maximum((ab ** 2).sum(1), 1e-12)
        aq = Q[:, None, :2] - a[None]
        t = np.clip((aq * ab[None]).sum(2) / ln2[None], 0.0, 1.0)
        d = np.hypot(*(aq - t[..., None] * ab[None]).transpose(2, 0, 1))
        k = d.argmin(1)
        dk = d[np.arange(len(Q)), k]
        better = dk < best
        best[better] = dk[better]
        seg[better] = k[better] + k0
        tt[better] = t[np.arange(len(Q)), k][better]
    return best, seg, tt


def path_heights(P: np.ndarray, line: np.ndarray, prof: np.ndarray, half: float,
                 blend: float) -> np.ndarray: #vers 1
    """New z for world points P: level across the path at its profile height,
    fading back to the ground over blend metres past the edge."""
    d, seg, t = path_project(P, line)
    target = prof[seg] + (prof[np.minimum(seg + 1, len(prof) - 1)] - prof[seg]) * t
    if blend > 1e-6:
        u = np.clip((d - half) / blend, 0.0, 1.0)
        w = 1.0 - (3.0 * u * u - 2.0 * u * u * u)
    else:
        w = (d <= half).astype(np.float64)
    return P[:, 2] + (target - P[:, 2]) * w
