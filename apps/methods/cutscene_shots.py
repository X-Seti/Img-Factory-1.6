#this belongs in apps/methods/cutscene_shots.py - Version: 1
# X-Seti - November 06 2026 - IMG Factory 1.6 - Cutscene camera shots

"""
Camera .dat tracks (FOV, roll, camera position, target) as a list of shots.
A cut is a repeated FOV key time; each shot keeps its keys with times from
its own start, so lengths can change, shots be added, removed or split, and
join_shots gives the four tracks back (unchanged shots give the same keys).
"""

##Methods list -
# _cuts
# follow_keys
# join_shots
# key3
# key9
# shot_keys
# split_shots

import math
from typing import List, Optional, Tuple

GAP = 0.02                                   # camera / target: first key of a new shot after the cut


def key3(v: float) -> List[float]: #vers 1
    """FOV / roll key values (value, in, out)."""
    return [v, v, v]


def key9(p) -> List[float]: #vers 1
    """Position key values (xyz, in xyz, out xyz)."""
    return list(p[:3]) * 3


def _cuts(tracks) -> List[float]: #vers 1
    """Cut times: repeated FOV key times."""
    f = tracks[0]
    return sorted({f[i][0] for i in range(1, len(f)) if f[i][0] == f[i - 1][0]})


def split_shots(tracks) -> List[dict]: #vers 1
    """[{'start', 'len', 'keys': [4 x [(rel t, values)]]}] from camera tracks."""
    cuts = _cuts(tracks)
    end = max((tr[-1][0] for tr in tracks if tr), default=0.0)
    bounds = [0.0] + cuts + [max(end, cuts[-1] if cuts else 0.0)]
    shots = [{'start': a, 'len': b - a, 'keys': [[], [], [], []]} for a, b in zip(bounds, bounds[1:])]
    if not shots:
        shots = [{'start': 0.0, 'len': end, 'keys': [[], [], [], []]}]
    for ti, tr in enumerate(tracks[:4]):
        k, seen = 0, False
        for t, vals in tr:
            while k < len(cuts) and (t > cuts[k] or (t == cuts[k] and seen)):
                k, seen = k + 1, False
            if k < len(cuts) and t == cuts[k]:
                seen = True                              # first key at the cut ends this shot
            sh = shots[min(k, len(shots) - 1)]
            sh['keys'][ti].append((t - sh['start'], list(vals)))
    return shots


def join_shots(shots: List[dict]): #vers 1
    """Four camera tracks from shots laid end to end."""
    tracks, start = [[], [], [], []], 0.0
    for sh in shots:
        for ti in range(4):
            tracks[ti].extend((round(start + rt, 6), list(v)) for rt, v in sh['keys'][ti])
        start = round(start + sh['len'], 6)
    return tracks


def shot_keys(length: float, cam: List[Tuple[float, tuple]], tgt: List[Tuple[float, tuple]],
              fov: float = 70.0, roll: float = 0.0, first: bool = False) -> dict: #vers 1
    """A new shot: cam / tgt [(rel t, xyz)]; FOV and roll held; camera keys start after the cut."""
    off = 0.0 if first else GAP
    cam = [(max(off, t), p) for t, p in cam]
    tgt = [(max(off, t), p) for t, p in tgt]
    return {'start': 0.0, 'len': length,
            'keys': [[(0.0, key3(fov)), (length, key3(fov))], [(0.0, key3(roll)), (length, key3(roll))],
                     [(t, key9(p)) for t, p in cam], [(t, key9(p)) for t, p in tgt]]}


def follow_keys(at, start: float, length: float, dist: float = 4.0, height: float = 1.6,
                side: float = 0.0, step: float = 0.5): #vers 1
    """Camera behind an actor and target on it: (cam keys, tgt keys), shot-relative times.
    at(t) -> (x, y, z) actor centre in scene space; side: degrees around the actor (0 = behind)."""
    cam, tgt = [], []
    n = max(1, int(round(length / step)))
    for i in range(n + 1):
        t = start + length * i / n
        x, y, z = at(t)
        x2, y2, _ = at(t + 0.5)
        x1, y1, _ = at(t - 0.5)
        hd = math.atan2(y2 - y1, x2 - x1) if abs(x2 - x1) + abs(y2 - y1) > 1e-3 else 0.0
        a = hd + math.pi + math.radians(side)
        cam.append((t - start, (x + dist * math.cos(a), y + dist * math.sin(a), z + height)))
        tgt.append((t - start, (x, y, z + 0.4)))
    return cam, tgt
