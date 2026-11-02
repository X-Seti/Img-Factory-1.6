#this belongs in apps/methods/cutscene_files.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - Cutscene .cut / .dat files

"""
GTA III / VC / SA cutscene files (anim/cuts.img): the .cut script (sections
info, model, text, uncompress, motion, remove, extracol, peffect ... each
closed by 'end') and the .dat camera path (four keyframe tracks: FOV,
roll, camera position, target position).
"""

##Methods list -
# build_cam_dat
# build_cut
# cam_at
# parse_cam_dat
# parse_cut

from typing import List, Tuple

# column names per .cut section (rows are comma separated)
CUT_COLUMNS = {
    'info': ["Key", "Values"],
    'model': ["Set", "Model", "Animation"],
    'text': ["Start ms", "Length ms", "GXT key"],
    'uncompress': ["Animation"],
    'motion': ["Object : bone", "Type"],
    'remove': ["X", "Y", "Z", "Model"],
    'extracol': ["Value"],
    'peffect': ["Effect", "Time ms", "Object", "Bone", "Attach", "X", "Y", "Z", "Dir X", "Dir Y", "Dir Z"],
}
CAM_TRACKS = ("FOV", "Roll", "Camera position", "Target position")


def parse_cut(text: str) -> List[Tuple[str, List[str]]]: #vers 1
    """[(section name, [row lines])] in file order."""
    out, cur = [], None
    for raw in text.replace('\r', '').replace('\0', '').split('\n'):
        line = raw.strip()
        if not line:
            continue
        if cur is None:
            cur = (line.lower(), [])
            out.append(cur)
        elif line.lower() == 'end':
            cur = None
        else:
            cur[1].append(line)
    return out


def build_cut(sections: List[Tuple[str, List[str]]]) -> str: #vers 1
    """.cut text (CRLF) from parse_cut style sections."""
    lines = []
    for name, rows in sections:
        lines += [name] + list(rows) + ['end']
    return '\r\n'.join(lines) + '\r\n'


def parse_cam_dat(text: str) -> List[List[Tuple[float, List[float]]]]: #vers 1
    """Four tracks of (time s, values) from a camera .dat."""
    tracks, cur = [], None
    for raw in text.replace('\r', '').replace('\0', '').split('\n'):
        line = raw.strip()
        if not line:
            continue
        if line == ';':
            if cur is not None:
                tracks.append(cur)
                cur = None
            continue
        if cur is None:
            cur = []                                         # key count line
            continue
        parts = [p.strip() for p in line.rstrip(',').split(',') if p.strip()]
        if not parts:
            continue
        t = float(parts[0].rstrip('fF'))
        cur.append((t, [float(p) for p in parts[1:]]))
    if cur is not None:
        tracks.append(cur)
    while len(tracks) < 4:
        tracks.append([])
    return tracks[:4]


def build_cam_dat(tracks) -> str: #vers 1
    """Camera .dat text (CRLF) from four tracks."""
    lines = []
    for tr in list(tracks)[:4]:
        lines.append(f"{len(tr)},")
        for t, vals in tr:
            lines.append(f"{t:.6f}f," + ''.join(f"{v:.6f}," for v in vals))
        lines.append(';')
    lines.append(';')
    return '\r\n'.join(lines) + '\r\n'


def cam_at(track, t: float) -> List[float]: #vers 1
    """Linear value of a track at time t (first / last key outside the range)."""
    if not track:
        return []
    if t <= track[0][0]:
        return list(track[0][1])
    for (t0, a), (t1, b) in zip(track, track[1:]):
        if t0 <= t <= t1:
            k = 0.0 if t1 == t0 else (t - t0) / (t1 - t0)
            return [x + (y - x) * k for x, y in zip(a, b)]
    return list(track[-1][1])
