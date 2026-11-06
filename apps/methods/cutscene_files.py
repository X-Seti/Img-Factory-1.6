#this belongs in apps/methods/cutscene_files.py - Version: 2
# X-Seti - November 02 2026 - IMG Factory 1.6 - Cutscene .cut / .dat files

"""
GTA III / VC / SA cutscene files (anim/cuts.img): the .cut script (sections
info, model, text, uncompress, motion, remove, extracol, peffect ... each
closed by 'end') and the .dat camera path (four keyframe tracks: FOV,
roll, camera position, target position). GTA IV .cut: [TAG] ... [/TAG]
blocks, one [SECTION_START] .. [SECTION_END] group per .wad part.
"""

##Methods list -
# build_cam_dat
# build_cut
# build_iv_cut
# cam_at
# iv_cut_parts
# parse_cam_dat
# parse_cut
# parse_iv_cut

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
# GTA IV .cut tags (rows split at spaces / tabs)
IV_CUT_COLUMNS = {
    'models': ["Index", "Model", "Animation", "Face animation"],
    'text': ["Start ms", "Length ms", "GXT key"],
    'variation': ["Model", "Component", "Drawable", "Texture", "Palette"],
    'remove': ["X", "Y", "Z", "Model"],
    'draw_distance': ["Time ms", "Flag", "Distance"],
    'offset': ["X", "Y", "Z"], 'player_start': ["X", "Y", "Z"],
    'cutscene_header': ["Value 1", "Value 2", "Value 3", "Value 4"],
}
IV_MARKERS = ('section_start', 'section_end')


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


def parse_iv_cut(text: str) -> Tuple[List[Tuple[str, List[str]]], list]: #vers 1
    """GTA IV .cut: ([(tag, rows)], [rows span or None]); markers have no rows."""
    import re
    secs, spans, cur, pos = [], [], None, 0
    for line in text.splitlines(keepends=True):
        s = line.strip()
        m = re.fullmatch(r'\[(/?)([A-Za-z_0-9]+)\]', s)
        if m and not m.group(1):
            tag = m.group(2).lower()
            if tag in IV_MARKERS:
                secs.append((tag, []))
                spans.append(None)
            elif cur is None:
                cur = (tag, [], pos + len(line))
        elif m and cur is not None and m.group(2).lower() == cur[0]:
            secs.append((cur[0], cur[1]))
            spans.append((cur[2], pos))
            cur = None
        elif cur is not None and s:
            cur[1].append(s)
        pos += len(line)
    return secs, spans


def build_iv_cut(text: str, spans: list, sections: List[Tuple[str, List[str]]]) -> str: #vers 1
    """IV .cut text with changed blocks' rows rewritten; the rest kept as it was."""
    old, _ = parse_iv_cut(text)
    nl = '\r\n' if '\r\n' in text else '\n'
    for (tag, rows), (_t, was), span in sorted(zip(sections, old, spans), key=lambda x: -(x[2] or (0,))[0]):
        if span is None or rows == was:
            continue
        a, b = span
        text = text[:a] + nl + ''.join(r + nl for r in rows) + text[b:]
    return text


def iv_cut_parts(sections: List[Tuple[str, List[str]]]) -> List[dict]: #vers 1
    """One dict per IV section: models [(index, model, anim, face anim)], anim, camera, duration s."""
    parts, cur = [], None
    for tag, rows in sections:
        if tag == 'section_start':
            cur = {'models': [], 'anim': '', 'camera': '', 'duration': 0.0, 'variation': []}
            parts.append(cur)
        elif tag == 'section_end':
            cur = None
        elif cur is not None:
            if tag == 'models':
                for r in rows:
                    f = r.split()
                    if len(f) >= 3:
                        cur['models'].append((f[0], f[1], f[2], f[3] if len(f) > 3 else ''))
            elif tag in ('anim', 'camera') and rows:
                cur[tag] = rows[0].strip()
            elif tag == 'duration' and rows:
                cur['duration'] = float(rows[0]) / 1000.0
            elif tag == 'variation':
                cur['variation'] = [[int(x) for x in r.split()] for r in rows if r.split()]
    return parts
