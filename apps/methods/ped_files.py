#this belongs in apps/methods/ped_files.py - Version: 3
# X-Seti - October 21 2026 - IMG Factory 1.6 - Ped data files (III / VC / SA)

"""
Ped data files: the peds section of peds.ide (SA, IV) / default.ide (III, VC),
plus the ped data files (ped, pedgrp, pedstats, animgrp, clothes, shopping,
popcycle, SA Decision), ped.ifp, anim.img and gta3.img of a game root.
Rows are edited in place; every other line is written back unchanged.
"""

##Methods list -
# _resolve
# game_ped_paths
# ped_data_files
# read_ped_ide
# write_ped_ide

import os
import re
from typing import Dict, List, Optional, Tuple

PED_COLUMNS = {
    'gta3': ['ID', 'Model', 'TXD', 'Type', 'Behaviour', 'Anim group', 'Cars'],
    'vc':   ['ID', 'Model', 'TXD', 'Type', 'Behaviour', 'Anim group', 'Cars',
             'Anim file', 'Radio 1', 'Radio 2'],
    'sa':   ['ID', 'Model', 'TXD', 'Type', 'Behaviour', 'Anim group', 'Cars', 'Flags',
             'Anim file', 'Radio 1', 'Radio 2', 'Voice type', 'Voice 1', 'Voice 2'],
    'iv':   ['Model', 'Props', 'Type', 'Move clips', 'Gestures', 'Phone gestures', 'Facials',
             'Visemes', 'Flags', 'Strafe clips', 'Melee clips', 'Radio 1', 'Radio 2', 'Ped type',
             'Voice 1', 'Voice 2'],
}


def _resolve(root: str, rel: str) -> Optional[str]: #vers 1
    """Case-insensitive path under root, or None."""
    cur = root
    for part in rel.split('/'):
        if not os.path.isdir(cur):
            return None
        hit = next((n for n in os.listdir(cur) if n.lower() == part.lower()), None)
        if hit is None:
            return None
        cur = os.path.join(cur, hit)
    return cur


PED_DATA_FILES = ('ped.dat', 'pedgrp.dat', 'pedstats.dat', 'animgrp.dat', 'clothes.dat',
                  'shopping.dat', 'popcycle.dat', 'pedpersonality.dat', 'pedpopulation.dat',
                  'pedProps.dat', 'pedVariations.dat')


def ped_data_files(root: str) -> List[Tuple[str, str]]: #vers 2
    """(label, path) of every ped-related data file present (incl. Decision)."""
    data = _resolve(root, 'data') or _resolve(root, 'common/data')
    if not data:
        return []
    out = []
    for rel in PED_DATA_FILES:
        p = _resolve(data, rel)
        if p:
            out.append((os.path.basename(p), p))
    dec = _resolve(data, 'Decision')
    if dec:
        for dirpath, _dirs, files in sorted(os.walk(dec)):
            for n in sorted(files):
                if n.lower().endswith(('.ped', '.grp', '.txt')):
                    p = os.path.join(dirpath, n)
                    out.append(("Decision/" + os.path.relpath(p, dec), p))
    return out


def game_ped_paths(root: str) -> Dict[str, Optional[str]]: #vers 3
    """game, ide, data files, ifp, anim.img and gta3.img paths for a III / VC / SA / IV root."""
    iv_ide = _resolve(root, 'common/data/peds.ide')
    sa_ide = None if iv_ide else _resolve(root, 'data/peds.ide')
    ide = iv_ide or sa_ide or _resolve(root, 'data/default.ide')
    game = 'iv' if iv_ide else 'sa' if sa_ide else None
    if game is None and ide:
        _lines, rows = read_ped_ide(ide)
        game = 'vc' if rows and len(rows[0][1]) >= 8 else 'gta3'
    return {'game': game, 'ide': ide,
            'ifp': _resolve(root, 'anim/ped.ifp'), 'anim_img': _resolve(root, 'anim/anim.img'),
            'img': _resolve(root, 'models/gta3.img'), 'data_files': ped_data_files(root)}


def read_ped_ide(path: str) -> Tuple[List[str], List[Tuple[int, List[str], str]]]: #vers 2
    """(all lines, [(line index, fields, trailing comment)]) of the peds section."""
    with open(path, 'r', encoding='latin-1') as f:
        lines = f.read().splitlines()
    rows, inside = [], False
    for i, line in enumerate(lines):
        body, _sep, comment = line.partition('#')
        word = body.strip().lower()
        if not inside:
            inside = word == 'peds'
            continue
        if word == 'end':
            break
        if not word:
            continue
        fields = [p.strip() for p in re.split(r',', body) if p.strip() != '']
        if len(fields) >= 6 and (fields[0].lstrip('-').isdigit() or len(fields) >= 12):
            rows.append((i, fields, comment))
    return lines, rows


def write_ped_ide(path: str, rows: List[Tuple[int, List[str], str]]) -> int: #vers 1
    """Rewrite only the changed rows over their own lines (backup first); count changed."""
    from apps.methods.file_backup import safe_write_bytes
    with open(path, 'rb') as f:
        raw = f.read()
    nl = '\r\n' if b'\r\n' in raw else '\n'
    lines, old_rows = read_ped_ide(path)
    old = {i: fields for i, fields, _c in old_rows}
    changed = 0
    for i, fields, comment in rows:
        if old.get(i) == fields:
            continue
        lines[i] = ', '.join(fields) + (f"\t#{comment}" if comment else '')
        changed += 1
    if changed:
        text = nl.join(lines) + (nl if raw.endswith(b'\n') else '')
        safe_write_bytes(path, text.encode('latin-1'), f"Ped IDE: {os.path.basename(path)}")
    return changed
