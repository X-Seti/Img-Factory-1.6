#!/usr/bin/env python3
#this belongs in apps/components/Map_Editor/depends/map_changes.py - Version: 3
# X-Seti - September23 2026 - IMG Factory 1.6 - Map Workshop change tracking and save points

"""
Map Workshop change tracking and save points - per-IPL state signatures,
snapshot / restore of edited IPL data, save point files (json.gz).
"""

##Methods list -
# apply_ide_raw_lines
# collect_ipl_state
# ipl_signatures
# list_savepoints
# prune_savepoints
# read_savepoint
# replace_ipl_state
# state_from_json
# state_to_json
# write_ide_raw_lines
# write_savepoint
# _freeze
# _inst_key
# _src

import dataclasses
import gzip
import json
import os
import time
from typing import Dict, List

# (category key, loader attribute)
CATEGORIES = [('inst', 'instances'), ('cull', 'culls'), ('zone', 'zones'), ('path', 'paths'),
              ('grge', 'grges'), ('enex', 'enexes'), ('occl', 'occls'), ('auzo', 'auzos')]
_SKIP = ('source_ipl', 'line_no', 'raw_line')


def _src(o): #vers 1
    """Source IPL display name of one entry."""
    return o.get('source_ipl') if isinstance(o, dict) else getattr(o, 'source_ipl', None)


def _freeze(v): #vers 1
    """Hashable snapshot of an entry, ignoring source/line bookkeeping."""
    if dataclasses.is_dataclass(v):
        return tuple(_freeze(getattr(v, f.name)) for f in dataclasses.fields(v) if f.name not in _SKIP)
    if isinstance(v, dict):
        return tuple(sorted((k, _freeze(x)) for k, x in v.items() if k not in _SKIP))
    if isinstance(v, (list, tuple)):
        return tuple(_freeze(x) for x in v)
    if isinstance(v, float):
        return round(v, 5)
    return v


def _inst_key(i): #vers 1
    """Fast state tuple for one IPLInstance."""
    return (i.model_id, i.model_name, i.interior, round(i.pos_x, 5), round(i.pos_y, 5),
            round(i.pos_z, 5), round(i.rot_x, 6), round(i.rot_y, 6), round(i.rot_z, 6),
            round(i.rot_w, 6), i.lod_index, round(i.scale_x, 5), round(i.scale_y, 5),
            round(i.scale_z, 5))


def ipl_signatures(loader) -> Dict[str, int]: #vers 1
    """{ipl display name: hash of all its entries} over every category."""
    acc: Dict[str, list] = {}
    for inst in getattr(loader, 'instances', []):
        acc.setdefault(inst.source_ipl, []).append(('i', _inst_key(inst)))
    for cat, attr in CATEGORIES[1:]:
        for o in getattr(loader, attr, None) or []:
            acc.setdefault(_src(o), []).append((cat, _freeze(o)))
    return {k: hash(tuple(v)) for k, v in acc.items() if k}


def collect_ipl_state(loader, names) -> Dict[str, Dict[str, list]]: #vers 1
    """References to every entry of the named IPLs, grouped by category."""
    names = set(names)
    out = {n: {cat: [] for cat, _ in CATEGORIES} for n in names}
    for cat, attr in CATEGORIES:
        for o in getattr(loader, attr, None) or []:
            s = _src(o)
            if s in names:
                out[s][cat].append(o)
    return out


def replace_ipl_state(loader, state: Dict[str, Dict[str, list]]): #vers 1
    """Swap the named IPLs' entries in the loader for the given ones."""
    names = set(state)
    for cat, attr in CATEGORIES:
        lst = getattr(loader, attr, None)
        if lst is None:
            continue
        lst[:] = [o for o in lst if _src(o) not in names]
        for n in state:
            lst.extend(state[n].get(cat, []))


def state_to_json(state) -> dict: #vers 1
    """Serialisable copy of collect_ipl_state output."""
    def one(o):
        return dict(o) if isinstance(o, dict) else dataclasses.asdict(o)
    return {n: {cat: [one(o) for o in items] for cat, items in cats.items()}
            for n, cats in state.items()}


def state_from_json(data) -> Dict[str, Dict[str, list]]: #vers 1
    """Rebuild entries (fresh objects) from state_to_json output."""
    from apps.methods.gta_dat_parser import (IPLInstance, CullEntry, PathGroup, PathNode,
                                             GrgeEntry, EnexEntry, OcclEntry, AuzoEntry)
    classes = {'inst': IPLInstance, 'cull': CullEntry, 'path': PathGroup, 'grge': GrgeEntry,
               'enex': EnexEntry, 'occl': OcclEntry, 'auzo': AuzoEntry}

    def build(cls, d):
        names = {f.name for f in dataclasses.fields(cls)}
        return cls(**{k: v for k, v in d.items() if k in names})

    out = {}
    for n, cats in data.items():
        out[n] = {}
        for cat, items in cats.items():
            if cat == 'zone':
                out[n][cat] = [dict(d) for d in items]
            elif cat == 'path':
                groups = []
                for d in items:
                    g = build(PathGroup, {k: v for k, v in d.items() if k != 'nodes'})
                    g.nodes = [build(PathNode, nd) for nd in d.get('nodes', [])]
                    groups.append(g)
                out[n][cat] = groups
            elif cat in classes:
                out[n][cat] = [build(classes[cat], d) for d in items]
    return out


def write_savepoint(folder: str, label: str, meta: dict, state) -> str: #vers 1
    """Write one save point file; returns its path."""
    os.makedirs(folder, exist_ok=True)
    stamp = time.strftime('%Y%m%d_%H%M%S')
    path = os.path.join(folder, f"savepoint_{stamp}.json.gz")
    n = 1
    while os.path.exists(path):
        n += 1
        path = os.path.join(folder, f"savepoint_{stamp}_{n}.json.gz")
    body = {'version': 1, 'created': time.time(), 'label': label, 'meta': meta,
            'ipls': state_to_json(state)}
    tmp = path + '.tmp'
    with gzip.open(tmp, 'wt', encoding='utf-8') as f:
        json.dump(body, f)
    os.replace(tmp, path)
    return path


def read_savepoint(path: str) -> dict: #vers 1
    """Load a save point file (state still in json form under 'ipls')."""
    with gzip.open(path, 'rt', encoding='utf-8') as f:
        return json.load(f)


def list_savepoints(folder: str) -> List[dict]: #vers 1
    """Newest-first [{path, created, label, ipls}] without loading entries."""
    out = []
    if not os.path.isdir(folder):
        return out
    for fn in os.listdir(folder):
        if fn.startswith('savepoint_') and fn.endswith('.json.gz'):
            p = os.path.join(folder, fn)
            try:
                d = read_savepoint(p)
                out.append({'path': p, 'created': d.get('created', 0), 'label': d.get('label', ''),
                            'ipls': sorted(d.get('ipls', {}))})
            except Exception as e:
                print(f"[Map save points] unreadable {p}: {e}")
    return sorted(out, key=lambda x: x['created'], reverse=True)


def prune_savepoints(folder: str, keep: int): #vers 1
    """Keep only the newest `keep` automatic save points."""
    autos = [s for s in list_savepoints(folder) if s['label'].startswith('Auto')]
    for s in autos[max(0, keep):]:
        try:
            os.remove(s['path'])
        except OSError as e:
            print(f"[Map save points] couldn't remove {s['path']}: {e}")



def apply_ide_raw_lines(path: str, rows) -> bytes: #vers 1
    """File bytes with [(line_no, model_id, raw)] replacements; line endings kept, IDs checked."""
    with open(path, 'rb') as f:
        lines = f.read().decode('latin-1').splitlines(keepends=True)
    for line_no, model_id, raw in rows:
        i = line_no - 1
        if not (0 <= i < len(lines)):
            raise ValueError(f"{os.path.basename(path)}: line {line_no} out of range")
        head = lines[i].split('#')[0].split(',')[0].strip()
        if head != str(model_id):
            raise ValueError(f"{os.path.basename(path)} line {line_no} holds '{head}', expected {model_id}")
        eol = lines[i][len(lines[i].rstrip('\r\n')):]
        lines[i] = raw.rstrip('\r\n') + eol
    return ''.join(lines).encode('latin-1')


def write_ide_raw_lines(path: str, rows) -> None: #vers 1
    """Write edited IDE lines into one .ide file (backup first)."""
    from apps.methods.file_backup import safe_write_bytes
    safe_write_bytes(path, apply_ide_raw_lines(path, rows), label=f"IDE edit {os.path.basename(path)}")
