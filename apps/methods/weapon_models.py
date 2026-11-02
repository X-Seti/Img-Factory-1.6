#this belongs in apps/methods/weapon_models.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - Weapon models, textures, collision

"""
Weapon model list from the game's default.ide (weap section; III: objs ids
170-184), PNG export / import of TXD textures, and one-model COL records
inside a COL archive.
"""

##Methods list -
# _find
# col_index
# export_textures_png
# replace_col_record
# replace_texture
# weapon_models

import os
from typing import Dict, List, Tuple

III_WEAPON_IDS = range(170, 185)                 # default.ide: "# weapons: 170-184"


def _find(folder: str, name: str) -> str: #vers 1
    """Case-insensitive child path, or ''."""
    if not folder or not os.path.isdir(folder):
        return ''
    hit = next((f for f in os.listdir(folder) if f.lower() == name.lower()), None)
    return os.path.join(folder, hit) if hit else ''


def weapon_models(game_root: str) -> List[Tuple[int, str, str]]: #vers 1
    """[(id, model, txd)] of every weapon model in data/default.ide."""
    ide = _find(_find(game_root, 'data'), 'default.ide')
    if not ide:
        return []
    weap, objs, section = [], [], ''
    with open(ide, encoding='latin-1') as f:
        for line in f:
            body = line.split('#')[0].strip()
            low = body.lower()
            if low in ('objs', 'weap', 'tobj', 'peds', 'cars', 'hier', 'anim', 'txdp', 'path', '2dfx'):
                section = low
                continue
            if low == 'end':
                section = ''
                continue
            parts = [p.strip() for p in body.split(',')]
            if len(parts) < 3 or not parts[0].isdigit():
                continue
            row = (int(parts[0]), parts[1], parts[2])
            if section == 'weap':
                weap.append(row)
            elif section == 'objs' and row[0] in III_WEAPON_IDS:
                objs.append(row)
    return weap or objs


def col_index(arc) -> Dict[str, object]: #vers 1
    """{model name: IMG entry} for every model in the archive's .col entries."""
    from apps.methods.col_splice import _rec_name, split_records
    out = {}
    for e in arc.entries:
        if e.name.lower().endswith('.col'):
            recs, _tail = split_records(arc.read_entry_data(e))
            for r in recs:
                out.setdefault(_rec_name(r), e)
    return out


def replace_col_record(col_data: bytes, model: str, record: bytes) -> bytes: #vers 1
    """COL archive bytes with model's record swapped for record."""
    from apps.methods.col_splice import _rec_name, split_records
    recs, tail = split_records(col_data)
    hit = [i for i, r in enumerate(recs) if _rec_name(r) == model.lower()]
    if not hit:
        raise ValueError(f"{model} not in this COL file")
    recs[hit[0]] = record
    return b''.join(recs) + tail


def export_textures_png(txd_data: bytes, folder: str) -> int: #vers 1
    """Every texture of a TXD as name.png in folder; count written."""
    from PIL import Image
    from apps.methods.txd_parser import parse_txd
    n = 0
    for t in parse_txd(txd_data):
        rgba = t.get('rgba_data')
        if not rgba or not t.get('width') or not t.get('height'):
            continue
        Image.frombytes('RGBA', (t['width'], t['height']), bytes(rgba)).save(
            os.path.join(folder, t['name'] + '.png'))
        n += 1
    return n


def replace_texture(txd_data: bytes, name: str, png_path: str) -> bytes: #vers 1
    """TXD bytes with texture name replaced by a PNG (format kept, re-encoded)."""
    from PIL import Image
    from apps.methods.txd_parser import parse_txd
    from apps.methods.txd_splice import tag_loaded_texture, txd_from_textures
    texs = parse_txd(txd_data)
    for t in texs:
        tag_loaded_texture(t)
    t = next((x for x in texs if x['name'].lower() == name.lower()), None)
    if t is None:
        raise ValueError(f"{name} not in this TXD")
    img = Image.open(png_path).convert('RGBA')
    t.update({'width': img.width, 'height': img.height, 'rgba_data': img.tobytes(),
              'mipmaps': 1, 'mip_count': 1, 'mipmap_levels': [], 'compressed_data': None,
              'original_bgra_data': None, 'has_alpha': True})
    return txd_from_textures(texs, txd_data)
