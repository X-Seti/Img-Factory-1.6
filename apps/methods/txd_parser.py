#this belongs in apps/methods/txd_parser.py - Version: 7
# X-Seti - October05 2026 - IMG Factory 1.6 - TXD parser for viewers

"""
parse_txd / load_txd for Model, Map, Vehicle, viewers; uses txd_reader.
"""

##Methods list -
# load_txd
# parse_txd

from typing import List


def parse_txd(data: bytes) -> List[dict]: #vers 7
    """Any texture file to dicts (mip 0 decoded), TXD Workshop reader."""
    from apps.methods.txd_reader import read_texture_file
    try:
        _kind, texs = read_texture_file(data, levels=False)
    except Exception as e:
        print(f"txd_parser: parse_txd error: {e}")
        return []
    out = []
    for t in texs:
        if not t.get('rgba_data') or not t.get('width') or not t.get('height'):
            continue
        ff = t.get('filter_flags', 0) or 0
        t.setdefault('mask', t.get('alpha_name', '') or '')
        t.setdefault('mip_count', t.get('mipmaps', 1))
        t.setdefault('platform', t.get('platform_id', 0))
        t.setdefault('filter_mode', ff & 0xFF)
        t.setdefault('wrap_u', (ff >> 8) & 0x0F if ff else 1)
        t.setdefault('wrap_v', (ff >> 12) & 0x0F if ff else 1)
        out.append(t)
    return out


def load_txd(path: str) -> List[dict]: #vers 2
    """Load and parse a texture file from disk."""
    from apps.methods.txd_reader import read_texture_file
    try:
        with open(path, 'rb') as f:
            data = f.read()
        if path.lower().endswith(('.xtx', '.chk')):
            return read_texture_file(data, path, levels=False)[1]
        return parse_txd(data)
    except Exception as e:
        print(f"txd_parser: load_txd({path}) error: {e}")
        return []
