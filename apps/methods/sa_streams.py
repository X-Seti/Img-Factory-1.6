#this belongs in apps/methods/sa_streams.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - SA audio streams

"""
GTA SA audio/streams packs: track lookup (CONFIG/StrmPaks.dat, TrakLkup.dat),
XOR decryption and the Ogg Vorbis data of one track; cutscene name -> track.
"""

##Methods list -
# _find
# cutscene_ogg
# read_track_ogg
# read_track_table

import os
import struct
from typing import List, Optional, Tuple

import numpy as np

STREAM_KEY = bytes.fromhex('EA3AC4A19AA814F348B0D7239DE8FFF1')   # XOR, by file offset
TRACK_HEADER = 8068                                  # beat table + length, then OggS

# gta_sa.exe 1.0 cutscene audio table: cutscene name -> track index
SA_CUTSCENE_TRACKS = {
    'bcesar2': 626, 'bcesar4': 627, 'bcesa4w': 628, 'bcesar5': 629, 'bcesa5w': 630,
    'bcras1': 631, 'bcras2': 632, 'bhill1': 633, 'bhill2': 634, 'bhill3a': 635, 'bhill3b': 636,
    'bhill3c': 637, 'bhill5a': 638, 'bhill5b': 639, 'cas_1a': 640, 'cas_1b': 642, 'cas_2': 643,
    'cas_3': 644, 'cas_4a': 645, 'cas_4b': 646, 'cas_4c': 647, 'cas_5a': 648, 'cas_6a': 649,
    'cas6b_1': 650, 'cas6b_2': 651, 'cas_7b': 652, 'cas_9a1': 653, 'cas_9a2': 654,
    'cas_11a': 641, 'cat_1': 655, 'cat_2': 656, 'cat_3': 657, 'cat_4': 658, 'cesar1a': 659,
    'cesar2a': 660, 'crash1a': 661, 'crash2a': 662, 'crash3a': 663, 'crashv1': 664,
    'crasv2a': 665, 'crasv2b': 666, 'd10_alt': 667, 'd8_alt': 668, 'desert1': 671,
    'desert2': 672, 'desert3': 673, 'desert4': 674, 'desert6': 675, 'desert8': 676,
    'desert9': 677, 'des_10a': 678, 'des_10b': 679, 'doc_2': 680, 'epilog': 681,
    'farl_2a': 682, 'farl_3a': 683, 'farl_3b': 684, 'farl_4a': 685, 'farl_5a': 686,
    'final1a': 687, 'final2a': 688, 'final2b': 689, 'garag1b': 690, 'garag1c': 691,
    'garag3a': 692, 'grove1a': 693, 'grove1b': 694, 'grove1c': 695, 'grove2': 696,
    'heist1a': 697, 'heist2a': 698, 'heist4a': 699, 'heist5a': 700, 'heist6a': 701,
    'heist8a': 702, 'intro1a': 703, 'intro1b': 704, 'intro2a': 705, 'prolog1': 706,
    'prolog2': 707, 'prolog3': 708, 'riot_1a': 709, 'riot_1b': 710, 'riot_2': 711,
    'riot_4a': 712, 'riot_4b': 713, 'riot_4c': 714, 'riot_4d': 715, 'riot4e1': 716,
    'riot4e2': 717, 'ryder1a': 718, 'ryder2a': 719, 'ryder3a': 720, 'scrash1': 721,
    'scrash2': 722, 'smoke1a': 723, 'smoke1b': 724, 'smoke2a': 725, 'smoke2b': 726,
    'smoke3a': 727, 'smoke4a': 728, 'steal_1': 729, 'steal_2': 730, 'steal_4': 731,
    'steal_5': 732, 'strap1a': 733, 'strap2a': 734, 'strap3a': 735, 'strap4a': 736,
    'strp4b1': 737, 'strp4b2': 738, 'sweet1a': 739, 'sweet1b': 740, 'sweet1c': 741,
    'sweet2a': 742, 'sweet2b': 743, 'sweet3a': 744, 'sweet3b': 745, 'sweet4a': 746,
    'sweet5a': 747, 'sweet6a': 748, 'sweet6b': 749, 'sweet7a': 750, 'synd_2a': 751,
    'synd_2b': 752, 'synd_3a': 753, 'synd_4a': 754, 'synd_4b': 755, 'synd_7': 756,
    'truth_1': 758, 'truth_2': 757, 'w2_alt': 759, 'woozi1a': 761, 'woozi1b': 762,
    'woozie2': 760, 'woozie4': 763, 'zero_1': 764, 'zero_2': 765, 'zero_4': 766, 'date1a': 670,
    'date1b': 669, 'date2a': 670, 'date2b': 669, 'date3a': 670, 'date3b': 669, 'date4a': 670,
    'date4b': 669, 'date5a': 670, 'date5b': 669, 'date6a': 670, 'date6b': 669,
}


def _find(folder: str, name: str) -> Optional[str]: #vers 1
    """Case-insensitive child of folder, or None."""
    if not folder or not os.path.isdir(folder):
        return None
    hit = next((f for f in os.listdir(folder) if f.lower() == name.lower()), None)
    return os.path.join(folder, hit) if hit else None


def read_track_table(audio_dir: str) -> List[Tuple[str, int, int]]: #vers 1
    """[(pack file path, offset, length)] per track index."""
    cfg = _find(audio_dir, 'CONFIG')
    paks, lkup = _find(cfg, 'StrmPaks.dat'), _find(cfg, 'TrakLkup.dat')
    streams = _find(audio_dir, 'streams')
    if not (paks and lkup and streams):
        return []
    with open(paks, 'rb') as f:
        raw = f.read()
    names = [raw[i:i + 16].split(b'\0')[0].decode('latin-1') for i in range(0, len(raw) - 15, 16)]
    with open(lkup, 'rb') as f:
        raw = f.read()
    out = []
    for i in range(len(raw) // 12):
        pack, off, length = struct.unpack_from('<B3xII', raw, i * 12)
        path = _find(streams, names[pack]) if pack < len(names) and names[pack] else None
        out.append((path or '', off, length))
    return out


def read_track_ogg(pack_path: str, offset: int, length: int) -> Optional[bytes]: #vers 1
    """Decrypted Ogg Vorbis bytes of one track, None if not Ogg."""
    start = offset + TRACK_HEADER
    with open(pack_path, 'rb') as f:
        f.seek(start)
        data = np.frombuffer(f.read(length), dtype=np.uint8)
    key = np.frombuffer(STREAM_KEY, dtype=np.uint8)
    key = np.roll(key, -(start % 16))
    plain = (data ^ np.resize(key, len(data))).tobytes()
    return plain if plain[:4] == b'OggS' else None


def cutscene_ogg(audio_dir: str, cutscene: str) -> Optional[bytes]: #vers 1
    """Ogg sound of an SA cutscene from audio/streams/CUTSCENE, or None."""
    track = SA_CUTSCENE_TRACKS.get(cutscene.lower())
    if track is None:
        return None
    table = read_track_table(audio_dir)
    if track >= len(table) or not table[track][0]:
        return None
    return read_track_ogg(*table[track])
