#this belongs in apps/methods/weapon_files.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - Weapon data files

"""
weapon.dat (III / VC / SA), melee.dat and ar_stats.dat (SA) as editable
rows. Lines keep their own spacing: a cell edit replaces only that word.
Column names and the hex flag meanings come from the files' own headers.
"""

##Methods list -
# data_rows
# flag_names
# melee_combos
# set_word
# split_words

import re
from typing import Dict, List, Tuple

SA_GUN = ["Type", "Fire type", "Target range", "Weapon range", "Model 1", "Model 2", "Slot",
          "Anim group", "Clip", "Damage", "Offset X", "Offset Y", "Offset Z", "Skill", "Stat needed",
          "Accuracy", "Move speed", "Loop start", "Loop end", "Fire", "Loop2 start", "Loop2 end",
          "Fire2", "Breakout", "Flags", "Speed", "Radius", "Lifespan", "Spread"]
SA_MELEE = ["Type", "Fire type", "Target range", "Weapon range", "Model 1", "Model 2", "Slot",
            "Base combo", "Combos", "Flags", "Stealth anim"]
SA_AIM = ["Anim group", "Aim X", "Aim Z", "Duck X", "Duck Z", "Reload A", "Reload B",
          "Crouch reload A", "Crouch reload B"]
VC_WEAPON = ["Name", "Fire type", "Range", "Fire rate", "Reload", "Ammo", "Damage", "Speed", "Radius",
             "Lifespan", "Spread", "Offset X", "Offset Y", "Offset Z", "Anim", "Anim 2", "Loop start",
             "Loop end", "Fire", "Loop2 start", "Loop2 end", "Fire2", "Breakout", "Model 1", "Model 2",
             "Flags", "Slot"]
III_WEAPON = VC_WEAPON[:16] + ["Loop start", "Loop end", "Fire", "Fire2", "Model", "Flags"]
AR_STATS = ["Id", "Stat", "Value"]
COMBO_ATTACK = ["Key", "Hit", "Chain", "Radius", "Hit level", "Damage", "Hit anim", "Alt hit", "Ground loop"]

# hex flag digit meanings, right-most digit first
SA_GUN_FLAGS = [("CANAIM", "AIMWITHARM", "1STPERSON", "ONLYFREEAIM"), ("MOVEAIM", "MOVEFIRE", "", ""),
                ("THROW", "HEAVY", "CONTINUOUSFIRE", "TWIN_PISTOL"),
                ("RELOAD", "CROUCHFIRE", "RELOAD2START", "LONG_RELOAD"), ("SLOWSDWN", "RANDSPEED", "EXPANDS", "")]
VC_FLAGS = [("USE_GRAVITY", "SLOWS_DOWN", "DISSIPATES", "RAND_SPEED"), ("EXPANDS", "EXPLODES", "CANAIM", "CANAIM_WITHARM"),
            ("1ST_PERSON", "HEAVY", "THROW", "RELOAD_LOOP2START"), ("USE_2ND", "GROUND_2ND", "FINISH_3RD", "RELOAD"),
            ("FIGHTMODE", "CROUCHFIRE", "COP3_RD", "GROUND_3RD"), ("PARTIALATTACK", "ANIMDETONATE", "", "")]
III_FLAGS = ["USE_GRAVITY", "SLOWS_DOWN", "DISSIPATES", "RAND_SPEED", "EXPANDS", "EXPLODES", "CANAIM",
             "CANAIM_WITHARM", "1ST_PERSON", "HEAVY", "THROW"]      # decimal bit flags
MELEE_FLAGS = [("ATTACK_2", "ATTACK_3", "GROUND", "RUNNING"), ("FALL_1", "FALL_2", "FALL_3", "FALL_GROUND"),
               ("FALL_RUN", "BLOCK", "OWN_IDLE", ""), ("NOFALL_1", "NOFALL_2", "NOFALL_3", "")]


def split_words(line: str) -> List[str]: #vers 1
    """[lead space, word, space, word, ...]: joining gives the line back."""
    return re.split(r'(\S+)', line)[:-1] if line.strip() else [line]


def set_word(line: str, index: int, value: str) -> str: #vers 1
    """line with its index-th word (0 = first) replaced; spacing kept, new words appended."""
    parts = re.split(r'(\S+)', line)
    words = parts[1::2]
    if index < len(words):
        parts[1 + 2 * index] = value
        return ''.join(parts)
    return line.rstrip() + ''.join('\t' + w for w in [''] * (index - len(words)) + [value])


def data_rows(text: str, marker: str = None) -> List[Tuple[int, List[str]]]: #vers 1
    """(line index, words) of each data line; marker ('$', '\\xa3', '%') keeps only those lines
    (marker dropped from the words); None keeps every non-comment, non-blank line."""
    out = []
    for i, line in enumerate(text.split('\n')):
        s = line.strip()
        if not s or s.startswith('#') or s.upper().startswith('ENDWEAPONDATA'):
            continue
        words = s.split()
        if marker is not None:
            if words[0] != marker:
                continue
            words = words[1:]
        elif words[0] in ('$', '\xa3', '%'):
            continue
        out.append((i, words))
    return out


def flag_names(hex_text: str, table) -> List[str]: #vers 1
    """Names set in a flags word: hex digit table (right-most digit first), or III_FLAGS (decimal)."""
    try:
        v = int(hex_text, 10 if table is III_FLAGS else 16)
    except ValueError:
        return []
    if table is III_FLAGS:
        return [n for b, n in enumerate(III_FLAGS) if v & (1 << b)]
    out = []
    for d, names in enumerate(table):
        digit = (v >> (4 * d)) & 0xF
        out += [n for b, n in enumerate(names) if n and digit & (1 << b)]
    return out


def melee_combos(text: str) -> List[Dict]: #vers 1
    """melee.dat blocks: name, first / last line, rows [(line index, words)]."""
    out, cur = [], None
    for i, line in enumerate(text.split('\n')):
        s = line.strip()
        if not s or s.startswith('#'):
            continue
        w = s.split()
        if w[0] in ('START_COMBO', 'START_LEVELS'):
            cur = {'name': w[1] if len(w) > 1 else 'LEVELS', 'start': i, 'rows': []}
            out.append(cur)
        elif w[0] == 'END_COMBO':
            if cur is not None:
                cur['end'] = i
            cur = None
        elif cur is not None:
            cur['rows'].append((i, w))
    return out
