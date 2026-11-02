#this belongs in apps/methods/cj_player.py - Version: 3
# X-Seti - November 02 2026 - IMG Factory 1.6 - SA player (CJ) clothes

"""
SA player built from models/player.img parts: data/shopping.dat 'Clothes'
items (texture, name, model, component), data/clothes.dat rules (SETC,
HIDE, CUTS for cutscene parts), outfit resolving, and merging the chosen
parts (one shared 37-bone skeleton) into a single DFF with its textures
named, plus one TXD of the parts' textures.
"""

##Methods list -
# merge_parts
# merge_txds
# parse_clothes_dat
# parse_shopping_clothes
# resolve_outfit
# texture_part

import struct
from typing import Dict, List, Optional, Tuple

# shopping.dat clothes type -> component
COMPONENTS = {0: 'torso', 1: 'head', 2: 'legs', 3: 'feet', 13: 'necklace', 14: 'watch',
              15: 'glasses', 16: 'hat', 17: 'special'}
# new game CJ: component -> (model, texture dictionary); hands always worn
DEFAULT_OUTFIT = {'torso': ('vest', 'vest'), 'head': ('head', 'player_face'), 'legs': ('jeans', 'jeansdenim'),
                  'feet': ('sneaker', 'sneakerbincblk'), 'hands': ('hands', '')}


def parse_clothes_dat(text: str) -> dict: #vers 1
    """clothes.dat rules: setc [(model, comp, new model, texture)], tex {model: texture},
    hide [(model, comp)], cuts {model: cutscene model}."""
    rules = {'setc': [], 'tex': {}, 'hide': [], 'cuts': {}}
    ignored = set()
    for raw in text.replace('\r', '').split('\n'):
        p = raw.split('#', 1)[0].split()
        if not p:
            continue
        op = p[0].upper()
        if op == 'IGNORE' and len(p) > 1:
            ignored.add(p[1].lower())
        elif op == 'ENDIGNORE' and len(p) > 1:
            ignored.discard(p[1].lower())
        elif op == 'SETC' and len(p) >= 4 and p[1].lower() not in ignored:
            rules['setc'].append((p[1].lower(), p[2].lower(), p[3].lower(),
                                  p[4].lower() if len(p) > 4 and p[4] != '-' else ''))
        elif op == 'TEX' and len(p) >= 3:
            rules['tex'][p[1].lower()] = p[2].lower()
        elif op == 'HIDE' and len(p) >= 3 and p[1].lower() not in ignored:
            rules['hide'].append((p[1].lower(), p[2].lower()))
        elif op == 'CUTS' and len(p) >= 3:
            rules['cuts'][p[1].lower()] = p[2].lower()
    return rules


def parse_shopping_clothes(text: str) -> List[dict]: #vers 2
    """shopping.dat Clothes and Haircuts items: texture, name key, model, component, price."""
    out, inside = [], False
    for raw in text.replace('\r', '').split('\n'):
        p = raw.split('#', 1)[0].split()
        if not p:
            continue
        if p[0].lower() == 'section':
            inside = len(p) > 1 and p[1].lower() in ('clothes', 'haircuts')
            continue
        if not inside or p[0].lower() == 'end' or len(p) < 4 or not p[3].lstrip('-').isdigit():
            continue
        comp = COMPONENTS.get(int(p[3]))
        if comp:
            price = next((int(x) for x in reversed(p[4:]) if x.isdigit()), 0)
            out.append({'texture': p[0].lower(), 'name': p[1], 'model': p[2].lower(),
                        'component': comp, 'price': price})
    return out


def resolve_outfit(outfit: Dict[str, Tuple[str, str]], rules: Optional[dict],
                   cutscene: bool = False) -> List[Tuple[str, str, str]]: #vers 2
    """Worn parts after clothes.dat: [(component, model, texture dictionary)].
    SETC forces components, HIDE drops them, CUTS swaps in cutscene models."""
    wear = dict(outfit)
    rules = rules or {'setc': [], 'tex': {}, 'hide': [], 'cuts': {}}
    worn = {m for m, _t in wear.values()}
    for model, comp, new, tex in rules['setc']:
        if model in worn:
            old = wear.get(comp, ('', ''))
            wear[comp] = (new, tex or (old[1] if old[0] == new else ''))   # '-' keeps the texture
    worn = {m for m, _t in wear.values()}
    for model, comp in rules['hide']:
        if model in worn and comp in wear:
            del wear[comp]
    out = []
    for comp, (model, tex) in wear.items():
        if not model:
            continue
        tex = rules['tex'].get(model, tex)
        if cutscene:
            model = rules['cuts'].get(model, model)
        out.append((comp, model, tex))
    return out


def merge_parts(parts: List[bytes]) -> bytes: #vers 1
    """One DFF from player parts sharing the skeleton: the first part's clump with every
    other part's geometries and atomics appended."""
    from apps.methods import rw_chunks as rw
    if not parts:
        raise ValueError("No parts")
    out = parts[0]
    for extra in parts[1:]:
        roots = rw.parse_rw(out)
        clump = roots[0]
        gl = next(c for c in clump.children if c.type == 0x1A)
        n_geo = struct.unpack_from('<i', out, gl.children[0].data_start)[0]
        ex = rw.parse_rw(extra)[0]
        egl = next(c for c in ex.children if c.type == 0x1A)
        geos = [c for c in egl.children if c.type == 0x0F]
        atoms = [c for c in ex.children if c.type == 0x14]
        buf = out
        last_geo = [c for c in gl.children if c.type == 0x0F][-1]
        buf = rw.insert_section(buf, last_geo, b''.join(bytes(extra[g.offset:g.end]) for g in geos))
        clump = rw.parse_rw(buf)[0]
        gl = next(c for c in clump.children if c.type == 0x1A)
        b = bytearray(buf)
        struct.pack_into('<i', b, gl.children[0].data_start, n_geo + len(geos))
        buf = bytes(b)
        new_atoms = bytearray()
        for a in atoms:
            chunk = bytearray(extra[a.offset:a.end])
            gi = struct.unpack_from('<i', chunk, 24 + 4)[0]         # struct: frame, geometry, flags
            struct.pack_into('<i', chunk, 24 + 4, n_geo + gi)
            new_atoms += chunk
        clump = rw.parse_rw(buf)[0]
        last_atom = [c for c in clump.children if c.type == 0x14][-1]
        buf = rw.insert_section(buf, last_atom, bytes(new_atoms))
        b = bytearray(buf)
        clump = rw.parse_rw(buf)[0]
        st = clump.children[0]
        struct.pack_into('<i', b, st.data_start, struct.unpack_from('<i', b, st.data_start)[0] + len(atoms))
        out = bytes(b)
    return out


def texture_part(raw: bytes, tex_name: str) -> bytes: #vers 1
    """Part DFF with every untextured material given texture tex_name (parts ship without)."""
    from apps.methods import rw_chunks as rw
    out = raw
    while True:
        mat = next((x for r in rw.parse_rw(out) for x in r.walk()
                    if x.type == 0x07 and x.children and x.children[0].type == 0x01
                    and not any(c.type == 0x06 for c in x.children)), None)
        if mat is None:
            return out
        st = mat.children[0]
        stamp = st.stamp
        tex = rw.make_section(0x06, stamp, rw.make_section(0x01, stamp, bytes([2, 0x11, 1, 0]))
                              + rw.make_section(0x02, stamp, rw.string_payload(tex_name))
                              + rw.make_section(0x02, stamp, rw.string_payload(''))
                              + rw.make_section(0x03, stamp, b''))
        b = bytearray(out)
        struct.pack_into('<i', b, st.data_start + 12, 1)            # textured
        out = rw.insert_section(bytes(b), st, tex)


def merge_txds(txds: List[bytes]) -> bytes: #vers 1
    """One TXD holding every texture of txds (first TXD's version and device)."""
    from apps.methods import rw_chunks as rw
    natives, seen, head = [], set(), None
    for t in txds:
        if not t:
            continue
        root = rw.parse_rw(t)[0]
        if root.type != 0x16:
            continue
        if head is None:
            head = (root.stamp, struct.unpack_from('<H', t, root.children[0].data_start + 2)[0])
        for n in root.children:
            if n.type == 0x15:
                st = n.children[0]
                name = bytes(t[st.data_start + 8:st.data_start + 40]).split(b'\0', 1)[0].lower()
                if name not in seen:
                    seen.add(name)
                    natives.append(bytes(t[n.offset:n.end]))
    if head is None:
        raise ValueError("No textures")
    stamp, dev = head
    inner = (struct.pack('<III', 1, 4, stamp) + struct.pack('<HH', len(natives), dev)
             + b''.join(natives) + struct.pack('<III', 3, 0, stamp))
    return struct.pack('<III', 0x16, len(inner), stamp) + inner
