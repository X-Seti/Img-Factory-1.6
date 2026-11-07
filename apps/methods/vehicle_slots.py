#this belongs in apps/methods/vehicle_slots.py - Version: 1
# X-Seti - November 06 2026 - IMG Factory 1.6 - Vehicle slots across the game data files

"""
Vehicle slots for GTA III / VC / SA: one slot = an IDE 'cars' line plus its
handling.cfg line(s), carcols.dat and carmods.dat (SA) entries and the model /
TXD in an IMG. Add (copy of a template slot), swap model, remove and check.
Text files keep their line endings; each file is backed up once (.bak).
"""

##Methods list -
# _backup
# _find
# _read_lines
# _write_lines
# detect_game
# free_ids
#
##class VehicleSlots: -
# __init__
# _clone_handling
# _clone_name_lines
# _handling_index
# _ide_paths
# _name_index
# add_slot
# check
# load
# model_imgs
# new_img
# register_img
# remove_slot
# save
# set_model

import os
import re
import shutil
import struct
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

ID_RANGES = {'iii': (90, 150), 'vc': (130, 236), 'sa': (400, 611)}     # standard game ranges
_SPECIAL = ('!', '$', '%', '^')            # handling.cfg: bike, flying, boat, anim lines (id 2nd)


def _find(folder: str, name: str) -> Optional[str]: #vers 1
    """Case-insensitive file lookup inside folder."""
    if not os.path.isdir(folder):
        return None
    low = name.lower()
    return next((os.path.join(folder, f) for f in os.listdir(folder) if f.lower() == low), None)


def _read_lines(path: str) -> List[str]: #vers 1
    """Lines with their own endings kept."""
    with open(path, 'r', encoding='latin-1', newline='') as f:
        return f.read().splitlines(keepends=True)


def _backup(path: str) -> None: #vers 1
    """Copy path to path.bak once (first change only)."""
    if os.path.isfile(path) and not os.path.exists(path + '.bak'):
        shutil.copy2(path, path + '.bak')


def _write_lines(path: str, lines: List[str]) -> None: #vers 1
    _backup(path)
    with open(path, 'w', encoding='latin-1', newline='') as f:
        f.write(''.join(lines))


def detect_game(root: str) -> Optional[str]: #vers 1
    """'sa', 'vc' or 'iii' from the data/*.dat present."""
    data = _find(root, 'data')
    if not data:
        return None
    if _find(data, 'gta.dat') and _find(data, 'vehicles.ide'):
        return 'sa'
    if _find(data, 'gta_vc.dat'):
        return 'vc'
    if _find(data, 'gta3.dat'):
        return 'iii'
    return None


@dataclass
class Slot:
    id: int
    model: str
    txd: str
    type: str
    handling: str
    gxt: str
    path: str                              # IDE file
    line: int                              # line index in it
    problems: List[str] = field(default_factory=list)


def free_ids(slots: List[Slot], lo: int, hi: int) -> List[int]: #vers 1
    """IDs in lo..hi no slot uses."""
    used = {s.id for s in slots}
    return [i for i in range(lo, hi + 1) if i not in used]


class VehicleSlots: #vers 1
    """All vehicle slots of one game folder."""

    def __init__(self, root: str, game: Optional[str] = None): #vers 1
        self.root = root
        self.game = game or detect_game(root)
        if self.game is None:
            raise ValueError("Not a GTA III / VC / SA folder (no data/gta*.dat)")
        self.data = _find(root, 'data')
        self.files: Dict[str, List[str]] = {}
        self.dirty: set = set()
        self.slots: List[Slot] = []
        self.load()

    def _ide_paths(self) -> List[str]: #vers 1
        """IDE files named in default.dat and gta*.dat."""
        out = []
        for dat in ('default.dat', 'gta.dat', 'gta_vc.dat', 'gta3.dat'):
            p = _find(self.data, dat)
            if not p:
                continue
            for ln in _read_lines(p):
                m = re.match(r'\s*IDE\s+(\S+)', ln, re.I)
                if m:
                    cur = self.root
                    for part in m.group(1).replace('\\', '/').split('/'):
                        cur = _find(cur, part) if cur else None
                    if cur and cur not in out:
                        out.append(cur)
        return out

    def load(self): #vers 1
        """Read IDE cars sections, handling.cfg, carcols.dat, carmods.dat."""
        self.files, self.slots, self.dirty = {}, [], set()
        for path in self._ide_paths():
            lines = _read_lines(path)
            sec = None
            for i, ln in enumerate(lines):
                s = ln.split('#')[0].strip()
                low = s.lower()
                if low in ('cars', 'objs', 'tobj', 'peds', 'path', '2dfx', 'weap', 'hier', 'anim', 'txdp'):
                    sec = low
                    continue
                if low == 'end':
                    sec = None
                    continue
                if sec != 'cars' or not s:
                    continue
                p = [x for x in re.split(r'[\s,]+', s) if x]     # game splits on commas and spaces
                if len(p) < 6 or not p[0].isdigit():
                    continue
                self.slots.append(Slot(int(p[0]), p[1], p[2], p[3], p[4], p[5], path, i))
            if any(s.path == path for s in self.slots):
                self.files[path] = lines
        for name in ('handling.cfg', 'carcols.dat', 'carmods.dat'):
            p = _find(self.data, name)
            if p:
                self.files[p] = _read_lines(p)
        self.handling_path = _find(self.data, 'handling.cfg')
        self.carcols_path = _find(self.data, 'carcols.dat')
        self.carmods_path = _find(self.data, 'carmods.dat') if self.game == 'sa' else None

    def _handling_index(self) -> Dict[str, List[int]]: #vers 1
        """Handling id (upper) -> line indexes in handling.cfg."""
        out = {}
        for i, ln in enumerate(self.files.get(self.handling_path, [])):
            t = ln.split()
            if not t or t[0].startswith(';') or t[0].startswith('#'):
                continue
            key = t[1] if t[0] in _SPECIAL and len(t) > 1 else t[0]
            out.setdefault(key.upper(), []).append(i)
        return out

    def _name_index(self, path: Optional[str], sections: Tuple[str, ...]) -> Dict[str, List[int]]: #vers 1
        """Model name (lower) -> line indexes inside the named sections of carcols / carmods."""
        out, sec = {}, None
        for i, ln in enumerate(self.files.get(path, []) if path else []):
            s = ln.split('#')[0].strip()
            low = s.lower()
            if low in ('col', 'car', 'car3', 'car4', 'mods', 'link', 'wheel'):
                sec = low
                continue
            if low == 'end':
                sec = None
                continue
            if sec in sections and s:
                out.setdefault(s.split(',')[0].strip().lower(), []).append(i)
        return out

    def model_imgs(self) -> Dict[str, str]: #vers 1
        """Entry name (lower) -> IMG path, over models/gta3.img, models/vehicles.img and gta.dat IMG lines."""
        from apps.methods.img_core_classes import IMGFile
        paths = []
        models = _find(self.root, 'models')
        for n in ('gta3.img', 'vehicles.img'):
            p = _find(models, n) if models else None
            if p:
                paths.append(p)
        for dat in ('gta.dat', 'gta_vc.dat', 'gta3.dat'):
            p = _find(self.data, dat)
            for ln in _read_lines(p) if p else []:
                m = re.match(r'\s*(?:IMG|CDIMAGE)\s+(\S+)', ln, re.I)
                if m:
                    cur = self.root
                    for part in m.group(1).replace('\\', '/').split('/'):
                        cur = _find(cur, part) if cur else None
                    if cur and cur not in paths:
                        paths.append(cur)
        out = {}
        for p in paths:
            a = IMGFile(p)
            if a.open():
                for e in a.entries:
                    out.setdefault(e.name.lower(), p)
        return out

    def check(self) -> List[Slot]: #vers 1
        """Fill slot.problems: duplicate id / name, model or TXD missing, no handling or colours."""
        imgs = self.model_imgs()
        hand = self._handling_index()
        cols = self._name_index(self.carcols_path, ('car', 'car3', 'car4'))
        ids, names = {}, {}
        for s in self.slots:
            ids.setdefault(s.id, []).append(s)
            names.setdefault(s.model.lower(), []).append(s)
        for s in self.slots:
            s.problems = []
            if len(ids[s.id]) > 1:
                s.problems.append("duplicate ID")
            if len(names[s.model.lower()]) > 1:
                s.problems.append("duplicate name")
            if s.model.lower() + '.dff' not in imgs:
                s.problems.append("no model")
            if s.txd.lower() + '.txd' not in imgs:
                s.problems.append("no TXD")
            if s.handling.upper() not in hand:
                s.problems.append("no handling")
            if s.model.lower() not in cols:
                s.problems.append("no colours")
        return self.slots

    def _clone_handling(self, src: str, new: str) -> None: #vers 1
        """Copy the handling line(s) of src under the id new."""
        path = self.handling_path
        if not path:
            return
        lines = self.files[path]
        hand = self._handling_index()
        if new.upper() in hand or src.upper() not in hand:
            return
        add = []
        for i in hand[src.upper()]:
            ln = lines[i]
            t = ln.split()
            key = t[1] if t[0] in _SPECIAL else t[0]
            add.append((i, re.sub(re.escape(key), new.upper(), ln, count=1)))
        for i, ln in sorted(add, reverse=True):
            lines.insert(i + 1, ln)
        self.dirty.add(path)

    def _clone_name_lines(self, path: Optional[str], sections, src: str, new: str) -> None: #vers 1
        """Copy src's lines in carcols / carmods under the name new."""
        if not path:
            return
        idx = self._name_index(path, sections)
        if new.lower() in idx or src.lower() not in idx:
            return
        lines = self.files[path]
        for i in sorted(idx[src.lower()], reverse=True):
            ln = lines[i]
            lines.insert(i + 1, re.sub(r'^\s*[^,\s]+', new.lower(), ln, count=1))
        self.dirty.add(path)

    def add_slot(self, template: Slot, new_id: int, name: str, txd: Optional[str] = None,
                 handling: Optional[str] = None) -> Slot: #vers 1
        """New slot copied from template: IDE line, handling, colours and mods."""
        if any(s.id == new_id for s in self.slots):
            raise ValueError(f"ID {new_id} is in use")
        if any(s.model.lower() == name.lower() for s in self.slots):
            raise ValueError(f"{name} is in use")
        txd = txd or name
        handling = (handling or name).upper()
        lines = self.files[template.path]
        src = lines[template.line]
        body, _sep, comment = src.partition('#')
        end = src[len(src.rstrip('\r\n')):]
        p = [x for x in re.split(r'[\s,]+', body.strip()) if x]
        p[0], p[1], p[2], p[4] = str(new_id), name, txd, handling
        new_line = ', '.join(p) + (end or '\n')
        pos = max(s.line for s in self.slots if s.path == template.path) + 1
        lines.insert(pos, new_line)
        for s in self.slots:
            if s.path == template.path and s.line >= pos:
                s.line += 1
        self.dirty.add(template.path)
        self._clone_handling(template.handling, handling)
        self._clone_name_lines(self.carcols_path, ('car', 'car3', 'car4'), template.model, name)
        self._clone_name_lines(self.carmods_path, ('mods',), template.model, name)
        slot = Slot(new_id, name, txd, template.type, handling, p[5], template.path, pos)
        self.slots.append(slot)
        return slot

    def remove_slot(self, slot: Slot) -> None: #vers 1
        """Drop the IDE line, colours, mods and handling (when no other slot uses it)."""
        lines = self.files[slot.path]
        del lines[slot.line]
        for s in self.slots:
            if s.path == slot.path and s.line > slot.line:
                s.line -= 1
        self.slots.remove(slot)
        self.dirty.add(slot.path)
        for path, secs in ((self.carcols_path, ('car', 'car3', 'car4')), (self.carmods_path, ('mods',))):
            idx = self._name_index(path, secs).get(slot.model.lower(), [])
            for i in sorted(idx, reverse=True):
                del self.files[path][i]
            if idx:
                self.dirty.add(path)
        if not any(s.handling.upper() == slot.handling.upper() for s in self.slots):
            idx = self._handling_index().get(slot.handling.upper(), [])
            for i in sorted(idx, reverse=True):
                del self.files[self.handling_path][i]
            if idx:
                self.dirty.add(self.handling_path)

    def new_img(self, img_path: str, name: str, data: bytes) -> None: #vers 1
        """Create an IMG holding one entry: SA one VER2 file, III / VC a .dir / .img pair."""
        if os.path.exists(img_path):
            raise ValueError(f"{os.path.basename(img_path)} exists")
        body = data + bytes(-len(data) % 2048)
        if self.game == 'sa':
            head = b'VER2' + struct.pack('<I', 1) + struct.pack('<IHH24s', 1, len(body) // 2048, 0,
                                                                  name.encode('ascii')[:23])
            with open(img_path, 'wb') as f:
                f.write(head + bytes(2048 - len(head)) + body)
        else:
            with open(os.path.splitext(img_path)[0] + '.dir', 'wb') as f:
                f.write(struct.pack('<II24s', 0, len(body) // 2048, name.encode('ascii')[:23]))
            with open(img_path, 'wb') as f:
                f.write(body)

    def set_model(self, img_path: str, slot: Slot, dff: Optional[bytes], txd: Optional[bytes]) -> None: #vers 1
        """Write the slot's DFF / TXD into an IMG (made when missing; originals backed up beside it)."""
        from apps.methods.img_core_classes import IMGFile
        made = not os.path.exists(img_path)
        if made:
            first = (slot.model + '.dff', dff) if dff else (slot.txd + '.txd', txd)
            self.new_img(img_path, *first)
        arc = IMGFile(img_path)
        if not arc.open():
            raise ValueError(f"{os.path.basename(img_path)}: not readable")
        bak = os.path.join(os.path.dirname(img_path), 'vehicle_backup')
        for name, data in ((slot.model + '.dff', dff), (slot.txd + '.txd', txd)):
            if not data:
                continue
            e = next((x for x in arc.entries if x.name.lower() == name.lower()), None)
            if e is not None and not made:
                os.makedirs(bak, exist_ok=True)
                keep = os.path.join(bak, e.name)
                if not os.path.exists(keep):
                    with open(keep, 'wb') as f:
                        f.write(arc.read_entry_data(e))
            if not arc.add_entry(e.name if e else name, data):
                raise ValueError(f"{name}: not written to {os.path.basename(img_path)}")

    def register_img(self, rel: str = 'MODELS\\VEHICLES.IMG') -> bool: #vers 1
        """SA: add an IMG line for rel to gta.dat (after the last IMG line); False if present / not SA."""
        p = _find(self.data, 'gta.dat') if self.game == 'sa' else None
        if not p:
            return False
        lines = self.files.get(p) or _read_lines(p)
        if any(re.match(r'\s*IMG\s+' + re.escape(rel) + r'\s*$', ln, re.I) for ln in lines):
            return False
        end = next((ln[len(ln.rstrip('\r\n')):] for ln in lines if ln.endswith('\n')), '\r\n')
        last = max((i for i, ln in enumerate(lines) if re.match(r'\s*IMG\s', ln, re.I)), default=-1)
        lines.insert(last + 1, f"IMG {rel}{end}")
        self.files[p] = lines
        self.dirty.add(p)
        return True

    def save(self) -> List[str]: #vers 1
        """Write every changed text file (backed up first); paths written."""
        out = []
        for path in sorted(self.dirty):
            _write_lines(path, self.files[path])
            out.append(path)
        self.dirty.clear()
        return out
